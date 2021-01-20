memo_sqlite_con <- memoise::memoise( function(db, flags=RSQLite::SQLITE_RO, ...) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db, flags=flags, ...)
  con
})

memo_tbl <- memoise::memoise(function(db, table) {
  check_package_available('RSQLite')
  check_package_available('dbplyr')
  db=path.expand(db)
  con <- try(memo_sqlite_con(db), silent = TRUE)
  if(inherits(con, 'try-error'))
    return(NULL)
  res <- dplyr::tbl(con, table)
  res
})

# little utility function for GJ's convenience and because google filestream
# occasionally wrongly thinks a file has been modified ...
local_or_google <- function(f, local = NULL) {
  if(is.null(local)){local = path.expand("~/projects/JanFunke/")}
  g="/Volumes/GoogleDrive/Shared drives/hemibrain/fafbsynapses/"
  if(file.exists(file.path(local,f))) file.path(local,f) else file.path(g,f)
}

synlinks_tbl <- function(local = NULL) {
  p=local_or_google("20191211_fafbv14_buhmann2019_li20190805_nt20201223.db", local = local)
  memo_tbl(p, "synlinks")
}

flywireids_tbl <- function(local = NULL) {
  p=local_or_google("flywire_synapses.db", local = local)
  memo_tbl(p, "flywireids")
}

ntpredictions_tbl <- function(local = NULL) {
  p=local_or_google("20191211_fafbv14_buhmann2019_li20190805_nt20201223.db", local = local)
  memo_tbl(p, "predictions2")
}


#' Find the input/output partners for a flywire neuron
#'
#' @description \code{flywire_partners} is a low level function returning one
#'   row for every synapse.
#'
#' @details FIXME behaviour when there are no partners is not well-defined.
#'
#'   Note that there are many duplicated connections in this raw output, false
#'   autapses (i.e. the same neuron connected to itself) and other false
#'   positives. See Buhmann et al for details and ideas about cleaning up the
#'   results.
#'
#' @param rootid Character vector specifying one or more flywire rootids. As a
#'   convenience for \code{flywire_partner_summary} this argument is passed to
#'   \code{\link{ngl_segments}} allowing you to pass in
#' @param partners Whether to fetch input or output synapses
#' @param details Whether to include additional details such as X Y Z location
#'   (default \code{FALSE})
#' @param roots Whether to fetch the flywire rootids of the partner neurons
#'   (default \code{TRUE})
#' @param cloudvolume.url The segmentation source URL for cloudvolume. Normally
#'   you can ignore this and rely on the default segmentation chosen by
#'   \code{\link{choose_segmentation}}
#' @param method Whether to use a local SQLite database or remote spine service
#'   for synapse data. The default \code{auto} uses a local database when
#'   available (45GB but faster).
#' @param local path to SQLite synapse data. Evaluated by \code{fafbseg:::local_or_google}. Work in progress. Default is to download
#' this data and place it in \code{~/projects/JanFunke}.
#' @param ... Additional arguments passed to \code{\link{pbsapply}}
#' @export
#' @family automatic-synapses
#' @examples
#' \donttest{
#' pp=flywire_partners("720575940621039145")
#' head(pp)
#' }
flywire_partners <- function(rootid, partners=c("outputs", "inputs", "both"),
                             details=FALSE, roots=TRUE, cloudvolume.url=NULL, method=c("auto", "spine", "sqlite"), Verbose=TRUE, local = NULL,...) {
  partners=match.arg(partners)
  method=match.arg(method)
  if(method=="auto") {
    flywireids=flywireids_tbl(local=local)
    method <- if(is.null(flywireids)) "spine" else "sqlite"
  }

  if(isTRUE(details)) {
    synlinks=synlinks_tbl(local=local)
    if(is.null(synlinks))
      stop("I cannot find the Buhmann sqlite database required when details=TRUE!")
  }

  if(length(rootid)>1) {
    res=pbapply::pbsapply(rootid, flywire_partners, partners = partners, ...,
                          simplify = F, details=details, roots=roots, cloudvolume.url=cloudvolume.url, method=method, local = local)
    df=dplyr::bind_rows(res, .id = 'query')
    return(df)
  }
  svids = get_flywire_svids(rootid=rootid, cloudvolume.url = cloudvolume.url, Verbose = Verbose)
  if(Verbose)
    message("Finding synapses for supervoxels")
  if(method=='spine') {
    resp=httr::POST("https://spine.janelia.org/app/synapse-service/segmentation/flywire_supervoxels/csv", body=list(query_ids=svids), encode = 'json')
    httr::stop_for_status(resp)
    # fread looks after int64 values, but ask for regular data.frame
    if(Verbose)
      message("Reading synapse data")
    resdf <- data.table::fread(text = httr::content(resp, as='text', encoding = 'UTF-8'), data.table=FALSE)
    colnames(resdf) <- c("offset", 'pre_svid', "post_svid", "scores", "cleft_scores")
    # we can get the same row appearing twice for autapses
    resdf <- filter(resdf, !duplicated(.data$offset))
    if (partners == "outputs"){
      resdf <-  filter(resdf, .data$pre_svid %in% svids)
    }else if (partners == "inputs"){
      resdf <-  filter(resdf, .data$post_svid %in% svids)
    }
  } else {
    if(partners == "both"){
      res = dplyr::filter(flywireids,
                          .data$pre_svid%in%svids|.data$post_svid%in%svids)
    }else{
      df <- if (partners == "outputs"){
        tibble::tibble(pre_svid = svids)
      }else if (partners == "inputs"){
        tibble::tibble(post_svid = svids)
      }
      res = dplyr::inner_join(flywireids, df,
                              by = ifelse(partners == "outputs", "pre_svid", "post_svid"),
                              copy = TRUE,
                              auto_index = TRUE)
    }
    # could try to count rows in result but not sure if that runs it twice
    resdf=as.data.frame(res) # this is the very time consuming step
  }

  if(isTRUE(details)) {
    if(Verbose)
      message("Finding additional details for synapses")
    resdf=as.data.frame(dplyr::inner_join(synlinks, resdf, by="offset", copy=TRUE))
  }
  # sort by offset (TODO don't do this if already sorted)
  resdf=resdf[order(resdf$offset),,drop=FALSE]
  rownames(resdf) <- NULL
  # reorder columns so that they are always in same order
  preferredcolorder=c("offset", "pre_x", "pre_y", "pre_z", "post_x", "post_y", "post_z",
                      "scores", "cleft_scores", "segmentid_pre", "segmentid_post",
                      "pre_svid", "post_svid", "post_id", "pre_id")
  colstomatch <- union(preferredcolorder, colnames(resdf))
  colstomatch=intersect(colstomatch, colnames(resdf))
  resdf=resdf[match(colstomatch, colnames(resdf))]

  if(nrow(resdf)>0 && isTRUE(roots)) {
    message("Fetching root ids")
    if(partners=="outputs"){
      resdf$post_id=bit64::as.integer64(fafbseg::flywire_rootid(resdf$post_svid, cloudvolume.url=cloudvolume.url))
      resdf$pre_id=bit64::as.integer64(rootid)
    } else if (partners=="inputs") {
      resdf$pre_id=bit64::as.integer64(fafbseg::flywire_rootid(resdf$pre_svid, cloudvolume.url=cloudvolume.url))
      resdf$post_id=bit64::as.integer64(rootid)
    }else{
      resdf$pre_id=bit64::as.integer64(fafbseg::flywire_rootid(resdf$pre_svid, cloudvolume.url=cloudvolume.url))
      resdf$post_id=bit64::as.integer64(fafbseg::flywire_rootid(resdf$post_svid, cloudvolume.url=cloudvolume.url))
      resdf$prepost = ifelse(as.character(resdf$pre_id)%in%rootid,0,1)
    }
  }
  resdf
}

# hidden
get_flywire_svids<-function(rootid,
                            cloudvolume.url = NULL,
                            Verbose = TRUE){
  if(Verbose){
    message("Fetching supervoxel ids for id: ", rootid)
  }
  svids = fafbseg::flywire_leaves(rootid, cloudvolume.url = cloudvolume.url)
  if (!bit64::is.integer64(svids)){
    svids = bit64::as.integer64(as.character(svids))
  }
  bad_svids = which(svids == bit64::as.integer64(0L))
  if (length(bad_svids)) {
    svids = svids[-bad_svids]
    warning("Dropping ", length(bad_svids), " supervoxels with id 0!")
  }
  svids
}

#' @description \code{flywire_partner_summary} summarises the connectivity of
#'   one or more flywire neurons.
#' @rdname flywire_partners
#' @param threshold For \code{flywire_partner_summary} only return partners with
#'   greater than this number of connections to the query neuron(s) (default of
#'   0 returns all connections)
#' @param remove_autapses For \code{flywire_partner_summary} whether to remove
#'   autapses (defaults to TRUE)
#' @param Verbose Whether to print status messages
#' @export
#' @importFrom dplyr summarise group_by n arrange desc filter mutate
#' @family automatic-synapses
#'
#' @examples
#' \donttest{
#' flywire_partner_summary("720575940621039145", partners='out')
#' flywire_partner_summary("720575940621039145", partners='in')
#' flywire_partner_summary("720575940621039145")
#'
#' # summary for neuron at a XYZ location (in this case in raw coordinates)
#' flywire_partner_summary(flywire_xyz2id(cbind(155682, 58180, 3215),
#'   rawcoords = TRUE))
#'
#' \dontrun{
#' # Use Ctrl+Shift+J to share a flywire scene and then do this to get partner
#' # summary for that URL
#' flywire_partner_summary(clipr::read_clip())
#'
#' }
#' }
flywire_partner_summary <- function(rootid, partners=c("outputs", "inputs"),
                                    threshold=0, remove_autapses=TRUE, Verbose=NA, local = NULL, ...) {
  check_package_available('tidyselect')
  partners=match.arg(partners)
  rootid=ngl_segments(rootid)
  if (length(rootid) > 1) {
    if(is.na(Verbose)) Verbose=FALSE
    res = pbapply::pbsapply(
      rootid,
      flywire_partner_summary,
      partners = partners,
      simplify = F,
      threshold = threshold,
      remove_autapses = remove_autapses,
      Verbose=Verbose, local = local,
      ...
    )
    df = dplyr::bind_rows(res, .id = 'query')
    return(df)
  }

  if(is.na(Verbose)) Verbose=TRUE

  partnerdf=flywire_partners(rootid, partners=partners, local = local)
  # partnerdf=flywire_partners_memo(rootid, partners=partners)
  if(remove_autapses) {
    partnerdf=partnerdf[partnerdf$post_id!=partnerdf$pre_id,,drop=FALSE]
  }
  groupingcol=if(partners=='outputs') "post_id" else "pre_id"
  querycol=if(partners!='outputs') "post_id" else "pre_id"

  res <- partnerdf %>%
    group_by(.data[[groupingcol]]) %>%
    summarise(weight=n(), n=length(unique(.data[[querycol]]))) %>%
    arrange(desc(.data$weight)) %>%
    filter(.data$weight>threshold)

  # convert 64 bit ints to char (safer but bigger)
  is64=sapply(res, bit64::is.integer64)
  if(any(is64)) {
    for(i in which(is64)) {
      res[[i]]=as.character(res[[i]])
    }
  }
  res
}


## Functions for neurotransmitter prediction


#' Return raw neurotransmitter prediction results for output of flywire neuron
#'
#' @param x A single root id as a string OR a \code{data.frame} of output
#'   (downstream) partners returned by \code{flywire_partners}.
#' @inheritParams flywire_partners
#' @return A \code{data.frame} of neurotransmitter predictions
#' @importFrom dplyr select arrange
#' @export
#' @family automatic-synapses
#'
#' @examples
#' \donttest{
#' # an olfactory projection neuron
#' flywire_ntpred("720575940615237849")
#' # alternatively
#' \dontrun{
#' flywire_ntpred(flywire_xyz2id(cbind(116923, 61378, 1474), rawcoords = T))
#' }
#' }
flywire_ntpred <- function(x, local=NULL, cloudvolume.url = NULL) {
  check_package_available('matrixStats')

  if(is.character(x)) {
    rootid=x
    x <- flywire_partners(x, partners = 'outputs', roots = FALSE, Verbose=FALSE, cloudvolume.url = cloudvolume.url, local = local)
  } else {
    rootid=NULL
  }
  poss.nts=c("gaba", "acetylcholine", "glutamate", "octopamine", "serotonin",
             "dopamine")
  extracols=c("scores", "cleft_scores","pre_x", "pre_y", "pre_z")
  stopifnot(is.data.frame(x))
  if(all(poss.nts %in% colnames(x))) {
    # looks like we already got the NT info
  } else {
    # NB the sqlite table has to come first in the join
    ntpredictions=ntpredictions_tbl(local=local)
    if(is.null(ntpredictions))
      stop("I cannot find the neurotransmitter predictions sqlite database!")
    x=as.data.frame(arrange(dplyr::inner_join(ntpredictions, x, copy = T, by=c("id"="offset"))), .data$offset)
    colnames(x)[1]='offset'
  }


  if(!all(extracols %in% colnames(x))) {
    missing_cols <- setdiff(extracols, colnames(x))
    synlinks=synlinks_tbl(local=local)
    if(is.null(synlinks))
      stop("I cannot find the buhmann sqlite database required to fetch synapse details!")
    x = as.data.frame(
      arrange(
        dplyr::inner_join(
          select(synlinks, union("offset", missing_cols)),
          x, copy = T, by = "offset"),
        .data$offset
      )
    )

  }
  dmx=data.matrix(x[poss.nts])
  x[,'top.p']=matrixStats::rowMaxs(dmx)
  top.col=apply(dmx, 1, which.max)
  x[,'top.nt']=poss.nts[top.col]
  class(x)=union("ntprediction", class(x))
  attr(x,'rootid')=rootid
  x
}

#' @export
#' @family automatic-synapses
#' @param ... additional arguments passed to \code{\link{print}}
#' @rdname flywire_ntpred
#' @description the \code{print.ntprediction} method provides a quick summary of
#'   the neurotransmitter prediction for all output synapses.
print.ntprediction <- function(x, ...) {
  tx=table(x$top.nt)
  cat("neuron", attr(x, 'rootid'), "with", sum(tx), "output synapses.\n")
  withr::with_options(list(digits=3), {
    print(sort(tx, decreasing = TRUE)/sum(tx)*100, ...)
  })
}

#' Plot neurotransmitter prediction summaries or synapses in 3D
#'
#' @description \code{flywire_ntplot} plots a ggplot2 histogram of predicted
#'   neurotransmitter vs prediction probability.
#'
#' @param x A flywire rootid or a data.frame of neurotransmitter predictions
#'   returned by \code{\link{flywire_ntpred}}
#' @param nts A character vector of neurotransmitters to include in the plot
#'   (default all 6)
#' @param cleft.threshold A threshold for the cleft score calculated by Buhmann
#'   et al 2019 (default 0, we have used 30-100 to increase specificity)
#' @inheritParams flywire_partners
#' @export
#' @family automatic-synapses
#' @examples
#' \donttest{
#' # a cholinergic olfactory projection neuron
#' ntp=flywire_ntpred("720575940615237849")
#' flywire_ntplot(ntp)
#' flywire_ntplot(ntp, nts=c("gaba", "acetylcholine", "glutamate"))
#' flywire_ntplot(ntp, nts=c("gaba", "acetylcholine", "glutamate"), cleft.threshold=100)
#' }
flywire_ntplot <- function(x, nts=c("gaba", "acetylcholine", "glutamate",
                                    "octopamine", "serotonin", "dopamine"),
                           cleft.threshold=0, local = NULL, cloudvolume.url = NULL) {
  check_package_available('ggplot2')
  nts=match.arg(nts, several.ok = T)
  x=flywire_ntpred(x, local=local, cloudvolume.url = cloudvolume.url)
  x=dplyr::filter(x, .data$cleft_scores>=cleft.threshold &
                    .data$top.nt %in% nts)
  ntcols = c(
    gaba = "#E6A749",
    acetylcholine = "#4B506B",
    glutamate = "#70B657",
    octopamine = "#7A4F98",
    serotonin = "#93A3CF",
    dopamine = "#CF6F6C"
  )[nts]

  ggplot2::qplot(x$top.p, fill=x$top.nt, xlab = 'probability') +
    ggplot2::scale_fill_manual('nt', values=ntcols, breaks=names(ntcols))
}

#' @description \code{flywire_ntplot3d} makes a 3D plot of synapse location
#'
#' @param plot Whether to plot points or spheres ("points" with \code{size=5}
#'   works quite well)
#' @param ... additional arguments passed to \code{\link{spheres3d}} or
#'   \code{\link{points3d}}
#' @inheritParams flywire_partners
#' @export
#' @importFrom rgl spheres3d points3d
#' @family automatic-synapses
#' @rdname flywire_ntplot
#' @examples
#' \dontrun{
#' flywire_ntplot3d(ntp, nts=c("gaba", "acetylcholine",
#'   "glutamate"), plot='points', cleft.threshold=30, size=5)
#' }
flywire_ntplot3d <- function(x, nts=c("gaba", "acetylcholine", "glutamate",
                                      "octopamine", "serotonin", "dopamine"),
                             plot=c("points", "spheres"), cleft.threshold=0,
                             local = NULL, cloudvolume.url = NULL,
                             ...) {
  plot=match.arg(plot)
  nts=match.arg(nts, several.ok = TRUE)
  x=flywire_ntpred(x, local = local, cloudvolume.url = cloudvolume.url)
  x=filter(x, .data$cleft_scores>=cleft.threshold &
             .data$top.nt %in% nts)
  pts=xyzmatrix(x[,c("pre_x", "pre_y", "pre_z")])
  # pts=xyzmatrix(x[,c("post_x", "post_y", "post_z")])
  pts.fw=fafb2flywire(pts)

  cols = c(
    gaba = "#E6A749",
    acetylcholine = "#4B506B",
    glutamate = "#70B657",
    octopamine = "#7A4F98",
    serotonin = "#93A3CF",
    dopamine = "#CF6F6C"
  )[nts]
  if(plot=="spheres")
    spheres3d(pts.fw, col=cols[x$top.nt], radius = 200, ...)
  else
    points3d(pts.fw, col=cols[x$top.nt], ...)
}

#' Attach synapses to flywire neuron skeletons
#'
#' @description Attach the appropriate input and output synapses to each flywire neuron skeleton in a neuronlist.
#' @param x a \code{nat::neuronlist} for flywire neurons in the FlyWire or FAFB14 brainspace. These skeletons can be created
#' using \code{\link{skeletor}}, or retreived using \code{hemibrainr::flywire_neurons}.
#' @param connectors a \code{data.frame} of FAFB synapses, with XYZ coordinates, to attach to \code{x}. If \code{NULL} (default) synapses are fetched,
#' as in \code{\link{flywire_partners}}.
#' @param remove_autapses  whether to remove autapses (defaults to \code{TRUE}).
#' @param transmitters if \code{TRUE} also attempt to retreive neurotransmitter predictions from Eckstein et al. 2020, for the flywire neuron in question.
#' @param ... methods sent to \code{nat::nlapply}.
#' @inheritParams flywire_partners
#'
#' @return A \code{nat::neuronlist} object, where each neuron in the neuronlist has a \code{data.frame}
#' of synapses at neuron$connectors.
#'
#' @export
#' @family automatic-synapses
#'
#' @examples
#' \donttest{
#' \dontrun{
#' choose_segmentation("flywire")
#' nx=xform_brain(elmr::dense_core_neurons, ref="FlyWire", sample="FAFB14")
#' xyz =xyzmatrix(nx)
#' ids = unique(flywire_xyz2id(xyz[sample(1:nrow(xyz),100),]))
#' neurons = skeletor(ids, brain = elmr::FAFB14.surf)
#' neurons.syns = flywire_neurons_add_synapses(neurons, transmitters = TRUE)
#' neurons.syns[,]
#'
#' # Plot in 3D
#' library(catmaid)
#' nopen3d()
#' plot3d(neurons.syns, WithConnectors = TRUE)
#'
#' # Axon-dendrite split
#' library(hemibrainr)
#' neurons.flow = flow_centrality(neurons.syns,
#'   polypre = TRUE,
#'   mode = "centrifugal")
#' clear3d()
#' plot3d_split(neurons.flow, WithConnectors = TRUE,
#' transmitter = TRUE,
#' radius = 1000, soma = 4000)
#' }
#' }
flywire_neurons_add_synapses <- function(x,
                                         connectors = NULL,
                                         cloudvolume.url=NULL, # fafbseg.cloudvolume.url="graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31"
                                         method=c("auto", "spine", "sqlite"),
                                         remove_autapses = TRUE,
                                         Verbose=TRUE,
                                         transmitters=TRUE,
                                         local = NULL, # "/Volumes/nnautilus/projects/JanFunke"
                                         ...) UseMethod("flywire_neurons_add_synapses")
flywire_neurons_add_synapses.neuron <- function(x,
                                                connectors = NULL,
                                                cloudvolume.url=NULL,
                                                method=c("auto", "spine", "sqlite"),
                                                remove_autapses = TRUE,
                                                Verbose=TRUE,
                                                transmitters=TRUE,
                                                local = NULL,
                                                ...){
  method = match.arg(method)
  rootid = x$flywire.id
  if(is.null(connectors)){
    synapses = flywire_partners(rootid = rootid,
                                partners = "both",
                                roots=TRUE,
                                details=TRUE,
                                cloudvolume.url = cloudvolume.url,
                                method=method,
                                Verbose=Verbose,
                                local = local,
                                ...)
    if(isTRUE(!is.null(synapses) && !nrow(synapses))){
      class(x) = union(c("flywireneuron", "catmaidneuron"), class(x))
      return(x)
    }
    if(remove_autapses) {
      synapses=synapses[synapses$post_id!=synapses$pre_id,,drop=FALSE]
    }
    # Add synapses
    synapses %>% dplyr::group_by(.data$prepost) %>%
      dplyr::filter(.data$cleft_scores>0) %>%
      dplyr::mutate(x = ifelse(rootid==.data$pre_svid, .data$pre_x, .data$post_x)) %>%
      dplyr::mutate(y = ifelse(rootid==.data$pre_svid, .data$pre_y, .data$post_y)) %>%
      dplyr:: mutate(z = ifelse(rootid==.data$pre_svid, .data$pre_z, .data$post_z)) %>%
      dplyr:: arrange(desc(.data$scores),desc(.data$cleft_scores)) %>%
      dplyr::select(.data$offset, .data$prepost, .data$x, .data$y, .data$z,
               .data$scores, .data$cleft_scores,
               .data$segmentid_pre, .data$segmentid_post, .data$pre_svid, .data$post_svid,
               .data$pre_id, .data$post_id) %>%
      as.data.frame() ->
      synapses.xyz
  }else{
    synapses=connectors[connectors$post_id%in%rootid|connectors$pre_id%in%rootid,,drop=FALSE]
  }
  # If transmiters
  if(transmitters){
    if(Verbose){
      message("Adding transmitter prediction information (Eckstein et al. 2020)")
    }
    npred = flywire_ntpred(x=synapses.xyz, local = local, cloudvolume.url = cloudvolume.url)
    pref.order = c("offset", "x", "y", "z", "scores", "cleft_scores", "top.p", "top.nt", "gaba", "acetylcholine",
                   "glutamate", "octopamine", "serotonin", "dopamine", "prepost",
                   "segmentid_pre", "segmentid_post",
                   "pre_svid", "post_svid", "pre_id", "post_id")
    pref.order = intersect(pref.order,colnames(npred))
    if(nrow(npred)){
      synapses.xyz = npred[,pref.order]
    }
  }else{
    synapses.xyz$top.nt = "unknown"
  }
  near = nabor::knn(query= nat::xyzmatrix(synapses.xyz),data=nat::xyzmatrix(x$d),k=1)
  synapses.xyz$treenode_id = x$d[near$nn.idx,"PointNo"]
  # synapses.xyz = synapses.xyz[near$nn.dists<10000,] # remove erroneously associated synapses
  synapses.xyz$connector_id = synapses.xyz$segmentid_pre
  x$connectors = as.data.frame(synapses.xyz, stringsAsFactors = FALSE)
  x$transmitter.predictions = table(subset(synapses.xyz, synapses.xyz$prepost == 0)$top.nt)
  class(x) = union(c("flywireneuron", "catmaidneuron"), class(x))
  x
}
flywire_neurons_add_synapses.neuronlist <- function(x,
                                                    connectors=NULL,
                                                    cloudvolume.url=NULL,
                                                    method=c("auto", "spine", "sqlite"),
                                                    remove_autapses=TRUE,
                                                    Verbose=TRUE,
                                                    transmitters=FALSE,
                                                    local=NULL,
                                                    ...){
  method = match.arg(method)
  rootids = tryCatch(x[,"flywire.id"], error = function(e) NULL)
  if(is.null(rootids)){
    rootids = names(x)
  }
  if(is.null(rootids)){
    rootids = sapply(x, function(y) y$flywire.id)
  }
  x = add_field_seq(x,rootids,field="flywire.id")
  neurons.syn = nat::nlapply(x,
                         flywire_neurons_add_synapses.neuron,
                         connectors=connectors,
                         cloudvolume.url = cloudvolume.url,
                         method = method,
                         remove_autapses=remove_autapses,
                         Verbose = Verbose,
                         transmitters = transmitters,
                         local = local,
                         ...)
  nmeta = lapply(neurons.syn, extract_ntpredictions.neuron)
  nmeta = do.call(rbind, nmeta)
  meta2 = cbind(neurons.syn[,], nmeta[,setdiff(colnames(nmeta),colnames(neurons.syn[,]))])
  rownames(meta2) = meta2$flywire.id
  neurons.syn[,] = meta2
  neurons.syn
}
# neurons.syns = flywire_neurons_add_synapses(neurons, transmitters = TRUE, cloudvolume.url = "graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31", local =  "/Volumes/nnautilus/projects/JanFunke")

# hidden
extract_ntpredictions.neuron <- function(x,
                                  poss.nts=c("gaba", "acetylcholine", "glutamate", "octopamine", "serotonin","dopamine")
                                  ){
  synapses = x$connectors
  synapses.xyz = tryCatch(subset(synapses, synapses$prepost == 0), error = function(e) NULL)
  synapses.xyz = tryCatch(synapses.xyz[,colnames(synapses.xyz)%in%poss.nts], error = function(e) NULL)
  flywire.id = ifelse(is.null(x$flywire.id),NA,x$flywire.id)
  if(is.null(synapses.xyz)||!nrow(synapses.xyz)){
    data.frame(flywire.id = flywire.id, top.nt = "unknown", top.p = "unknown", pre = 0, post = 0, stringsAsFactors = FALSE)
  }else{
    if(ncol(synapses.xyz)){
      tops = colSums(synapses.xyz)/nrow(synapses.xyz)
      top.p = max(tops)
      top.nt = names(tops)[which.max(max(tops))][1]
    }else{
      top.p = "unknown"
      top.nt = "unknown"
    }
    pre = nullToZero(sum(synapses$prepost==0))
    post = nullToZero(sum(synapses$prepost==1))
    data.frame(flywire.id = flywire.id, top.nt = top.nt, top.p = top.p, pre = pre, post = post, stringsAsFactors = FALSE)
  }
}

