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
  if(is.null(local))
    local = getOption('fafbseg.sqlitepath')
  local=path.expand(local)
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
#' @param partners Whether to fetch input or output synapses or both.
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
#' @param local path to SQLite synapse data. Evaluated by
#'   \code{fafbseg:::local_or_google}. Work in progress. Default is to download
#'   this data and place it in \code{~/projects/JanFunke}.
#' @param ... Additional arguments passed to \code{\link{pbsapply}}
#' @export
#' @importFrom bit64 as.integer64 is.integer64
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
  rootid=ngl_segments(rootid, as_character = TRUE, must_work = TRUE, unique = TRUE)


  if(method!="spine") {
    flywireids=flywireids_tbl(local=local)
    sqliteok=!is.null(flywireids)
    if(details) {
      synlinks=synlinks_tbl(local=local)
      sqliteok=sqliteok & !is.null(synlinks)
    }
    if(method=='auto')
      method <- if(sqliteok) "sqlite" else "spine"
    else {
      if(is.null(flywireids))
        stop("method=sqlite but could not connect to flywireids database!")
      if(is.null(flywireids) && details)
        stop("I cannot find the Buhmann sqlite database required when details=TRUE!")
    }
  }

  if(length(rootid)>1) {
    res=pbapply::pbsapply(rootid, flywire_partners, partners = partners, ...,
                          simplify = F, details=details, roots=roots, cloudvolume.url=cloudvolume.url, method=method, Verbose=Verbose, local = local)
    df=dplyr::bind_rows(res, .id = 'query')
    return(df)
  }

  if(Verbose)
    message("Fetching supervoxel ids for id: ", rootid)
  svids=flywire_leaves(rootid, cloudvolume.url=cloudvolume.url,
                                integer64 = TRUE)

  if(!is.integer64(svids))
    svids=as.integer64(as.character(svids))
  # we don't want to include 0 i.e. bad segmentation by accident as this
  # could fetch a huge number of rows from spine. Ofc this shouldn't happen ...
  bad_svids=which(is.na(svids) | svids<1L)
  if(length(bad_svids)) {
    svids=svids[-bad_svids]
    warning("Dropping ", length(bad_svids), " supervoxels with id 0!")
  }

  if(Verbose)
    message("Finding synapses for supervoxels")
  if(method=='spine') {
    resdf=spine_svids2synapses(svids, Verbose, partners, details = details)
  } else {
    if(partners %in% c("inputs", "both")) {
      df=tibble::tibble(post_svid = svids)
      inputs = dplyr::inner_join(flywireids, df,
                                by = "post_svid",
                                copy = TRUE,
                                auto_index = TRUE)
    }
    if(partners %in% c("outputs", "both")) {
      df=tibble::tibble(pre_svid = svids)
      outputs = dplyr::inner_join(flywireids, df,
                                 by = "pre_svid",
                                 copy = TRUE,
                                 auto_index = TRUE)
    }

    resdf <- if(partners == "both") {
      dplyr::union(inputs, outputs, all=F)
    } else {
      if (partners == "outputs") outputs else inputs
    }
  }

  if(isTRUE(details) && method!='spine') {
    if(Verbose)
      message("Finding additional details for synapses")
    # spine returns different details from sqlite, this avoids duplicate cols
    colswehave=setdiff(colnames(resdf), "offset")
    # nb we sort by offset here with arrange
    resdf <- synlinks %>%
      select(!dplyr::any_of(colswehave)) %>%
      dplyr::inner_join(resdf, by="offset", copy=TRUE) %>%
      dplyr::arrange(.data$offset)
  }
  # this will run the query for the sqlite case
  resdf=as.data.frame(resdf)
  # sort if we didn't already, strangely this slows down query when details=FALSE
  # sqlite seems to choose the wrong strategy in order to use an index for sorting
  # instead of making the join efficient
  if(!details)
    resdf=dplyr::arrange(resdf, .data$offset)
  rownames(resdf) <- NULL
  # reorder columns so that they are always in same order
  preferredcolorder=c("offset", "pre_x", "pre_y", "pre_z", "post_x", "post_y", "post_z",
                      "scores", "cleft_scores", "segmentid_pre", "segmentid_post",
                      "pre_svid", "post_svid", "post_id", "pre_id")
  colstomatch <- union(preferredcolorder, colnames(resdf))
  colstomatch=intersect(colstomatch, colnames(resdf))
  resdf=resdf[match(colstomatch, colnames(resdf))]

  if(isTRUE(roots)) {
    if(nrow(resdf)==0) {
      # special case filling in empty columns when no results
      resdf$post_id=bit64::integer64()
      resdf$pre_id=bit64::integer64()
      if(length(partners)>1)
        resdf$prepost=integer()
    } else {
      if(Verbose){
        message("Fetching root ids")
      }
      if(partners=="outputs"){
        resdf$post_id=flywire_rootid(resdf$post_svid, integer64 = T, cloudvolume.url=cloudvolume.url)
        resdf$pre_id=as.integer64(rootid)
      } else if (partners=="inputs") {
        resdf$pre_id=flywire_rootid(resdf$pre_svid, integer64 = T, cloudvolume.url=cloudvolume.url)
        resdf$post_id=as.integer64(rootid)
      } else {
        nrows=nrow(resdf)
        combined_svids=c(resdf$pre_svid, resdf$post_svid)
        stopifnot(length(combined_svids)==nrows*2)
        combined_rootids=flywire_rootid(combined_svids, integer64 = T,
                                        cloudvolume.url=cloudvolume.url)

        resdf$pre_id=combined_rootids[seq_len(nrows)]
        resdf$post_id=combined_rootids[seq_len(nrows)+nrows]
        resdf$prepost = ifelse(as.character(resdf$pre_id)%in%rootid,0,1)
      }
    }
  }
  resdf
}


spine_svids2synapses <- function(svids, Verbose, partners, details=FALSE) {
  url="https://spine.janelia.org/app/synapse-service/segmentation/flywire_supervoxels/csv"
  if(isTRUE(details))
    url=paste0(url, "?locations=true&nt=eckstein2020")
  resp=httr::POST(url, body=list(query_ids=svids), encode = 'json')
  httr::stop_for_status(resp)
  # fread looks after int64 values, but ask for regular data.frame
  if(Verbose)
    message("Reading synapse data")
  resdf <- data.table::fread(text = httr::content(resp, as='text', encoding = 'UTF-8'), data.table=FALSE)
  colnames(resdf)[1:5] <- c("offset", 'pre_svid', "post_svid", "scores", "cleft_scores")
  # we can get the same row appearing twice for autapses
  resdf <- filter(resdf, !duplicated(.data$offset))
  if (partners == "outputs"){
    resdf <-  filter(resdf, .data$pre_svid %in% svids)
  } else if (partners == "inputs"){
    resdf <-  filter(resdf, .data$post_svid %in% svids)
  }
  resdf
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
#' @inheritParams flywire_ntplot
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
                                    threshold=0, remove_autapses=TRUE,
                                    cleft.threshold = 0,
                                    method=c("auto", "spine", "sqlite"),
                                    Verbose=NA, local = NULL, ...) {
  check_package_available('tidyselect')
  partners=match.arg(partners)
  rootid=ngl_segments(rootid, unique = TRUE, must_work = TRUE)
  details = cleft.threshold>0
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
      cleft.threshold=cleft.threshold,
      method=method,
      ...
    )
    df = dplyr::bind_rows(res, .id = 'query')
    return(df)
  }

  if(is.na(Verbose)) Verbose=TRUE

  partnerdf=flywire_partners(rootid, partners=partners, local = local, details = details, Verbose = Verbose, method = method)
  # partnerdf=flywire_partners_memo(rootid, partners=partners)
  if(remove_autapses) {
    partnerdf=partnerdf[partnerdf$post_id!=partnerdf$pre_id,,drop=FALSE]
  }
  if(details){
    partnerdf = dplyr::filter(partnerdf, .data$cleft_scores>=cleft.threshold)
  }
  groupingcol=if(partners=='outputs') "post_id" else "pre_id"
  querycol=if(partners!='outputs') "post_id" else "pre_id"

  res <- partnerdf %>%
    group_by(.data[[groupingcol]]) %>%
    summarise(weight=n(), n=length(unique(.data[[querycol]]))) %>%
    arrange(desc(.data$weight)) %>%
    filter(.data$weight>threshold)

  # convert 64 bit ints to char (safer but bigger)
  is64=sapply(res, is.integer64)
  if(any(is64)) {
    for(i in which(is64)) {
      res[[i]]=as.character(res[[i]])
    }
  }
  res
}


#' Fetch the synaptic adjacency matrix for a set of flywire neurons
#'
#' @section Limitations: This function is currently much more efficient when
#'   local SQLite tables are available; in their absence queries to the remote
#'   \emph{spine} server are possible but currently transfer more data than
#'   necessary. Future work could allow \emph{spine} queries than consider both
#'   pre and postsynaptic supervoxel ids as part of the query.
#'
#'   You should also be careful about how many neurons you attempt to query. The
#'   function is not designed to handle queries involving hundreds of neurons
#'   with the spine method being especially sensitive to overloading. If this is
#'   your intention, you might be better off using
#'   \code{\link{flywire_partners}} or \code{\link{flywire_partner_summary}}
#'   both of which fetch data in chunks and then manually filtering down to your
#'   ensemble of interest.
#'
#' @section Normalisation: It is always important to give careful thought to
#'   data normalisation when analysing these connectivity matrices. In general
#'   we feel that normalising by the total input onto each target cell makes the
#'   most sense, since this approximates the effectiveness of input in making
#'   the target cell fire. However if you do not include all inputs onto the
#'   target cells then even this normalisation has difficulties and it may be
#'   better to use raw counts.
#'
#' @description  Get an adjacency matrix for the predicted synaptic connectivity
#'   within a set of specific flywire bodies. You can specify a single pool of
#'   ids or separate input (upstream) and output (downstream) ids. In contrast
#'   to \code{\link{flywire_partner_summary}} this only returns connections
#'   amongst a defined set of ids rather than all possible partners.
#' @param rootids flywire root ids for the bodies to fetch all by all
#'   connectivity information.
#' @param inputids,outputids identifiers for input and output bodies (use as an
#'   alternative to \code{rootids})
#' @param sparse Whether to return a sparse matrix (default \code{FALSE})
#' @param remove_autapses whether to remove autapses (self-connections); most of
#'   these are erroneous.
#' @param cleft.threshold @inheritParams flywire_ntplot
#' @param Verbose Logical indication whether to print status messages during the
#'   query (default \code{T} when interactive, \code{F} otherwise).
#' @inheritParams flywire_partners
#'
#' @return A matrix with named rows of inputs and columns of outputs. The matrix
#'   will be square when rootids is specified but may otherwise be rectangular.
#'   Defaults to a regular (dense) matrix unless \code{sparse=TRUE}.
#' @family automatic-synapses
#' @export
#' @importFrom Matrix sparseMatrix
#' @examples
#' \donttest{
#' u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5392055178100736"
#' sm=flywire_adjacency_matrix(u)
#' # scaled to give proportion of inputs onto each target cell
#' heatmap(sm, scale='col')
#' # scale='none' => raw counts
#' # nb note use of assignment and keep.dendro so we can use dendrogram later
#' h=heatmap(sm, scale='none', keep.dendro = TRUE)
#' # same but with the cleft threshold applied
#' smc=flywire_adjacency_matrix(u, cleft.threshold = 30)
#' # note the reuse of the earlier dendrogram to return col order for comparison
#' heatmap(smc, scale='none', Colv=h$Colv)
#' # just a single upstream neuron
#' sm2=flywire_adjacency_matrix(inputids="720575940625862385", outputids=u)
#' }
flywire_adjacency_matrix <- function(rootids = NULL, inputids = NULL,
                                     outputids = NULL, sparse = FALSE,
                                     remove_autapses=TRUE,
                                     cleft.threshold = 0,
                                     Verbose=interactive(),
                                     method=c("auto", "spine", "sqlite")) {

  if (is.null(rootids)) {
    if (is.null(inputids) || is.null(outputids))
      stop("You must either specify bodyids OR (inputids AND outputids)!")
    inputids = ngl_segments(inputids)
    outputids = ngl_segments(outputids)
  } else {
    if (!is.null(inputids) || !is.null(outputids))
      stop("You must either specify bodyids OR (inputids AND outputids)!")
    inputids <- ngl_segments(rootids)
    outputids <- inputids
  }

  method=match.arg(method)
  flywireids <- flywireids_tbl()
  synlinks <- synlinks_tbl()
  if(method=='spine' ) {
    if(is.null(flywireids))
      stop("I cannot find the flywire svid sqlite database!")
    if(is.null(synlinks))
      stop("I cannot find the Buhmann sqlite database!")
  } else if(method=='auto') method=ifelse(is.null(synlinks)||is.null(flywireids), "spine", "sqlite")

  if(Verbose)
    message("Looking up supervoxel ids")
  outputsvids=flywire_leaves(outputids, integer64=TRUE)
  if(length(outputids)==1)
    outputsvids=list(outputsvids)
  inputsvids=flywire_leaves(inputids, integer64=TRUE)
  if(length(inputids)==1)
    inputsvids=list(inputsvids)

  # nb unlisting destroys the integer64 class, so we need to add it back
  # record the index into the input root id arrays
  dfin=data.frame(
    pre_svid=structure(unlist(inputsvids, use.names = F), class="integer64"),
    pre_rootidx=rep(seq_along(inputsvids), sapply(inputsvids, length)))
  dfout=data.frame(
    post_svid=structure(unlist(outputsvids, use.names = F), class="integer64"),
    post_rootidx=rep(seq_along(outputsvids), sapply(outputsvids, length)))

  if(method=="spine") {
    if(Verbose)
      message("Fetching synapse data from spine server")
    # merge all svids
    allrows <- if(nrow(dfin) < nrow(dfout)) {
      spine_svids2synapses(svids = dfin$pre_svid, Verbose = Verbose, partners = 'outputs')
    } else {
      spine_svids2synapses(svids = dfout$post_svid, Verbose = Verbose, partners = 'inputs')
    }
    dd <- allrows %>%
      filter(pre_svid %in% dfin$pre_svid & post_svid %in% dfout$post_svid) %>%
      mutate(pre_rootidx=dfin$pre_rootidx[match(pre_svid, dfin$pre_svid)]) %>%
      mutate(post_rootidx=dfout$post_rootidx[match(post_svid, dfout$post_svid)])
  } else {
    # sqlite version
    if(Verbose)
      message("Running SQLite query for partners")
    dd <- flywireids %>%
      inner_join(dfin, by='pre_svid', copy=T) %>%
      inner_join(dfout, by='post_svid', copy=T) %>%
      inner_join(x=synlinks, by='offset', copy=T)
  }

  if(cleft.threshold>0) {
    dd=filter(dd, .data$cleft_scores>cleft.threshold)
  }
  dd=as.data.frame(dd)
  if(remove_autapses) {
    dd <- filter(dd, .data$pre_rootidx!=.data$post_rootidx)
  }

  sm = sparseMatrix(
    i = dd$pre_rootidx,
    j = dd$post_rootidx,
    dims = c(length(inputids), length(outputids)),
    x = 1L,
    dimnames = list(inputids, outputids)
  )
  if (isTRUE(sparse))
    sm
  else as.matrix(sm)
}


## Functions for neurotransmitter prediction


#' Return raw neurotransmitter prediction results for output of flywire neuron
#'
#' @param x A single root id as a string OR a \code{data.frame} of output
#'   (downstream) partners returned by \code{flywire_partners}.
#' @inheritParams flywire_partners
#' @inheritParams flywire_ntplot
#' @return A \code{data.frame} of neurotransmitter predictions
#' @importFrom dplyr select arrange inner_join rename
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
flywire_ntpred <- function(x,
                           cleft.threshold=0, remove_autapses=TRUE,
                           local=NULL, cloudvolume.url = NULL) {
  if(is.data.frame(x)) {
    rootid=attr(x,'rootid')
  } else {
    rootid=ngl_segments(x, as_character = TRUE)
    x <- flywire_partners(rootid, partners = 'outputs', roots = TRUE, Verbose=FALSE, cloudvolume.url = cloudvolume.url, local = local)
  }
  if(remove_autapses && all(c("post_id","pre_id")%in%colnames(x))){
    x <- x[x$post_id!=x$pre_id,,drop=FALSE]
  }else if (remove_autapses){
    warning("pre_id and post_id must be given to find and remove autapses")
  }
  poss.nts=c("gaba", "acetylcholine", "glutamate", "octopamine", "serotonin", "dopamine")
  extracols=c("scores", "cleft_scores","pre_x", "pre_y", "pre_z")
  stopifnot(is.data.frame(x))
  if(all(poss.nts %in% colnames(x))) {
    # looks like we already got the NT info
  } else {
    # NB the sqlite table has to come first in the join
    ntpredictions=ntpredictions_tbl(local=local)
    if(is.null(ntpredictions))
      stop("I cannot find the neurotransmitter predictions sqlite database!")

    x = ntpredictions %>%
      inner_join(x, copy = TRUE, by=c("id"="offset")) %>%
      rename(offset=.data$id)
  }

  if(!all(extracols %in% colnames(x))) {
    missing_cols <- setdiff(extracols, colnames(x))
    synlinks=synlinks_tbl(local=local)
    if(is.null(synlinks))
      stop("I cannot find the Buhmann sqlite database required to fetch synapse details!")
    x = synlinks %>%
      select(union("offset", missing_cols)) %>%
      dplyr::inner_join(x, copy = TRUE, by = "offset")
  }
  # finish query ...
  x=x%>%
    dplyr::filter(.data$cleft_scores>=cleft.threshold)  %>%
    arrange(.data$offset) %>%
    as.data.frame()
  # this avoids using matrixStats::rowMaxs and is just as fast
  x[,'top.p']=do.call(pmax, as.list(x[poss.nts]))
  # this has slightly odd default behaviour of choosing a random tie breaker
  # for things within 1e-5 of each other, which may not match above exactly
  # this is a rare event, but does occur
  top.col=max.col(x[poss.nts], ties.method = "first")
  x[,'top.nt']=poss.nts[top.col]
  class(x)=union("ntprediction", class(x))
  attr(x,'rootid')=rootid
  x
}

#' @export
#' @param ... additional arguments passed to \code{\link{print}}
#' @rdname flywire_ntpred
#' @description the \code{print.ntprediction} method provides a quick summary of
#'   the neurotransmitter prediction for all output synapses.
print.ntprediction <- function(x, ...) {
  ids=attr(x, 'rootid')
  if(length(ids)>1) {
    cat(length(ids), "neurons with a total of ", nrow(x), "output synapses\n")
    by(x, x$query, function(x) {attr(x, 'rootid')=unique(x$query);print(x)}, simplify = F)
    return(invisible(x))
  }
  tx=table(x$top.nt)
  cat("neuron", ids, "with", sum(tx), "output synapses:")
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
#' @return \code{flywire_ntplot} returns a \code{ggplot2::\link[ggplot2]{ggplot}} object
#'   that can be further customised to modify the plot (see examples).
#' @family automatic-synapses
#' @examples
#' \donttest{
#' # a cholinergic olfactory projection neuron
#' ntp=flywire_ntpred("720575940615237849")
#' flywire_ntplot(ntp)
#' flywire_ntplot(ntp, nts=c("gaba", "acetylcholine", "glutamate"))
#' flywire_ntplot(ntp, nts=c("gaba", "acetylcholine", "glutamate"), cleft.threshold=100)
#'
#' # ids for several Kenyon cells
#' kcsel=c("720575940623755722", "720575940609992371", "720575940625494549",
#' "720575940619442047", "720575940620517656", "720575940609793429",
#' "720575940617265029", "720575940631869024", "720575940637441955",
#' "720575940638892789")
#' kcpreds=flywire_ntpred(kcsel)
#' # collect the ggplot object
#' p <- flywire_ntplot(kcpreds)
#' # print it to see the aggregate plot (all neurons together)
#' p
#' # ... or use ggplot facets to separate by query neuron
#' p+ggplot2::facet_wrap(query~.)
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

  ggplot2::qplot(x$top.p, fill=x$top.nt, xlab = 'probability', data=x) +
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
#' @description Attach the appropriate input and output synapses to each flywire
#'   neuron skeleton in a neuronlist.
#' @param x a \code{nat::neuronlist} for flywire neurons in the FlyWire or
#'   FAFB14 brainspace. These skeletons can be created using
#'   \code{\link{skeletor}}, or retrieved using
#'   \code{hemibrainr::flywire_neurons}. When using
#'   \code{flywire_synapse_annotations}
#'   this can be a \code{data.frame} of synapses, e.g. from
#'   \code{flywire_ntpred}
#'   that need to be formatted as FlyWire annotations.
#' @param connectors a \code{data.frame} of FAFB synapses, with XYZ coordinates,
#'   to attach to \code{x}. If \code{NULL} (default) synapses are fetched, as in
#'   \code{\link{flywire_partners}}.
#' @param remove_autapses  whether to remove autapses (defaults to \code{TRUE}).
#' @param transmitters if \code{TRUE} also attempt to retrieve neurotransmitter
#'   predictions from Eckstein et al. 2020, for the flywire neuron in question.
#' @param cleft.threshold select only synaptic connections exceeding this
#'   confidence threshold (default of 0 uses all synapses; values in the range
#'   30-100 seem to make sense).
#' @param file when using \code{flywire_synapse_annotations}, the filepath to
#' which to output a \code{.csv}. If \code{NULL}, a \code{data.frame} formatted
#' like a annotations CSV for FlyWire, is returned.
#' @param sample if an integer, this is the number of synapses that are sampled
#' from \code{x}.
#' @param scale a scale factor applied to the XYZ coordinates for synapses.
#' Default moves them
#' from nanometer FlyWire space to raw voxel FlyWire space, which is most
#' appropriate
#' for FlyWire annotations.
#' @param best logical. If \code{TRUE} and sample is an integer, then the
#' synapses with the highest cleft scores are chosen, \code{1:sample}.
#' @param ... methods sent to \code{nat::nlapply}.
#' @inheritParams flywire_partners
#' @inheritParams flywire_ntpred
#'
#' @return A \code{nat::neuronlist} object, where each neuron in the neuronlist
#'   has a \code{data.frame} of synapses at neuron$connectors.
#'
#' @export
#' @family automatic-synapses
#'
#' @examples
#' \donttest{
#' \dontrun{
#' choose_segmentation("flywire")
#' nx=xform_brain(elmr::dense_core_neurons, ref="FlyWire", sample="FAFB14")
#' xyz = xyzmatrix(nx)
#' ids = unique(flywire_xyz2id(xyz[sample(nrow(xyz),100),]))
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
#'
#' # Save .csv of synapses as FlyWire annotations
#' flywire_synapse_annotations(ids[1], file="annotations1.csv",
#' cleft.threshold=30)
#'
#' # And similar, from a neuronlist
#' syns = hemibrainr::hemibrain_extract_synapses(neurons.flow,
#' .parallel = TRUE, OmitFailures = TRUE)
#' flywire_synapse_annotations(syns, file="annotations2.csv",
#' cleft.threshold=30)
#'
#' }
#' }
flywire_neurons_add_synapses <- function(x,
                                         connectors = NULL,
                                         cloudvolume.url=NULL, # fafbseg.cloudvolume.url="graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31"
                                         method=c("auto", "spine", "sqlite"),
                                         remove_autapses = TRUE,
                                         cleft.threshold = 0,
                                         Verbose=TRUE,
                                         transmitters=TRUE,
                                         local = NULL, # "/Volumes/nnautilus/projects/JanFunke"
                                         ...) UseMethod("flywire_neurons_add_synapses")

#' @export
#' @rdname flywire_neurons_add_synapses
flywire_neurons_add_synapses.neuron <- function(x,
                                                connectors = NULL,
                                                cloudvolume.url=NULL,
                                                method=c("auto", "spine", "sqlite"),
                                                remove_autapses = TRUE,
                                                cleft.threshold = 0,
                                                Verbose=TRUE,
                                                transmitters=TRUE,
                                                local = NULL,
                                                ...){
  method = match.arg(method)
  rootid = x$flywire.id
  poss.nts=c("gaba", "acetylcholine", "glutamate", "octopamine", "serotonin","dopamine")
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
    if(is.null(synapses) || nrow(synapses)==0){
      class(x) = union(c("flywireneuron", "catmaidneuron"), class(x))
      return(x)
    }
    if(remove_autapses) {
      synapses=synapses[synapses$post_id!=synapses$pre_id,,drop=FALSE]
    }
    # Add synapses
    synapses %>%
      dplyr::filter(.data$cleft_scores >= cleft.threshold) %>%
      dplyr::mutate(x = ifelse(.data$prepost, .data$post_x, .data$pre_x)) %>%
      dplyr::mutate(y = ifelse(.data$prepost, .data$post_y, .data$pre_y)) %>%
      dplyr::mutate(z = ifelse(.data$prepost, .data$post_z, .data$pre_z)) %>%
      dplyr::arrange(.data$offset) %>%
      dplyr::select("offset", "prepost", "x", "y", "z","scores", "cleft_scores",
                    "segmentid_pre", "segmentid_post", "pre_svid", "post_svid",
                    "pre_id", "post_id") %>%
      as.data.frame() ->
      synapses.xyz
    rownames(synapses.xyz) = synapses.xyz$offset
  } else {
    synapses.xyz=connectors[connectors$post_id%in%rootid|connectors$pre_id%in%rootid,,drop=FALSE]
  }
  # If transmitters
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
  # Attach synapses to skeleton
  nat::xyzmatrix(synapses.xyz) = fafb2flywire(nat::xyzmatrix(synapses.xyz))
  near = nabor::knn(query= nat::xyzmatrix(synapses.xyz),data=nat::xyzmatrix(x$d),k=1)
  synapses.xyz$treenode_id = x$d[near$nn.idx,"PointNo"]
  synapses.xyz$connector_id = synapses.xyz$segmentid_pre
  x$connectors = as.data.frame(synapses.xyz, stringsAsFactors = FALSE)
  if(transmitters){
    x$connectors[,colnames(x$connectors)%in%poss.nts] = round(x$connectors[,colnames(x$connectors)%in%poss.nts],digits=2)
  }
  # Get top transmitter result
  tx=table(subset(synapses.xyz, synapses.xyz$prepost == 0)$top.nt)
  tx=sort(tx, decreasing = TRUE)/sum(tx)*100
  x$ntpred = tx
  class(x) = union(c("flywireneuron", "catmaidneuron"), class(x))
  attr(x,'rootid')=rootid
  x
}

#' @export
#' @rdname flywire_neurons_add_synapses
flywire_neurons_add_synapses.neuronlist <- function(x,
                                                    connectors=NULL,
                                                    cloudvolume.url=NULL,
                                                    method=c("auto", "spine", "sqlite"),
                                                    remove_autapses=TRUE,
                                                    cleft.threshold = 0,
                                                    Verbose=TRUE,
                                                    transmitters=FALSE,
                                                    local=NULL,
                                                    ...){
  poss.nts=c("gaba", "acetylcholine", "glutamate", "octopamine", "serotonin","dopamine")
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
                         cleft.threshold=cleft.threshold,
                         Verbose = Verbose,
                         transmitters = transmitters,
                         local = local,
                         ...)
  extract_ntpredictions.neuronlist(neurons.syn)
}
# neurons.syns = flywire_neurons_add_synapses(neurons, transmitters = TRUE, local =  "/Volumes/nnautilus/projects/JanFunke")

# extract predictions neurons
extract_ntpredictions.neuronlist <- function(x,
                                             poss.nts=c("gaba", "acetylcholine", "glutamate", "octopamine", "serotonin","dopamine")){
  nmeta = lapply(x, extract_ntpredictions.neuron,poss.nts=poss.nts)
  nmeta = do.call(rbind, nmeta)
  df=x[,]
  # it seems the (flywire.)id column is not consistently named
  idcol=colnames(df)[1]
  # keep first col (id) and anything else not in nmeta
  tokeep=union(1, which(!(colnames(df) %in% colnames(nmeta))))
  colnames(df)[1]='flywire.id'
  df=df[tokeep]
  meta2 = dplyr::inner_join(df, nmeta,
                            by = "flywire.id",
                            copy = TRUE,
                            auto_index = TRUE)
  rownames(meta2) = meta2$flywire.id
  colnames(meta2)[colnames(meta2)=='flywire.id']=idcol
  x[,] = meta2
  x
}

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
      top.nt = names(tops)[which.max(tops)][1]
    }else{
      top.p = NA
      top.nt = "unknown"
    }
    pre = nullToZero(sum(synapses$prepost==0))
    post = nullToZero(sum(synapses$prepost==1))
    data.frame(flywire.id = flywire.id, top.nt = top.nt, top.p = top.p, pre = pre, post = post, stringsAsFactors = FALSE)
  }
}

# Get synapses as FlyWire annotations
#' @export
#' @rdname flywire_neurons_add_synapses
flywire_synapse_annotations <- function(x,
                                        file = NULL,
                                        scale = 1/c(4,4,40), # from nm to voxel space
                                        sample = NULL,
                                        best = TRUE,
                                        cleft.threshold = 30,
                                        remove_autapses = TRUE,
                                        local = NULL, # "/Volumes/GoogleDrive/Shared drives/hemibrain/fafbsynapses"
                                        cloudvolume.url = NULL){
  if(is.data.frame(x)||is.table(x)){
    synapse.sample = x
    if("cleft_scores"%in%colnames(x)){
      synapse.sample <- synapse.sample %>%
        dplyr::filter(.data$cleft_scores > cleft.threshold) %>%
        dplyr::collect()
    }
  }else{
    synapse.sample = flywire_ntpred(x,
                                    cleft.threshold=cleft.threshold,
                                    remove_autapses=remove_autapses,
                                    local=local,
                                    cloudvolume.url=cloudvolume.url)
    synapse.sample[,c("x","y","z")] = fafb2flywire(synapse.sample[,c("pre_x","pre_y","pre_z")])
  }
  if(!is.null(sample)){
    sample = checkmate::asInt(sample)
    if(!is.integer(sample)){
      stop("Sample must be NULL or an integer")
    }
    if(best){
      synapse.sample = synapse.sample[order(synapse.sample$cleft_scores, decreasing = TRUE),]
      synapse.sample = synapse.sample[1:min(sample,nrow(synapse.sample)),]
    }else{
      synapse.sample <- synapse.sample %>%
        dplyr::sample_n(size=sample, replace = FALSE) %>%
        dplyr::collect()
    }
  }
  if(!is.null(scale)){
    synapse.sample$`Coordinate 1` = apply(nat::xyzmatrix(synapse.sample),1,function(x) paste_coords(x*scale))
  }else{
    synapse.sample$`Coordinate 1` = apply(nat::xyzmatrix(synapse.sample),1,function(x) paste_coords(x))
  }
  # need columns: Coordinate 1	Coordinate 2	Ellipsoid Dimensions	Tags	Description	Segment IDs	Parent ID	Type	ID
  flywire.scan = data.frame(`Coordinate 1` = synapse.sample$`Coordinate 1`,
                            `Coordinate 2` = "",
                            `Ellipsoid Dimensions` = "",
                            tags = "",
                            Description = nullToZero(synapse.sample$top.nt, fill = NA),
                            `Segment IDs` = "",
                            `Parent ID` = "",
                            Type = "Point",
                            ID = "",
                            offset = nullToZero(synapse.sample$offset, fill = NA),
                            cleft_scores = nullToZero(synapse.sample$cleft_scores, fill = NA),
                            top.nt = nullToZero(synapse.sample$top.nt, fill = NA),
                            Label = nullToZero(synapse.sample$Label, fill = NA))
  colnames(flywire.scan) = gsub("\\."," ",colnames(flywire.scan))
  flywire.scan$`Coordinate 1` = as.character(flywire.scan$`Coordinate 1`)
  if(!is.null(file)){
    utils::write.csv(flywire.scan, file = file, row.names = FALSE)
  }else{
    flywire.scan
  }
}

# hidden
paste_coords <- function (xyz){
  paste0("(", paste(xyz, sep = ",", collapse = ","), ")")
}
