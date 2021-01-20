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
  rootid=ngl_segments(rootid, as_character = TRUE, must_work = TRUE)
  if(method!="spine") {
    flywireids=flywireids_tbl(local=local)
    if(method=='auto')
      method <- if(is.null(flywireids)) "spine" else "sqlite"
    else {
      if(is.null(flywireids))
        stop("method=sqlite but could not connect to flywireids database!")
    }
  }

  if(isTRUE(details)) {
    synlinks=synlinks_tbl(local=local)
    if(is.null(synlinks))
      stop("I cannot find the Buhmann sqlite database required when details=TRUE!")
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

  if(!bit64::is.integer64(svids))
    svids=bit64::as.integer64(as.character(svids))
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
    } else if (partners == "inputs"){
      resdf <-  filter(resdf, .data$post_svid %in% svids)
    }
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

  if(isTRUE(details)) {
    if(Verbose)
      message("Finding additional details for synapses")
    # nb we sort by offset here with arrange
    resdf <- synlinks %>%
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

  if(nrow(resdf)>0 && isTRUE(roots)) {
    message("Fetching root ids")
    if(partners=="outputs"){
      resdf$post_id=bit64::as.integer64(flywire_rootid(resdf$post_svid, cloudvolume.url=cloudvolume.url))
      resdf$pre_id=bit64::as.integer64(rootid)
    } else if (partners=="inputs") {
      resdf$pre_id=bit64::as.integer64(flywire_rootid(resdf$pre_svid, cloudvolume.url=cloudvolume.url))
      resdf$post_id=bit64::as.integer64(rootid)
    } else {
      resdf$pre_id=bit64::as.integer64(flywire_rootid(resdf$pre_svid, cloudvolume.url=cloudvolume.url))
      resdf$post_id=bit64::as.integer64(flywire_rootid(resdf$post_svid, cloudvolume.url=cloudvolume.url))
      resdf$prepost = ifelse(as.character(resdf$pre_id)%in%rootid,0,1)
    }
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
flywire_ntpred <- function(x, local=NULL, cloudvolume.url = NULL) {
  if(is.data.frame(x)) {
    rootid=attr(x,'rootid')
  } else {
    rootid=ngl_segments(x, as_character = T)
    x <- flywire_partners(rootid, partners = 'outputs', roots = FALSE, Verbose=FALSE, cloudvolume.url = cloudvolume.url, local = local)
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

    x = ntpredictions %>%
      inner_join(x, copy = T, by=c("id"="offset")) %>%
      rename(offset=.data$id)
  }

  if(!all(extracols %in% colnames(x))) {
    missing_cols <- setdiff(extracols, colnames(x))
    synlinks=synlinks_tbl(local=local)
    if(is.null(synlinks))
      stop("I cannot find the Buhmann sqlite database required to fetch synapse details!")
    x = synlinks %>%
      select(union("offset", missing_cols)) %>%
      dplyr::inner_join(x, copy = T, by = "offset")
  }
  # finish query ...
  x=x%>%
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
#' @family automatic-synapses
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

  ggplot2::qplot(top.p, fill=top.nt, xlab = 'probability', data=x) +
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

