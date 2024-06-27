check_cave <- memoise::memoise(function(min_version=NULL) {
  check_reticulate()
  tryCatch(
    cv <- reticulate::import("caveclient"),
    error = function(e) {
      stop(
        call. = F,
        "Please install the python caveclient package:\n",
        "This should normally work:\n",
        "fafbseg::simple_python('full')\n",
        "For more details see ?simple_python"
      )
    }
  )
  if(!is.null(min_version)) {
    warning("min_version not yet implemented for caveclient")
  #   cvv=numeric_version(cloudvolume_version())
  #   if(!isTRUE(cvv >= min_version))
  #     stop("You need cloudvolume version: ", min_version, " but you have: ", cvv,
  #          "\n  Please update e.g. using\n",
  #          "fafbseg::simple_python('basic')")
  }
  cv
})



#' Low level access to the Flywire CAVE annotation system
#'
#' @details This depends on installation of the Python caveclient library. See
#'   \code{\link{flywire_cave_query}} for more details.
#' @param datastack_name defaults to "flywire_fafb_production". See
#'   \url{https://global.daf-apis.com/info/} for other options.
#' @return The \code{caveclient.frameworkclient.CAVEclientFull} Python module
#'   wrapped by reticulate.
#' @export
#'
#' @examples
#' \donttest{
#' fac <- flywire_cave_client()
#' fac$annotation$get_tables()
#' fac$annotation$get_table_metadata('nuclei_v1')
#'
#' # annotation tables that have been materialised in order to map XYZ
#' # points onto current root ids (and immutable supervoxel ids)
#' # typically the same as fac$annotation$get_tables()
#' fac$materialize$get_tables()
#'
#' info=fac$info$get_datastack_info()
#' # the default synapse table for the dataset
#' info$synapse_table
#' }
flywire_cave_client <- memoise::memoise(function(datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  cavec=check_cave()
  client = cavec$CAVEclient(datastack_name)
}, ~memoise::timeout(12*3600))

#' Query the FlyWire CAVE annotation system
#'
#' @details CAVE (Connectome Annotation Versioning Engine) provides a shared
#'   infrastructure for a number of connectomics projects involving Sebastian
#'   Seung's group at Princeton and collaborators at the Allen Institute. There
#'   is both a backend system running on their servers and a Python client for
#'   end users.
#'
#'   You can find out more at \url{https://caveclient.readthedocs.io/} as well
#'   as looking at the Python notebooks on the github repo
#'   \url{https://github.com/seung-lab/CAVEclient}.
#'
#'   The annotation system shares authentication infrastructure with the rest of
#'   the FlyWire API (see \code{\link{flywire_set_token}}).
#'
#' @section CAVE Materialisation Versions and Timestamps: CAVE has a concept of
#'   table snapshots identified by an integer materialization \code{version}
#'   number. In some cases you may wish to query a table at this defined version
#'   number so that you can avoid root_ids changing during an analysis. Your
#'   calls will also be faster since no root id updates are required.
#'
#'   Note however that materialisation versions expire at which point the
#'   corresponding version of the database is expunged. However it is still
#'   possible to find the timestamp for an expired materialisation version.
#'   \code{flywire_cave_query} does this automatically using
#'   \code{\link{flywire_timestamp}}. In these circumstances queries will again
#'   be slower (quite possibly slower than the live query) since all root ids
#'   must be recalculated to match the timestamp.
#'
#'   CAVE's ability to handle different timepoints is key to analysis for a
#'   continually evolving segmentation but is frankly a little difficult for
#'   users to work with. You will find things simplest if you either \itemize{
#'
#'   \item use a long-term support (LTS) version, which will not expire such as
#'   the 630 version for the June 2023 public release or the 783 version to
#'   accompany the published FlyWire papers.
#'
#'   \item use the latest CAVE version
#'
#'   }
#' @section CAVE Views: In addition to regular database tables, CAVE provides
#'   support for \bold{views}. These are based on a SQL query which typically
#'   aggregates or combines multiple tables. For an example an aggregation might
#'   define the total number of output synapses for some selected neurons.
#'
#'   At present there are several restrictions on views. For example, you can
#'   only fetch views using an unexpired materialisation version (and you cannot
#'   specify a timepoint using a timestamp) . Furthermore some \code{filter_in,
#'   filter_out} queries using columns created by the SQL statement may not be
#'   possible.
#' @param table The name of the table (or view, see views section) to query
#' @param select_columns Either a character vector naming columns or a python
#'   dict (required if the query involves multiple tables).
#' @param live Whether to use live query mode, which updates any root ids to
#'   their current value.
#' @param version An optional CAVE materialisation version number. See details
#'   and examples.
#' @param timestamp An optional timestamp as a string or POSIXct, interpreted as
#'   UTC when no timezone is specified.
#' @param filter_in_dict,filter_out_dict Optional arguments consisting of key
#'   value lists that restrict the returned rows (keeping only matches or
#'   filtering out matches). Commonly used to selected rows for specific
#'   neurons. See examples and CAVE documentation for details.
#' @param ... Additional arguments to the query method. See examples and
#'   details.
#' @inheritParams flywire_cave_client
#'
#' @return A \code{tibble}. Note that xyzmatrix can be used on single columns
#'   containing XYZ locations.
#' @export
#' @seealso \code{\link{flywire_cave_client}}
#' @family cave-queries
#' @examples
#' \donttest{
#' # note use of limit to restrict the number of rows (must be integer)
#' n10=flywire_cave_query(table = 'nuclei_v1', limit=10L)
#' head(as.data.frame(n10))
#' }
#' \dontrun{
#' nuclei_v1=flywire_cave_query(table = 'nuclei_v1')
#' points3d(xyzmatrix(nuclei_v1$pt_position))
#'
#' library(elmr)
#' # calculate signed distance to FAFB surface
#' # NB this surface is not a perfect fit in the optic lobes
#' nuclei_v1$d=pointsinside(xyzmatrix(nuclei_v1$pt_position), FAFB.surf,
#'   rval = 'dist')
#' points3d(xyzmatrix(nuclei_v1$pt_position),
#'   col=matlab::jet.colors(20)[cut(nuclei_v1$d,20)])
#' plot3d(FAFB)
#' }
#' # Example of a query on a table
#' \dontrun{
#' # the Princeton (mostly) and Cambridge groups have tagged some bodies as
#' # not a neuron - these are often glia.
#' nans=flywire_cave_query('neuron_information_v2',
#'   filter_in_dict = list(tag='not a neuron'))
#' nrow(nans)
#' table(nans$user_id)
#' }
#' \dontrun{
#' psp_351=flywire_cave_query(table = 'proofreading_status_public_v1',
#'   version=351)
#' # get the last listed materialisation version
#' fcc=flywire_cave_client()
#' lastv=tail(fcc$materialize$get_versions(), n=1)
#' # pull that
#' psp_last=flywire_cave_query(table = 'proofreading_status_public_v1',
#'   version=lastv)
#' }
flywire_cave_query <- function(table,
                               datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
                               version=NULL,
                               timestamp=NULL,
                               live=is.null(version)&&is.null(timestamp),
                               filter_in_dict=NULL,
                               filter_out_dict=NULL,
                               select_columns=NULL,
                               ...) {
  if(isTRUE(live) && !is.null(version))
    warning("live=TRUE so ignoring materialization version")
  if(isTRUE(live) && !is.null(timestamp))
    warning("live=TRUE so ignoring timestamp")
  if(!is.null(timestamp) && !is.null(version))
    stop("You can only supply one of timestamp and materialization version")

  check_package_available('arrow')
  fac=flywire_cave_client(datastack_name=datastack_name)

  is_view=table %in% cave_views(fac)
  if(!is.null(version)) {
    version=as.integer(version)
    available=version %in% fac$materialize$get_versions()
    if(!available) {
      if(is_view)
        stop("Sorry! Views only work with unexpired materialisation versions.\n",
             "See https://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1697956174773839 for info.")
      timestamp=flywire_timestamp(version, datastack_name = datastack_name, convert = F)
      message("Materialisation version no longer available. Falling back to (slower) timestamp!")
      version=NULL
    }
  }

  if(!is.null(filter_in_dict) && !inherits(filter_in_dict, 'python.builtin.dict'))
    filter_in_dict=cavedict_rtopy(filter_in_dict)
  if(!is.null(filter_out_dict) && !inherits(filter_out_dict, 'python.builtin.dict'))
    filter_out_dict=cavedict_rtopy(filter_out_dict)
  if(!is.null(select_columns)) {
    if(is.character(select_columns) && is_view) {
        stop("You are querying a view. select_columns must be a python dict as specified by caveclient.")
    }
  }
  annotdf <- if(is_view) {
    if(!is.null(timestamp))
      warning("Sorry! You cannot specify a timestamp when querying a view.\n",
      "You can specify older timepoints by using unexpired materialisation versions.\n",
      "See https://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1697956174773839 for info.")
    reticulate::py_call(fac$materialize$query_view, view_name=table,
                        materialization_version=version,
                        filter_in_dict=filter_in_dict,
                        filter_out_dict=filter_out_dict,
                        select_columns=select_columns, ...)
  } else if(live) {
    # Live query updates ids
    timestamp=flywire_timestamp(timestamp = 'now', convert = FALSE)
    reticulate::py_call(fac$materialize$live_query, table=table,
                        timestamp=timestamp, filter_in_dict=filter_in_dict,
                        filter_out_dict=filter_out_dict,
                        select_columns=select_columns, ...)
  } else {
    if(!is.null(timestamp)) flywire_timestamp(timestamp = timestamp, convert = F)
    reticulate::py_call(fac$materialize$query_table, table=table,
                        materialization_version=version,
                        timestamp=timestamp, filter_in_dict=filter_in_dict,
                        filter_out_dict=filter_out_dict,
                        select_columns=select_columns, ...)
  }
  pandas2df(annotdf)
}

cave_views <- memoise::memoise(function(fac=flywire_cave_client()) {
  vv=fac$materialize$get_views()
  names(vv)
})

cavedict_rtopy <- function(dict) {
  # CAVE wants each entry should be a list and ids to be by ints
  for (i in seq_along(dict)) {
    if (all(is.integer64(dict[[i]])) || all(valid_id(dict[[i]])))
      dict[[i]]=rids2pyint(unlist(dict[[i]]))
    else if (!is.list(dict[[i]]))
      dict[[i]]=as.list(dict[[i]])
  }
  checkmate::check_names(names(dict), type = 'unique')
  pydict=reticulate::r_to_py(dict, convert = F)
  pydict
}

flywire_partners_cave <- function(rootid, partners=c("outputs", "inputs"),
                                  cleft.threshold=0,
                                  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
                                  synapse_table=NULL,
                                  fafbseg_colnames=TRUE, ...) {
  checkmate::assert_integerish(cleft.threshold,
                               lower = 0L, upper = 255L, len = 1)
  partners=match.arg(partners)
  fac=flywire_cave_client(datastack_name=datastack_name)
  if(is.null(synapse_table)) {
    synapse_table=fac$info$get_datastack_info()[['synapse_table']]
    if(!isTRUE(nzchar(synapse_table)))
      stop("Unable to identify synapse table for datastack: ", datastack_name)
  }
  dict=list(as.list(as.character(rootid)))
  names(dict)=ifelse(partners=="outputs", "pre_pt_root_id", "post_pt_root_id")
  res=flywire_cave_query(datastack_name = datastack_name,
                         table = synapse_table,
                         filter_in_dict=cavedict_rtopy(dict),
                         ...)
  # FIXME - integrate into CAVE query
  if(cleft.threshold>0)
    res=res[res$cleft_score>cleft.threshold,,drop=FALSE]

  if(fafbseg_colnames) {
    colnames(res)[1]='offset'
    colnames(res)=sub("pt_supervoxel_id", "svid", colnames(res))
    colnames(res)=sub("pt_root_id", "id", colnames(res))
    colnames(res)[colnames(res)=='connection_score']='scores'
    colnames(res)[colnames(res)=='cleft_score']='cleft_scores'
  }
  res
}

# retained in case it is useful somewhere else ...
update_rootids <- function(rootids, svids) {
  stopifnot(bit64::is.integer64(rootids))
  stopifnot(bit64::is.integer64(svids))
  outofdate=!flywire_islatest(rootids)
  if(any(outofdate)) {
    rootids[outofdate]=flywire_rootid(svids[outofdate], integer64 = TRUE)
  }
  rootids
}


cave_latestid <- function(rootid, integer64=FALSE, timestamp=NULL,
                          datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  fac=flywire_cave_client(datastack_name=datastack_name)
  rid=ngl_segments(rootid, as_character=T)
  if(length(rid)!=1 || !valid_id(rid))
    stop("cave_latestid expects a single valid rootid")
  # get a single python int via a python list
  pyids=rids2pyint(rid)
  pyid=pyids[0]
  res=reticulate::py_call(fac$chunkedgraph$get_latest_roots, pyid, timestamp_future=timestamp)
  newids=pyids2bit64(res, as_character = !integer64)
  newids
}


ts2pydatetime <- function(x) {
  if(inherits(x, 'datetime.datetime'))
    return(x)
  dt=reticulate::import('datetime')
  x2=as.numeric(as.POSIXlt(x, origin='1970-01-01', "UTC"))
  utc=dt$timezone$utc
  # better to make timezone explicit so no conversion issues later
  reticulate::py_call(dt$datetime$fromtimestamp, x2, utc)
}

# a function to return a python function!
# memoisation saves a few ms
pyslice <- memoise::memoise(function() {
  reticulate::py_run_string(local = T, paste0(
    "def pyslice(x,i):\n",
    "  return x[i]\n",
    "\n"))
})

cave_get_delta_roots <- function(timestamp_past, timestamp_future=Sys.time()) {
  fcc = flywire_cave_client()
  if (is.numeric(timestamp_future))
    timestamp_future = Sys.time() + timestamp_future
  if (is.numeric(timestamp_past))
    timestamp_past = timestamp_future + timestamp_past
  res = tryCatch(
    reticulate::py_call(
      fcc$chunkedgraph$get_delta_roots,
      timestamp_past = ts2pydatetime(timestamp_past),
      timestamp_future = ts2pydatetime(timestamp_future)
    ),
    error = function(e) {
      warning(e)
      list(old = character(), new = character())
    }
  )
  pyslice <- pyslice()$pyslice
  resl <- if (is.list(res))
    res
  else
    list(old = pyids2bit64(reticulate::py_call(pyslice, res, 0L)),
         new = pyids2bit64(reticulate::py_call(pyslice, res, 1L)))
  attr(resl, 'timestamp_past') = timestamp_past
  attr(resl, 'timestamp_future') = timestamp_future
  resl
}


#' Find standard UTC timestamp for flywire materialisation version or timestamp
#'
#' @details Note that all CAVE timestamps are in UTC. When the \code{timestamp}
#'   argument is a character vector \bold{it is assumed to be in UTC regardless
#'   of any timezone specification}. Unless the input character vector contains
#'   the string "UTC" then a warning will be issued.
#' @param version Integer materialisation version
#' @param timestamp A timestamp to normalise into an R or Python timestamp in
#'   UTC. The special value of \code{'now'} means the current time in UTC.
#' @param convert Whether to convert from Python to R timestamp (default:
#'   \code{TRUE})
#' @inheritParams flywire_cave_client
#'
#' @return A POSIXct object or Python datetime object in the UTC timezone.
#' @export
#' @family cave-queries
#' @examples
#' \donttest{
#' ts=flywire_timestamp(349)
#' ts
#' # As a unix timestamp (number of seconds since 00:00 on 1970-01-01)
#' as.numeric(ts)
#' tsp=flywire_timestamp(349, convert=FALSE)
#' # should be same as the numeric timestamp above
#' tsp$timestamp()
#'
#' flywire_timestamp(timestamp="2022-08-28 17:04:49 UTC")
#'
#' # nb this will return the current time *in UTC* regardless of your timezone
#' flywire_timestamp(timestamp="now")
#' }
#' \dontrun{
#' # same but gives a warning
#' flywire_timestamp(timestamp="2022-08-28 17:04:49")
#' }
flywire_timestamp <- function(version=NULL, timestamp=NULL, convert=TRUE,
                              datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  nargs=2L-sum(is.null(timestamp), is.null(version))
  # as a convenience for programmers
  if(nargs==0) return(NULL)
  if(nargs==2)
    stop("You must specify only one of version or timestamp")
  if(!is.null(timestamp)) {
    if(is.character(timestamp) && isTRUE(timestamp=="now"))
      timestamp=Sys.time()
    # if we have a POSIXt timestamp then convert to numeric to remove timezone
    if(inherits(timestamp, 'POSIXt'))
      timestamp=as.numeric(timestamp)
    timestamp <- if(is.numeric(timestamp)) {
      # nb as.numeric in case we have eg integer64
      as.POSIXct(as.numeric(timestamp), tz = 'UTC', origin='1970-01-01')
    } else if(is.character(timestamp)) {
      # a little tricky since any timezone information in string will be ignored
      if(!all(grepl("UTC", timestamp)))
        warning("Assuming that timezone is UTC for character vector input")
      as.POSIXct(timestamp, tz = 'UTC')
    } else if(inherits(timestamp, 'datetime.datetime')) {
      # python timestamp
      return(if(convert) cgtimestamp2posixct(timestamp) else timestamp)
    } else stop("Unsupported timezone class: ",
                paste(class(timestamp), collapse = ','))
    return(if(convert) timestamp else ts2pydatetime(timestamp))
  }

  fac=flywire_cave_client(datastack_name = datastack_name)
  version=as.integer(version)
  res=tryCatch(
    reticulate::py_call(fac$materialize$get_timestamp, version),
    error=function(e) {
    stop("Unable to find version: ", version, " for dataset ", datastack_name,"\nDetails:\n", as.character(e), call. = F)
  })
  if(convert) cgtimestamp2posixct(res) else res
}
