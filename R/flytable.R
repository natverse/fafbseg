check_seatable<- memoise::memoise(function(min_version=NULL) {
  check_reticulate()
  tryCatch(
    st <- reticulate::import("seatable_api"),
    error = function(e) {
      if(interactive()) {
        chc=readline("Install the seatable_api python package for flytable access (y/n)? ")
        if(tolower(chc)=='y') {
          simple_python(pyinstall = 'none', pkgs = 'seatable_api')
          return(reticulate::import("seatable_api"))
        }
      }
      stop(
        call. = F,
        "Please install the python seatable_api package:\n",
        "This should normally work:\n",
        "fafbseg::simple_python(pkgs='seatable_api')\n",
        "For more details see ?simple_python"
      )
    }
  )
  st
})


#' Low level functions to access the flytable metadata service
#'
#' @details Besides initial setup (next paragraph), you should not need to use
#'   these lower level functions directly. Instead we recommend higher level
#'   functions such as \code{\link{flytable_query}}.
#'
#'   In order to start using flytable, you must get an API token. There doesn't
#'   seem to be a convenient way to do this from the seatable web interface but
#'   you can get one by calling \code{flytable_set_token} with your flytable
#'   user and password. This should be a once only step. Thereafter you should
#'   have a \code{FLYTABLE_TOKEN} environment variable set in your .Renviron
#'   file.
#' @description \code{flytable_login} uses your flytable user name and email to
#'   log into the service.
#' @param token normally retrieved from \code{FLYTABLE_TOKEN} environment
#'   variable.
#' @param user,pwd flytable user and password used by \code{flytable_set_token}
#'   to obtain a token
#' @param url Optional URL to the server
#'
#' @return For \code{flytable_login}, a Python
#'   \href{https://seatable.github.io/seatable-scripts/python/account/}{\code{Account}}
#'    object from the seatable api as wrapped by reticulate.
#' @export
#' @family flytable
#' @examples
#' \dontrun{
#' flytable_login()
#' }
flytable_login <- function(url='https://flytable.mrc-lmb.cam.ac.uk/',
                           token=Sys.getenv("FLYTABLE_TOKEN", unset = NA_character_)) {
  st<-check_seatable()
  if(is.na(token)) {
      stop("FLYTABLE_TOKEN environment variable unset! Please do:\n",
           "flytable_set_token(user='xxx@gmail.com', pwd='yyy')\n",
           "in order to record one.")

  } else {
    ac <- reticulate::py_call(st$Account, login_name=NULL , password = NULL,
                              server_url = url)
    ac$token=token
  }
  invisible(ac)
}

#' @description \code{flytable_set_token} will obtain and store a permanent
#'   seatable user-level API token.
#' @export
#' @rdname flytable_login
#'
#' @examples
#' \dontrun{
#' flytable_set_token(user='xxx@gmail.com', pwd='yyy')
#' }
flytable_set_token <- function(user, pwd, url='https://flytable.mrc-lmb.cam.ac.uk/') {
  st<-check_seatable()
  ac<-reticulate::py_call(st$Account, login_name=user , password = pwd,
                      server_url = url)
  ac$auth()
  # so that it is immediately available
  Sys.setenv(FLYTABLE_TOKEN=ac$token)
  # and permanentyl available
  cat("FLYTABLE_TOKEN='", ac$token, "'\n", sep="", append = TRUE,
      file = path.expand("~/.Renviron"))
  return(invisible(NULL))
}

flytable_base_impl <- memoise::memoise(function(base_name=NULL, table=NULL, url, workspace_id=NULL) {
  ac=flytable_login()
  if(is.null(base_name) && is.null(table))
    stop("you must supply one of base or table name!")
  if(is.null(base_name)) {
    base=flytable_base4table(table, ac=ac, cached=F)
    return(invisible(base))
  }

  if(is.null(workspace_id)) {
    wsdf=flytable_workspaces(ac=NULL)
    wsdf.sel=subset(wsdf, wsdf$name == base_name)
    if(nrow(wsdf.sel)==0)
      stop("Unable to find a workspace containing basename:", base_name,
           "\nCheck basename and/or access permissions.")
    if(nrow(wsdf.sel)>1)
      stop("Multiple workspaces containing basename:", base_name,
           "\nYou must use flytable_base() specifying a workspace_id to resolve this ambiguity.")
    workspace_id=wsdf.sel[['workspace_id']]
  }
  base=reticulate::py_call(ac$get_base, workspace_id = workspace_id,
                      base_name = base_name)
  base
}, cache=cachem::cache_mem(max_age = 3*24*60^2))


#' @description \code{flytable_base} returns a \code{base} object (equivalent to
#'   a mysql database) which allows you to access one or more tables, logging in
#'   to the service if necessary. The returned base object give you full access
#'   to the Python
#'   \href{https://seatable.github.io/seatable-scripts/python/base/}{\code{Base}}
#'    API allowing a range of row/column manipulations.
#' @details \code{flytable_base} will use your flytable API token to log into
#'   the service.
#' @param base_name Character vector specifying the \code{base}
#' @param table Character vector specifying a table foe which you want a
#'   \code{base} object.
#' @param workspace_id A numeric id specifying the workspace. Advanced use only
#'   since we can normally figure this out from \code{base_name}.
#' @param cached Whether to use a cached base object
#'
#' @return For \code{flytable_base}, a Python
#'   \href{https://seatable.github.io/seatable-scripts/python/base/}{\code{Base}}
#'    object from the seatable api as wrapped by reticulate.
#' @export
#' @rdname flytable_login
#' @examples
#' \dontrun{
#' hemilineages=flytable_base(base_name='hemilineages')
#' # equivalent, but simpler since you only have to remember the table name
#' hemilineages=flytable_base('fafb_hemilineages_survey')
#' }
#'
flytable_base <- function(table=NULL, base_name=NULL,
                                           workspace_id=NULL,
                                           url='https://flytable.mrc-lmb.cam.ac.uk/',
                                           cached=TRUE) {
  if (!cached)
    memoise::forget(flytable_base_impl)
  # try once with cache, if not repeat uncached
  base=try({
    flytable_base_impl(
      table = table,
      base_name = base_name,
      url = url,
      workspace_id = workspace_id
    )
  }, silent = TRUE)
  # check if we have > 1h of token validity left (normally last 72h)
  # wrap in try just in case jwt_exp not available
  stale_token <- isTRUE(try(
    difftime(base$jwt_exp, Sys.time(), units = 'hours') < 1, silent = T))
  # we had a cache failure or token is stale so retry without cache
  retry=(cached && inherits(base, 'try-error')) || stale_token
  if(!retry)
    return(base)
  memoise::forget(flytable_base_impl)
  flytable_base_impl(
    table = table,
    base_name = base_name,
    url = url,
    workspace_id = workspace_id
  )
}


#' Flytable database queries
#'
#' @details Flytable uses programmatic access to the
#'   \href{https://seatable.github.io/seatable-scripts/}{seatable} API.
#'
#' @param base Character vector naming a seatable base (recommended) or a
#'   \code{Base} object returned by \code{flytable_base} (expert use).
#' @param table The name of a table inside your database
#' @param view_name An optional view which may limit the rows/columns displayed.
#' @param order_by Optional name of columns to order results
#' @param desc Whether to use descending order (default \code{FALSE} =>
#'   ascending order)
#' @param start Optional starting row
#' @param limit Maximum number of rows to return (the default \code{Inf} implies
#'   all rows)
#' @param chunksize Optional The maximum number of rows to request in one web
#'   request. For advanced use only as the default value of \code{NULL} will
#'   fetch as many as possible.
#' @param python Whether to return a Python pandas \code{DataFrame}. The default
#'   of \code{FALSE} returns an R \code{data.frame}
#'
#' @return An R \code{data.frame} or Pandas \code{DataFrame} depending on the
#'   value of the \code{python} argument.
#' @export
#' @family flytable
#' @name flytable-queries
#' @examples
#' \donttest{
#' flytable_list_rows(table = "testfruit")
#' }
flytable_list_rows <- function(table, base=NULL, view_name = NULL, order_by = NULL,
                               desc = FALSE, start = 0L, limit = Inf,
                               python=FALSE, chunksize=NULL) {
  if(is.character(base) || is.null(base))
    base=flytable_base(base_name = base, table = table)
  ncols=length(flytable_columns(base = base, table=table)$name)
  # it looks like you can only ask for 1,000,000 cells at a time
  if(is.null(chunksize))
    chunksize=pmin(floor(1e6/ncols),50000)
  res <- if(limit>chunksize) {
    # we can only get 50k rows at a time
    start=0L
    resl=list()
    while(TRUE) {
      # fetch up to limit total number of rows or chunksize, whichever is smaller
      rowstofetch=pmin(limit-start, chunksize)
      if(rowstofetch<1) break
      tres=flytable_list_rows_chunk(base=base, table=table, view_name=view_name,
                                   order_by=order_by, desc=desc, start=start,
                                   limit=rowstofetch)
      if(nrow(tres)==0) break
      resl[[length(resl)+1]]=tres
      if(nrow(tres)<rowstofetch) break
      start=start+nrow(tres)
    }
    if(length(resl)>1 && python)
      stop("Unable to return more than 50,000 rows when python=T!")
    # bind lists
    resl=lapply(resl, reticulate::py_to_r)
    allcols=unique(sapply(resl, colnames))
    resl=lapply(resl, function(df) {
      # missing_cols=setdiff(allcols, colnames(df))
      # for(col in missing_cols) df[col]=NULL
      list_cols=which(sapply(df, is.list))
      for(i in list_cols) {
        if(all(lengths(df[[i]]) == 1))
          df[[i]]=unlist(df[[i]], use.names = F)
      }
      df
      })
    if(length(resl)>1) {
      tt=try(do.call(rbind, resl), silent = TRUE)
      if(inherits(tt, 'try-error'))
        tt=try(dplyr::bind_rows(resl), silent = F)
      if(inherits(tt, 'try-error'))
        stop("Unable to combine data.frame chunks in flytable_list_rows!")
      tt
    } else resl[[1]]
  } else {
    tres=flytable_list_rows_chunk(base=base, table=table, view_name=view_name,
                                 order_by=order_by, desc=desc, start=start,
                                 limit=limit)
    if(python) tres else reticulate::py_to_r(tres)
  }
  if(python) res else flytable2df(res, tidf = flytable_columns(table, base))
}

flytable_list_rows_chunk <- function(base, table, view_name, order_by, desc, start, limit) {
  if(!is.finite(limit)) limit=NULL
  else limit=as.integer(checkmate::assertIntegerish(limit))
  start=as.integer(checkmate::assertIntegerish(start))
  ll = reticulate::py_call(
    base$list_rows,
    table_name = table,
    view_name = view_name,
    order_by = order_by,
    desc = desc,
    start = start,
    limit = limit
  )
  pd=reticulate::import('pandas')
  pdd=reticulate::py_call(pd$DataFrame, ll)
}

#' @description \code{flytable_query} performs a SQL query against a flytable
#'   database. You can omit the \code{base} argument unless you have tables of
#'   the same name in different bases.
#' @param sql A SQL query string. See examples and
#'   \href{https://seatable.github.io/seatable-scripts/python/query/}{seatable
#'   docs}.
#' @param limit An optional limit, which only applies if you do not specify a
#'   limit directly in the \code{sql} query. By default seatable limits SQL
#'   queries to 100 rows. We increase the limit to 100000 rows by default.
#' @param convert Expert use only: Whether or not to allow the Python seatable
#'   module to process raw output from the database. This is is principally for
#'   debugging purposes. NB this imposes a requirement of seatable_api >=2.4.0.
#' @return a \code{data.frame} of results. There should be 0 rows if no rows
#'   matched query.
#' @export
#' @rdname flytable-queries
#' @seealso \code{\link{tabify_coords}} to help with copy-pasting coordinates to
#'   seatable.
#' @examples
#' \donttest{
#' flytable_query("SELECT person, fruit_name FROM testfruit WHERE person!='Bob'")
#' }
#' \dontrun{
#' flytable_query(paste("SELECT root_id, supervoxel_id FROM info limit 5"))
#' }
flytable_query <- function(sql, limit=100000L, base=NULL, python=FALSE, convert=TRUE) {
  checkmate::assert_character(sql, len=1, pattern = 'select', ignore.case = T)
  # parse SQL to find a table
  res=stringr::str_match(sql,
                         stringr::regex("\\s+FROM\\s+[']{0,1}([^, ']+).*", ignore_case = T))
  if(any(is.na(res)[,2]))
    stop("Cannot identify a table name in your sql statement!\n")
  table=res[,2]
  if(is.null(base)) {
    base=try(flytable_base(table = table))
    if(inherits(base, 'try-error'))
      stop("I inferred table_name: ", table,
           " from your SQL query but couldn't connect to a base with this table!")
  } else if(is.character(base))
    base=flytable_base(base_name = base)
  if(!isTRUE(grepl("\\s+limit\\s+\\d+", sql)) && !isFALSE(limit)) {
    if(!is.finite(limit)) limit=.Machine$integer.max
    sql=paste(sql, "LIMIT", limit)
  }
  pyout <- reticulate::py_capture_output(
    ll <- try(reticulate::py_call(base$query, sql, convert=convert), silent = T)
    )
  if(inherits(ll, 'try-error')) {
    warning(paste('No rows returned by flytable', pyout, collapse = '\n'))
    return(NULL)
  }
  pd=reticulate::import('pandas')
  reticulate::py_capture_output(pdd <- reticulate::py_call(pd$DataFrame, ll))

  if(python) pdd else {
    colinfo=flytable_columns(table, base)
    df=flytable2df(pandas2df(pdd, use_arrow = F),
                   tidf = colinfo)
    fields=sql2fields(sql)
    if(length(fields)==1 && fields=="*") {
      toorder=intersect(colinfo$name, colnames(df))
    } else {
      toorder=intersect(sql2fields(sql), colnames(df))
    }
    rest=setdiff(colnames(df),toorder)
    df[c(toorder, rest)]
  }
}

sql2fields <- function(sql) {
  fieldstring=sub("SELECT\\s+(.+)\\s+FROM\\s+.+","\\1", sql, ignore.case = T)
  if(nchar(sql)==nchar(fieldstring))
    return(character())
  fields=scan(text = fieldstring, sep = ",", what = "", quiet = T)
  fields=trimws(fields)
  fields
}

flytable_workspaces <- function(ac=NULL, cached=TRUE) {
  if(is.null(ac))
    ac=flytable_login()
  if(!cached)
    memoise::forget(flytable_workspaces_impl)
  flytable_workspaces_impl(ac)
}

flytable_workspaces_impl <- memoise::memoise(function(ac=NULL) {
  ws=ac$list_workspaces()
  wl=sapply(ws$workspace_list, "[[", "table_list", simplify = F)
  wsdf=dplyr::bind_rows(wl[lengths(wl)>0])
})

flytable_base4table <- function(table, ac=NULL, cached=TRUE) {
  tdf=flytable_alltables(ac=ac, cached = cached)
  tdf.sel=subset(tdf, tdf$name==table)
  if(nrow(tdf.sel)==0)
    stop("Unable to find table named: ", table)
  if(nrow(tdf.sel)>1)
    stop("Multiple tables named: ", table, ". Please supply base name also!")
  flytable_base(base_name = tdf.sel$base_name, workspace_id=tdf.sel$workspace_id, cached = cached)
}


#' @description \code{flytable_alltables} lists all tables across all flytables
#'   bases.
#'
#' @param ac Optional account object returned by flytables_login
#' @param cached Whether to use a cached version of the response if available.
#'   Set to \code{FALSE} if you know tables have been added or renamed during
#'   your session.
#'
#' @return A \code{data.frame} containing the \code{base_name},
#'   \code{workspace_id}, table \code{name} and table \code{_id}.
#' @export
#' @rdname flytable_login
#' @examples
#' \donttest{
#' flytable_alltables()
#' }
flytable_alltables <- function(ac=NULL, cached=TRUE) {
  wsdf=flytable_workspaces(ac=ac, cached = cached)
  if(nrow(wsdf)==0)
    return(NULL)
  wsdf$workspace_id
  if(!cached)
    memoise::forget(flytable_tables)
  ll=lapply(seq_len(nrow(wsdf)), function(i) {
    flytable_tables(workspace_id = wsdf$workspace_id[i], base_name = wsdf$name[i])
  })
  tdf=dplyr::bind_rows(ll)
  tdf
}

flytable_tables <- memoise::memoise(function(base_name, workspace_id) {
  # when we actually run this we don't want to cache
  base=flytable_base(base_name=base_name, workspace_id = workspace_id, cached=FALSE)
  md=base$get_metadata()
  ll=lapply(md$tables, function(x) as.data.frame(x[c("name", "_id")], check.names=F))
  df=dplyr::bind_rows(ll)
  df1=data.frame(base_name=base_name, workspace_id=workspace_id, stringsAsFactors = F)
  cbind(df1, df)
})


#' @description \code{flytable_columns} returns the name and type of all regular
#'   columns in a base as well as the default R type. Private columns such as
#'   \code{_id} are not included.
#'
#' @param base Optional character vector naming a seatable base (recommended) or
#'   a \code{Base} object returned by \code{\link{flytable_base}} (expert use).
#'   The default value of \code{NULL} will rely on the \code{table} so long as
#'   it is unique across the flytable server.
#' @rdname flytable_login
#' @return \code{flytable_columns} a data.frame containing columns \code{name},
#'   \code{type} and \code{rtype}
#' @export
#' @examples
#' \donttest{
#' flytable_columns("info")
#' }
flytable_columns <- function(table, base=NULL, cached=TRUE) {
  if(is.character(base) || is.null(base))
    base=flytable_base(base_name = base, table = table)
  if(!cached)
    memoise::forget(flytable_columns_memo)
  flytable_columns_memo(table, base)
}

flytable_columns_memo <- memoise::memoise(function(table, base) {
  md=base$get_metadata()
  tablenames=sapply(md$tables, '[[', 'name')
  if(!isTRUE(length(tablenames)>0)) return(NULL)
  stopifnot(table %in% tablenames)
  ti=md$tables[[which(table==tablenames)]]
  ll=lapply(ti$columns, function(x) as.data.frame(x[c("name", "type")], check.names=F))
  tidf=dplyr::bind_rows(ll)
  tidf$rtype = sapply(
    tidf$type,
    switch,
    number = 'numeric',
    checkbox = 'logical',
    date = 'POSIXct',
    mtime = 'POSIXct',
    'character'
  )
  tidf$data=lapply(ti$columns, '[[', "data")
  tidf
}, cache = cachem::cache_mem(max_age = 60^2))

#' Update or append rows in a flytable database
#'
#' @description \code{flytable_update_rows} updates existing rows in a table,
#'   returning \code{TRUE} on success.
#' @details seatable automatically maintains a unique id for each row in a
#'   \code{_id} column. This is returned by flytable_query and friends. If you
#'   modify data and then want to update again, you need to keep the column
#'   containing this row \code{_id}.
#'
#'   You do not need to provide this \code{_id} column when appending new rows.
#'   Indeed you will get a warning when doing so.
#'
#'   The \code{chunksize} argument is required because it seems that there is a
#'   maximum of 1000 rows per update action.
#'
#' @param table Character vector naming a table
#' @param df A data.frame containing the data to upload including an \code{_id}
#'   column that can identify each row in the remote table.
#' @param append_allowed Whether rows without row identifiers can be appended.
#' @param chunksize To split large requests into smaller ones with max this many
#'   rows.
#' @param ... Additional arguments passed to \code{\link[pbapply]{pbsapply}}
#'   which might include \code{cl=2} to specify a number of parallel jobs to
#'   run.
#' @inheritParams flytable_query
#'
#' @return Logical indicating success, invisibly (failures will normally cause
#'   premature termination with errors written to the console).
#' @export
#' @family flytable
#' @examples
#' \dontrun{
#' fruit=flytable_list_rows('testfruit')
#' flytable_update_rows(table='testfruit', fruit[1:2, c(1,4:6)])
#' }
flytable_update_rows <- function(df, table, base=NULL, append_allowed=TRUE,
                                 chunksize=1000L, ...) {
  if(is.character(base) || is.null(base))
    base=flytable_base(base_name = base, table = table)

  nx=nrow(df)
  if(!isTRUE(nx>0)){
    warning("No rows to update in `df`!")
    return(TRUE)
  }
  # clean up df
  df=df2flytable(df, append = ifelse(append_allowed, NA, FALSE))
  newrows=is.na(df[["row_id"]])
  if(any(newrows)){
    # we'll add these rather than updating
    flytable_append_rows(df[newrows,,drop=FALSE], table=table, base=base, chunksize = chunksize, ...)
    df=df[!newrows,,drop=FALSE]
    nx=nrow(df)
  }

  if(!isTRUE(nx>0))
    return(TRUE)

  if(nx>chunksize) {
    nchunks=ceiling(nx/chunksize)
    chunkids=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
    chunks=split(df, chunkids)
    oks=pbapply::pbsapply(chunks, flytable_update_rows, table=table, base=base, chunksize=Inf, append_allowed=FALSE, ...)
    return(all(oks))
  }

  pyl=df2updatepayload(df)
  res=base$batch_update_rows(table_name=table, rows_data=pyl)
  ok=isTRUE(all.equal(res, list(success = TRUE)))
  return(ok)
}


# private function to convert a data.frame into the format
# needed by Base.batch_update_rowskey
df2updatepayload <- function(x, via_json=FALSE) {
  if(via_json) {
    # this is faster for small inputs but *much* slower for large ones
    othercols=setdiff(colnames(x), 'row_id')
    updates=lapply(seq_len(nrow(x)), function(i) list(row_id=x[i,'row_id'], row=as.list(x[i,othercols])))
    js=toJSON(updates, auto_unbox = T)
    pyjson=reticulate::import('json')
    pyl=reticulate::py_call(pyjson$loads, js)
    return(pyl)
  }

  # convert to pandas data.frame and then on to python list
  pdf=reticulate::r_to_py(x)
  # see https://flyconnectome.slack.com/archives/CPG4GF37V/p1634491549067700?thread_ts=1634318552.063100&cid=CPG4GF37V
  pyfun=df2updatepayload_py()
  reticulate::py_call(pyfun$pdf2list, pdf)
}

# a function to return a python function!
# memoisation saves a few ms
df2updatepayload_py <- memoise::memoise(function() {
  reticulate::py_run_string(local = T, paste0(
    "import pandas\n",
    "def pdf2list(df):\n",
    "  ids = df.row_id.values\n",
    "  data = df.drop('row_id', axis=1).to_dict(orient='records')\n",
    "  payload = [{'row_id': i, 'row': d} for i, d in zip(ids, data)]\n",
    "  return payload\n"))
})


#' @description \code{flytable_append_rows} appends data to an existing table, returning \code{TRUE} on success.
#' @export
#' @rdname flytable_update_rows
#' @examples
#' \dontrun{
#' flytable_append_rows(table="testfruit",
#'   data.frame(fruitname='lemon', person='David', nid=4))
#' }
flytable_append_rows <- function(df, table, base=NULL, chunksize=1000L, ...) {
  if(is.character(base) || is.null(base))
    base=flytable_base(base_name = base, table = table)

  nx=nrow(df)
  if(!isTRUE(nx>0)){
    warning("No rows to append in `df`!")
    return(TRUE)
  }
  # clean up df
  df=df2flytable(df, append = T)
  if(nx>chunksize) {
    nchunks=ceiling(nx/chunksize)
    chunkids=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
    chunks=split(df, chunkids)
    oks=pbapply::pbsapply(chunks, flytable_append_rows, table=table, base=base, chunksize=Inf, ...)
    return(all(oks))
  }

  pyl=df2appendpayload(df)
  res=base$batch_append_rows(table_name=table, rows_data=pyl)
  ok=isTRUE(all.equal(res[['inserted_row_count']], nx))
  return(ok)
}

# private function to convert a data.frame into the format
# needed by Base.batch_update_rowskey
df2appendpayload <- function(x, ...) {
  for(col in colnames(x)) {
    # work around problems with NA values in integer pandas columns
    # if(is.integer(x[[col]]) && any(is.na(x[[col]])))
    #   x[[col]]=as.numeric(x[[col]])
    # drop empty columns - we don't need them and they can upset seatable
    if(isTRUE(all(is.na(x[[col]])))) x[[col]]=NULL
  }

  pyx=reticulate::r_to_py(x)
  pyx$to_dict('records')
}

# private function to prepare a dataframe for upload to flytable
# append=NA means you can append or add
df2flytable <- function(df, append=TRUE) {
  stopifnot(is.data.frame(df))
  if(isTRUE(append)) {
    stopifnot(is.data.frame(df))
    # check if we have a row_id column
    idcols=intersect(colnames(df), c('_id', 'row_id'))
    if(length(idcols)>0) {
      # we may get situations in which these cols are empty, in which case no probs
      # but if they have some finite values we should warn
      if(!all(is.na(df[idcols])))
        warning("Dropping _id / row_id columns. Maybe you want to update rather than append?")
      df=df[setdiff(colnames(df), c('_id', 'row_id'))]
    }
    if(any(c('_mtime', '_ctime') %in% colnames(df))) {
      warning("Dropping _mtime, _ctime columns. Maybe you want to update rather than append?")
      df=df[setdiff(colnames(df), c('_mtime', '_ctime'))]
    }
  } else {
    # for update, make sure we have a row_id column
    if('_id' %in% colnames(df))
      colnames(df)[colnames(df)=='_id']='row_id'
    if(!isTRUE("row_id" %in% colnames(df)))
      stop("Data frames for update must have a _id or row_id column")
    if(any(duplicated(df[['row_id']])))
      stop("Duplicate row _ids present!")
    df[['row_id']][!nzchar(df[['row_id']])]=NA
    if(isFALSE(append) && any(is.na(df[['row_id']])))
      stop("missing row _ids!")
  }

  int64cols=sapply(df, bit64::is.integer64)
  for(i in which(int64cols)) {
    df[[i]]=as.character(i)
  }
  listcols=sapply(df, is.list)
  for(i in which(listcols)) {
    li=lengths(df[[i]])
    if(!isTRUE(all(li==1))) {
      stop("List column :", colnames(df)[i], " cannot be vectorised!")
    }
    df[[i]]=unlist(df[[i]])
  }
  df
}

# private function to tidy up oddly formatted columns
flytable2df <- function(df, tidf=NULL) {
  if(!isTRUE(ncol(df)>0))
    return(df)
  nr=nrow(df)
  listcols=sapply(df, is.list)
  for(i in which(listcols)) {
    li=lengths(df[[i]])
    if(isTRUE(all(li==1))) {
      ul=unlist(df[[i]])
      if(!isTRUE(length(ul)==nr))
        warning("List column :", colnames(df)[i], " cannot be vectorised!")
      else df[[i]]=ul
    } else if(isTRUE(all(li %in% 0:1))) {
      df[[i]][!nzchar(df[[i]])]=NA
      df[[i]]=null2na(df[[i]])
    } else warning("List column :", colnames(df)[i], " cannot be vectorised!")
  }
  if(is.null(tidf)) df else {
    if(is.character(tidf)) tidf=flytable_columns(tidf)
    flytable_fix_coltypes(df, tidf=tidf)
  }
}

flytable_fix_coltypes <- function(df, tidf, tz='UTC') {

  # remove columns that would end up as character anyway
  # tidf=tidf[tidf$rtype!='character',,drop=F]

  coltypes=sapply(df, mode)
  # charcols=names(which(coltypes=="character"))
  # candcols=intersect(charcols, tidf$name)
  for(col in colnames(df)) {
    sttype=tidf$type[match(col, tidf$name)]
    newtype=tidf$rtype[match(col, tidf$name)]
    curtype=coltypes[col]
    if(isTRUE(curtype==newtype) || coltypes[col]=="list") next
    if(col %in% c("_mtime", "_ctime") || isTRUE(sttype=='mtime')) {
      # one of the automatic timestamp columns
      df[[col]]=flytable_parse_date(df[[col]], format = 'timestamp', tz=tz)
    } else {
      if(is.na(newtype)) next
      if(newtype=='POSIXct') {
        # get extra metadata for this column if available
        coldata=tidf$data[[match(col, tidf$name)]]
        df[[col]]=flytable_parse_date(df[[col]], colinfo = coldata, tz=tz)
      } else {
        newcol=try(as(df[[col]], newtype), silent = T)
        if(inherits(newcol, 'try-error'))
          warning("Unable to change column: ", col, " to type: ", newtype)
        else df[[col]]=newcol
      }
    }
  }
  df
}


#' @importFrom methods as
flytable_parse_date <- function(x, colinfo=NULL,
                                format=c("guess", 'ymd', 'ymdhm', 'timestamp'),
                                tz='UTC', lubridate=NA) {
  formats=c(ymdhm="%Y-%m-%d %H:%M",
            ymdhms="%Y-%m-%d %H:%M:%S",
            ymd="%Y-%m-%d",
            timestamp="%Y-%m-%dT%H:%M:%OS%z")
  format=match.arg(format)
  format <- if(format!="guess") format
  else  if(!is.null(colinfo$format)) {
    if(colinfo$format=="YYYY-MM-DD HH:mm")
      "ymdhm"
    else if(colinfo$format=="YYYY-MM-DD")
      "ymd"
    else {
      warning("Unrecognised date format:", colinfo$format)
      NA
    }
  } else {
    # inspect
    if(any(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", x, perl=T))) {
      if(any(grepl("Z", x, fixed = T))) "ymdhms"
      else if(any(grepl("T", x, fixed = T))) "timestamp"
      else if(any(grepl("[0-2][0-9]:[0-5][0-9]", x, perl = T))) "ymdhm"
      else "ymd"
    } else {
      if(all(is.na(x)|!nzchar(x))) {
        warning("cannot parse empty date column")
      }
      else {
        warning("Unrecognised date format")
      }
      NA
    }
  }
  if(is.na(format)) return(x)
  stopifnot(format %in% names(formats))

  if(format=='ymdhm') {
    # list_rows and SQL give different date formats for seatable date fields!
    # list_rows: "2021-06-23 07:01"
    # SQL: '2022-01-12T09:30:00Z' (GMT) '2021-08-05 08:30:00' (other)
    if(any(grepl("[0-2][0-9]:[0-5][0-9]:[0-6][0-9]", x, perl = T)))
      format="ymdhms"
  }
  if(format=='ymdhms') {
    # there is some strange bug in the python client for GMT datetimes
    # related to https://github.com/seatable/seatable-api-python/issues/53
    x=sub('Z', '', x, fixed = T)
    x=sub('T', ' ', x, fixed = T)
  }
  format_str=formats[format]

    if(is.na(lubridate)) {
    lubridate=requireNamespace('lubridate', quietly = TRUE)
    if(!lubridate)
      warn_hourly("Please install suggested lubridate package for faster parsing of flytable dates")
  }
  if(lubridate) {
    # lubridate is fussy about parsing and insists on character vectors
    x[is.na(x)]=NA_character_
    lubridate::fast_strptime(x, format_str, tz=tz, lt = FALSE)
  } else {
    # remove colon from timezone to keep base::strptime happy
    if(format=='timestamp')
      x=sub("([+\\-][0-2][0-9]):([0-5][0-9])$","\\1\\2",x, perl = T)
    strptime(x, format_str, tz=tz)
  }
}

#' @export
#' @rdname flytable_update_rows
#' @description \code{flytable_nrow} returns the number or rows in one or more
#'   flytable tables using a SQL \code{COUNT} query.
flytable_nrow <- function(table, base=NULL) {
  if(length(table)>1) {
    res=sapply(table, flytable_nrow, base=base)
    return(res)
  }
  res=flytable_query(paste('SELECT COUNT(_id) from', table), base=base)
  stopifnot(is.data.frame(res))
  res[[1]]
}


flytable_delete_rows <- function(ids, table, DryRun=TRUE) {
  if(is.data.frame(ids)) ids=ids[['_id']]
  ids=unique(ids)
  if(!isTRUE(length(ids)>0))
    stop("No ids to delete")
  bb=flytable_base(table = table)
  pyids=reticulate::r_to_py(as.list(ids))
  stopifnot(inherits(pyids, "python.builtin.list"))
  if(!isFALSE(DryRun)) {
    ids
  } else {
    res=bb$batch_delete_rows(table_name = table, row_ids = pyids)
    ndeleted=unlist(res)
    if(!isTRUE(ndeleted==length(ids)))
      warning("only able to delete ", ndeleted, " out of ", length(ids), " rows!")
    ndeleted
  }
}

ids2sqlin <- function(ids, quote=TRUE) {
  # ids=ngl_segments(ids)
  if(quote)
    ids=shQuote(ids)
  idstr=paste(ids, collapse = ',')
  sprintf("IN (%s)", idstr)
}

col_types <- function(col, table) {
  tidf <- flytable_columns(table = table)
  type=tidf$rtype[match(col, tidf$name)]
  type
}

#' List selected rows from flytable
#'
#' @param ids One or more identifiers
#' @param table The name of the flytable table
#' @param fields The database columns to return
#' @param idfield Which field to use as a key for lookup
#' @param ... Additional arguments passed to \code{\link{flytable_query}}
#'
#' @return a dataframe containing the selected rows / columns
#' @family flytable
#' @export
flytable_list_selected <- function(ids=NULL, table='info', fields="*", idfield="root_id", ...) {
  if(length(fields)>1) fields=paste(fields, collapse = ',')
  if(is.null(ids)) {
    sql=glue::glue('select {fields} from {table}')
  } else {
    if(idfield=="root_id")
      ids=flywire_ids(ids)
    isnumber=isTRUE(col_types(idfield, table=table)=='numeric')
    idlist=ids2sqlin(ids, quote = !isnumber)
    sql=glue::glue('select {fields} from {table} where {idfield} {idlist}')
  }

  fq=flytable_query(sql, ...)
  if(isTRUE(nrow(fq)>0) && fields=="*") {
    cols=flytable_columns(table)$name
    dplyr::select(fq, cols, dplyr::everything())
  } else fq
}

cell_types_nomemo <- function(query=NULL, timestamp=NULL,
                              target='type', table='info',
                              fields=c("root_id", "supervoxel_id", "side", "flow", "super_class", "cell_class", "cell_type", "top_nt", "ito_lee_hemilineage", "hemibrain_type", "fbbt_id")) {
  if(is.null(query))
    query="_%"

  # handle queries against multiple tables
  if(length(table)>1) {
    resl=lapply(table, function(thistable)
      cell_types_nomemo(query = query, timestamp = timestamp, target=target, table=thistable))
    resln=sapply(resl, nrow)
    resl2=resl[resln>0]
    if(length(resl2)>1)
      return(dplyr::bind_rows(resl2))
    else if(length(resl2)==0)
      return(resl[[1]]) # empty data.frame
    else
      return(resl2[[1]])
  }

  likeline=switch (target,
                   type = sprintf('((cell_type LIKE "%s") OR (hemibrain_type LIKE "%s"))',query,query),
                   all = sprintf('((cell_type LIKE "%s") OR (hemibrain_type LIKE "%s") OR (cell_class LIKE "%s") OR (super_class LIKE "%s"))',query, query, query, query),
                   sprintf('(%s LIKE "%s")',target, query)
  )
  fields=paste(fields, collapse = ',')
  cell_types=flytable_query(paste(
    'select', fields,
    'FROM ', table,
    'WHERE status NOT IN ("bad_nucleus", "duplicate", "not_a_neuron")',
    'AND', likeline)
  )
  if (!is.null(timestamp) && nrow(cell_types)>0) {
    cell_types$root_id = flywire_updateids(cell_types$root_id,
                                           svids = cell_types$supervoxel_id,
                                           timestamp = timestamp)
  }
  cell_types
}

cell_types_memo <- memoise::memoise(cell_types_nomemo, ~memoise::timeout(5*60))
# cell_types_memo <- cell_types_nomemo

#' Fetch (memoised) flywire cell type information from flytable
#'
#' @details when \code{transfer_hemibrain_type=TRUE}, \code{hemibrain_type}
#'   values will be transferred into the \code{cell_type} column if
#'   \code{cell_type} is empty.
#'
#'   It seems that SQL LIKE searches (e.g. containing the \code{\%} symbol) do
#'   not work for the \code{ito_lee_hemilineage} column. You can still search
#'   for exact matches or use full regular expression queries (which operate by
#'   downloading all rows and then filtering on your machine).
#'
#'   Static cell type information is provided by Schlegel et 2023. See
#'   \href{https://github.com/flyconnectome/flywire_annotations}{flywire_annotations}
#'   github repository. It will be used by default when connection to the
#'   pre-release Cambridge flytable is not available or when specified by
#'   \code{options(fafbseg.use_static_celltypes=TRUE)}. Note that presently only
#'   one materialisation version (630) is supported for static data.
#'
#' @param cache Whether to cache the results for 5m (default \code{TRUE} since
#'   the flytable query is is a little expensive)
#' @param version An optional CAVE materialisation version number. See
#'   \code{\link{flywire_cave_query}} for more details. Note also that the
#'   special signalling value of \code{TRUE} implies the latest locally
#'   available connectome dump.
#' @param transfer_hemibrain_type Whether to transfer the \code{hemibrain_type}
#'   column into the \code{cell_type} (default TRUE, see details)
#' @param pattern Optional character vector specifying a pattern that cell types
#'   must match in a SQL \code{LIKE} statement executed by
#'   \code{\link{flytable_query}}. The suffix \code{_L} or \code{_R} can be used
#'   to restricted to neurons annotated to the L or R hemisphere. See examples.
#' @param target A character vector specifying which flytable columns
#'   \code{pattern} should match. The special value of \code{type} means either
#'   \code{cell_type} \emph{or} \code{hemibrain_type} should match. The special
#'   value of \code{all} means to match against any of \code{cell_type,
#'   hemibrain_type, cell_class}.
#' @param table Which cell type information tables to use (\code{info} for
#'   brain, \code{optic} for optic lobes or \code{both}).
#' @param use_static Whether to use static cell type information (from Schlegel
#'   et al)
#' @inheritParams flywire_cave_query
#'
#' @return The original data.frame left joined to appropriate rows from
#'   flytable.
#' @export
#' @seealso \code{\link{add_celltype_info}}
#' @examples
#' \donttest{
#' flytable_cell_types("MBON%")
#' flytable_cell_types("MBON%", version=450)
#' # the latest connectome dump, see flywire_connectome_data_version()
#' \dontrun{
#' flytable_cell_types("MBON%", version=TRUE)
#' }
#'
#' # two characters
#' flytable_cell_types("MBON__")
#' # at least one character
#' flytable_cell_types("MBON_%")
#' # range
#' flytable_cell_types("MBON2[0-5]")
#'
#' # include side specification
#' flytable_cell_types("DA2_lPN_R")
#' # only the RHS MBON20
#' flytable_cell_types("MBON20_R")
#' # all RHS cells with class MBON
#' flytable_cell_types("MBON_R", target="cell_class")
#'
#' # anything with type *OR* class information
#' cells=flytable_cell_types(target = 'all')
#' # anything that mentions PN anywhere
#' pncands=flytable_cell_types('%PN%', target = 'all')
#' }
flytable_cell_types <- function(pattern=NULL, version=NULL, timestamp=NULL,
  target=c("type", "cell_type", 'hemibrain_type', 'cell_class', 'super_class',
           'ito_lee_hemilineage', 'all'),
  table=c("info", "optic", "both"),
  transfer_hemibrain_type=c("extra", "none", "all"),
  cache=TRUE, use_static=NA) {

  # defaults to static unless option is set or we have a flytable token
  use_static=flywire_use_static_cell_types(use_static)
  if(isTRUE(use_static)) {
    if((!is.null(version) && !version %in%c(630, 783)) || !is.null(timestamp)){
      warning("ignoring version/timestamp argument")
      version=flywire_connectome_data_version(default = 783L)
    }
  }
  target=match.arg(target)
  if(use_static && target=='all') target='type'
  table=match.arg(table, several.ok = T)
  if('both' %in% table) table=c("info", "optic")
  transfer_hemibrain_type=match.arg(transfer_hemibrain_type)
  timestamp <- if(!is.null(timestamp) && !is.null(version))
    stop("You can only supply one of timestamp and materialization version")
  else if(!is.null(version) && !isTRUE(use_static)) {
    # (nb we don't need to find timestamp if we are using static cell types)
    if(isTRUE(version)) version=flywire_connectome_data_version()
    flywire_timestamp(version, convert=T)
  }
  else NULL
  if(!cache)
    memoise::forget(cell_types_memo)
  if(is.null(pattern) && !use_static) pattern="_%"

  # side specification
  side=NULL
  if(isTRUE(grepl("_[LR]$", pattern))) {
    mres=stringr::str_match(pattern, '(.+)_([LR])$')
    pattern=mres[,2]
    side=switch(mres[,3], L='left', R="right", stop("side problem in flytable_cell_types!"))
  }

  if(use_static && isTRUE(substr(pattern,1,1)!="/")) {
    # we're going to use the static cell type information but we have a SQL like
    pattern=gsub("%", '.*?', pattern, fixed = T)
    pattern=gsub("_", '.{1}', pattern, fixed = T)
    pattern=paste0("/", target, ":^", pattern, "$")

  }
  if(isTRUE(substr(pattern,1,1)=="/")) {
    smres=stringr::str_match(pattern, '/([^:]*):(.+)')
    if(is.na(smres[,1]))
       stop("Malformed regex query:`", pattern,"`! Should look like `/<field>:<regex`")
    regex=smres[,3]
    regex_target=match.arg(smres[,2],
      c("type", "cell_type", 'hemibrain_type', 'cell_class', 'super_class',
        'ito_lee_hemilineage', 'all'))
    pattern=NULL
    target='all'
  } else regex=NULL
  ct <- if(use_static) cell_types_static(version=version)
  else cell_types_memo(pattern, timestamp=timestamp, target=target, table=table)
  if(is.null(ct))
    stop("Error running flytable query likely due to connection timeout (restart R) or syntax error.")
  if(!is.null(side)){
    ct=ct[ct$side==side,,drop=F]
    rownames(ct)=NULL
  }

  if(transfer_hemibrain_type!='none') {
    # any row with valid hemibrain_type
    toupdate=!is.na(ct$hemibrain_type) & nzchar(ct$hemibrain_type)
    # only overwrite when cell_type is invalid
    if(transfer_hemibrain_type=='extra')
      toupdate= toupdate & (is.na(ct$cell_type) | nchar(ct$cell_type)==0)
    ct$cell_type[toupdate]=ct$hemibrain_type[toupdate]
  }
  if(!is.null(regex)) {
    if(regex_target=='type')
      regex_target='cell_type'
    else if(regex_target=='all')
      stop("target='all' is not supported with regular expressions!")
    ct=ct[grepl(regex, ct[[regex_target]]), ]
    # original rownames will only confuse
    rownames(ct)=NULL
  }
  ct
}

# private function to figure out if we should be using static cell types
flywire_use_static_cell_types <- function(use_static=NA) {
  if(is.na(use_static))
    use_static=getOption('fafbseg.use_static_celltypes',
                         nchar(Sys.getenv("FLYTABLE_TOKEN"))==0)
  else {
    if(!is.logical(use_static))
      stop("use_static should be NA or T/F")
  }
  use_static
}

cell_types_static <- function(version=783L) {
  stopifnot(version %in% c(783, 630))
  sf1 <- if(version==630)
    'Supplemental_file1_annotations.tsv'
  else
    'Supplemental_file1_neuron_annotations.tsv'

  anns=flywire_sirepo_file_memo(file.path('supplemental_files', sf1),
                                read = TRUE, data.table=FALSE,
                                version=version)
  cols=c("root_id", "supervoxel_id", "side", "flow", "super_class",
         "cell_class", "cell_type", "top_nt", "ito_lee_hemilineage",
         "hemibrain_type", "fbbt_id")
  anns %>%
    select(dplyr::all_of(cols)) %>%
    mutate(across(dplyr::where(is.character), ~dplyr::na_if(., "")))
}

#' Fetch flytable cell type information to a dataframe with flywire ids
#'
#' @description \code{add_celltype_info} will add information to an existing
#'   dataframe.
#' @details the root ids must be in a column called one of \code{"pre_id",
#'   "post_id", "root_id", "post_pt_root_id", "pre_pt_root_id"}. If you do not
#'   have exactly one of these columns present then you must specify your
#'   preferred column with the \code{idcol} argument.
#'
#' @param x a data.frame containing root ids or a \code{\link{neuronlist}} ()
#' @param idcol Optional character vector specifying the column containing ids
#'   of the neurons for which cell type information should be provided.
#' @param suffix A character suffix for the new columns (default value of
#'   \code{NULL} implies no suffix).
#' @param version Optional numeric CAVE version (see \code{flywire_cave_query}).
#'   The special signalling value of \code{TRUE} uses the current default data
#'   dump as returned by \code{\link{flywire_connectome_data_version}}.
#' @param ... additional arguments passed to \code{flytable_cell_types}
#' @inheritParams flytable_cell_types
#'
#' @return a data.frame with extra columns
#' @export
#' @seealso \code{\link{flytable_cell_types}}
#' @examples
#' \donttest{
#' kcin=flywire_partner_summary("720575940626474889", partners = 'in',
#'   cleft.threshold = 50)
#' kcin
#' kcin2=add_celltype_info(kcin)
#' kcin2
#' library(dplyr)
#' kcin2 %>%
#'   group_by(cell_type) %>%
#'   summarise(wt = sum(weight),n=n()) %>%
#'   arrange(desc(wt))
#' kcin2 %>%
#'   count(cell_class, wt = weight)
#' }
#'
#' \dontrun{
#' # read neuronlist containing "dotprops" for some olfactory projection neurons
#' da2=read_l2dps('DA2')
#' # add cell type details to that
#' da2=add_celltype_info(da2)
#' }
add_celltype_info <- function(x, idcol=NULL, version=NULL, suffix=NULL,
                              table=c("both", "info", "optic"), ...) {
  if(nat::is.neuronlist(x)) {
    nl=x
    x=as.data.frame(nl)
    if(ncol(x)==0)
      x=data.frame(root_id=names(nl))
    data.frame(nl) <- add_celltype_info(x, idcol = idcol, version = version, table=table, ...)
    return(nl)
  }

  stopifnot(is.data.frame(x))
  av=attr(x, 'version')
  if(is.null(version) && !is.null(av))
    version=av
  else if(isTRUE(version))
    version=flywire_connectome_data_version()

  if(is.null(idcol)) {
    idcols=c("pre_id", "post_id", "root_id", "post_pt_root_id", "pre_pt_root_id")
    idc=idcols %in% colnames(x)
    if(!isTRUE(sum(idc)==1))
      stop("You do not have exactly one of the standard columns in your dataframe!\n",
           "Please use the idcol argument to specify your preferred column.")
    idcol=idcols[idc]
  } else {
    if(!idcol %in% names(x))
      stop("id column: ", idcol, " is not present in x!")
  }

  ct=flytable_cell_types(version=version, target = 'all', table=table, ...)
  if(!is.character(x[[idcol]])) {
    if(!bit64::is.integer64(x[[idcol]]))
      stop("Expect either character or integer64 ids!")
    ct[['root_id']]=bit64::as.integer64(ct[['root_id']])
  }
  if(!is.null(suffix)) {
    ct.othercols=colnames(ct)!='root_id'
    colnames(ct)[ct.othercols]=paste0(colnames(ct)[ct.othercols], suffix)
  }
  joinexp=structure('root_id', names=idcol)
  dplyr::left_join(x, ct, by=joinexp)
}

#' @description \code{flytable_meta} will fetch a data.frame of metadata from
#'   flytable for a given set of identifiers.
#' @param ids Flywire identifiers/query in any form understood by
#'   \code{\link{flywire_ids}}
#' @param unique Whether to ensure that rows contain only unique identifiers.
#'   Default \code{FALSE}. When \code{TRUE} duplicate rows will be returned with
#'   a warning.
#' @export
#' @rdname add_celltype_info
#' @examples
#' \donttest{
#' flytable_meta("class:MBON")
#' flytable_meta("type:MBON2%")
#' # the / introduces a regex query (small performance penalty, more flexible)
#' flytable_meta("/type:MBON2[0-5]")
#' }
flytable_meta <- function(ids=NULL, version=NULL, table=c("both", "info", "optic"), unique=FALSE, ...) {
  if(is.null(ids)) {
    df=flytable_cell_types(target = 'all', version = version, table=table, ...)
  } else {
    ids=flywire_ids(ids, version = version, ...)
    df=data.frame(root_id=ids)
    df=add_celltype_info(df, version=version, table=table, ...)
  }
  if(isTRUE(unique)) {
    dups=duplicated(df$root_id)
    ndups=sum(dups)
    if(ndups>0) {
      dupids=unique(df$root_id[dups])
      duprows=df[df$root_id %in% dupids,,drop=F]
      duprows=duprows[order(duprows$root_id),,drop=F]
      df=df[!dups,,drop=F]
      attr(df, "duprows")=duprows
      warning("Dropping ", sum(dups), " rows containing duplicate root_ids!\n",
              "You can inspect all ", nrow(duprows), " rows with duplicate ids by doing:\n",
              "attr(df, 'duprows')\n",
              "on your returned data frame (replacing df as appropriate).")
    }
  }
  df
}


#' Set the flytable cell type and other columns for a set of root ids
#'
#' @details By default \code{DryRun=TRUE} so that you can see what would happen
#'   if you apply your changes. \bold{Be careful!} Undoing changes is hard, sometimes
#'   impossible exactly. If in doubt talk to Greg, Philipp et al before making
#'   programmatic updates.
#'
#'   Since flytable always keeps root ids up to date (at least every 30m), the
#'   \code{ids} argument must be mapped onto the latest ids (using
#'   \code{\link{flywire_updateids}}) before data can be uploaded to flytable.
#'   This will be much more efficient if you provide a supervoxel id for each
#'   neuron.
#'
#' @param ids In any form understood by \code{\link{flywire_ids}}. If this a
#'   data.frame and a \code{supervoxel_id} column is also present then that will
#'   be used to ensure that any root ids are efficiently mapped to their latest
#'   version.
#' @param cell_type Character vector of cell types - can be either of length 1
#'   or of the length of ids.
#' @param hemibrain_type Will also be used to set cell_type if missing and
#'   \code{copy_hemibrain_type=TRUE}
#' @param supervoxel_id Supervoxel ids corresponding to the root \code{ids}
#'   argument.
#' @param user user initials (can be a vector of length ids)
#' @param table The name of a flytable table to update (currently we only
#'   support the info (central brain) and optic tables.
#' @param DryRun Default value (T) will return the dataframe that would be used
#'   to update
#' @param copy_hemibrain_type The recommendation is now *not* to set the
#'   cell_type from hemibrain_type if only one is provided, so default is
#'   \code{FALSE}.
#' @param ... Additional columns to update in flytable
#'
#' @export
#'
#' @seealso \code{\link{flywire_updateids}}
#' @examples
#' \dontrun{
#' flytable_set_celltype('https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/4668606109450240',
#' hemibrain_type = 'vLN25', fbbt_id='FBbt_20003784', DryRun = T)
#' }
flytable_set_celltype <- function(ids, cell_type=NULL, hemibrain_type=NULL,
                                  user='GJ',
                                  supervoxel_id=NULL,
                                  DryRun=TRUE,
                                  table=c("info", "optic"),
                                  copy_hemibrain_type=FALSE,
                                  ...) {
  table=match.arg(table)
  ids <- if(is.data.frame(ids)) {
    flywire_updateids(fafbseg::flywire_ids(ids), svids = ids$supervoxel_id)
  } else flywire_updateids(fafbseg::flywire_ids(ids), svids = supervoxel_id)
  df=flytable_list_selected(ids, fields = c('_id', "root_id", 'status'), table = table)
  df=df[!df$status %in% c("bad_nucleus", "duplicate", "not_a_neuron"),]
  if(is.null(df) || nrow(df)==0)
    stop("No rows in flytable for these ids!")
  # let's check for problems
  missing_ids=setdiff(ids, df$root_id)
  dup_ids=unique(df$root_id[duplicated(df$root_id)])
  if(length(missing_ids)>0)
    message("The following ids are missing from the info table:\n",
            paste(missing_ids, collapse = ','))
  if(length(dup_ids>0))
    message("The following ids are duplicated in the info table:\n",
            paste(dup_ids, collapse = ','))
  if(length(missing_ids)>0 || length(dup_ids)>0)
    stop("Data hygiene problem in flytable: ", table, "!")

  # reorder df to match incoming ids
  dfids=data.frame(root_id=ids)
  df=dplyr::left_join(dfids, df, by='root_id')

  df2=data.frame(...)
  if(copy_hemibrain_type && is.null(cell_type))
    cell_type=hemibrain_type
  df$cell_type=cell_type
  df$hemibrain_type=hemibrain_type

  if(!is.null(df$hemibrain_type)) df$hemibrain_type_source=user
  if(!is.null(df$cell_type)) df$cell_type_source=user
  if(isTRUE(nrow(df2)>0)){
    df=cbind(df, df2)
  }

  if(DryRun)
    df
  else {
    flytable_update_rows(df, table, append_allowed = F)
  }
}

