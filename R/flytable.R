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

flytable_base_impl <- function(base_name=NULL, table=NULL, url, workspace_id=NULL) {
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
      stop("Unable to find a workspace containing basename:", basename,
           "\nCheck basename and/or access permissions.")
    if(nrow(wsdf.sel)>1)
      stop("Multiple workspaces containing basename:", basename,
           "\nYou must use flytable_base() specifying a workspace_id to resolve this ambiguity.")
    workspace_id=wsdf.sel[['workspace_id']]
  }
  base=reticulate::py_call(ac$get_base, workspace_id = workspace_id,
                      base_name = base_name)
  base
}


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
flytable_base <- memoise::memoise(function(table=NULL, base_name=NULL,
                                           workspace_id=NULL,
                                           url='https://flytable.mrc-lmb.cam.ac.uk/',
                                           cached=FALSE) {
  if (!cached)
    memoise::forget(flytable_base_impl)
  base = flytable_base_impl(
    table = table,
    base_name = base_name,
    url = url,
    workspace_id = workspace_id
  )
  base
})


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
                               python=FALSE) {
  if(is.character(base) || is.null(base))
    base=flytable_base(base_name = base, table = table)
  res <- if(limit>50000) {
    # we can only get 50k rows at a time
    start=0L
    resl=list()
    while(TRUE) {
      tres=flytable_list_rows_chunk(base=base, table=table, view_name=view_name,
                                   order_by=order_by, desc=desc, start=start,
                                   limit=limit)
      if(nrow(tres)==0) break
      resl[[length(resl)+1]]=tres
      if(nrow(tres)<50000) break
      start=start+nrow(tres)
    }
    if(length(resl)>1 && python)
      stop("Unable to return more than 50,000 rows when python=T!")
    # bind lists
    resl=lapply(resl, reticulate::py_to_r)
    if(length(resl)>1) do.call(rbind, resl) else resl[[1]]
  } else {
    tres=flytable_list_rows_chunk(base=base, table=table, view_name=view_name,
                                 order_by=order_by, desc=desc, start=start,
                                 limit=limit)
    if(python) tres else reticulate::py_to_r(tres)
  }
  if(python) res else flytable2df(res)
}

flytable_list_rows_chunk <- function(base, table, view_name, order_by, desc, start, limit) {
  if(!is.finite(limit)) limit=NULL
  else limit=as.integer(checkmate::assertIntegerish(limit))
  start=as.integer(checkmate::assertIntegerish(start))
  ll = base$list_rows(
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
#' @return
#' @export
#' @rdname flytable-queries
#' @examples
#' \donttest{
#' flytable_query("SELECT person, fruit_name FROM testfruit WHERE person!='Bob'")
#' }
#' \dontrun{
#' flytable_query("select FLYWIREsvid, hemibrain_match FROM fafb_hemilineages_survey WHERE hemibrain_match!='' limit 5", base="hemilineages")
#' }
flytable_query <- function(sql, limit=100000L, base=NULL, python=FALSE) {
  checkmate::assert_character(sql, len=1, pattern = 'select', ignore.case = T)
  if(is.null(base)) {
    # parse SQL to find a table
    res=stringr::str_match(sql,
                           stringr::regex("\\s+FROM\\s+[']{0,1}([^, ']+).*", ignore_case = T))
    if(any(is.na(res)[,2]))
      stop("Cannot identify a table name in your sql statement!\n",
           "Please supply the table or base argument to flytable_query to help me!")
    table=res[,2]
    base=try(flytable_base(base_name = base, table = table))
    if(inherits(base, 'try-error'))
      stop("I inferred table_name: ", table,
           " from your SQL query but couldn't connect to a base with this table!")
  } else if(is.character(base))
    base=flytable_base(base_name = base)

  if(!isTRUE(grepl("\\s+limit\\s+\\d+", sql)) && !isFALSE(limit)) {
    if(!is.finite(limit)) limit=.Machine$integer.max
    sql=paste(sql, "LIMIT", limit)
  }
  ll = try(reticulate::py_call(base$query, sql), silent = T)
  if(inherits(ll, 'try-error')) {
    warning('No rows returned by flytable')
    return(NULL)
  }
  pd=reticulate::import('pandas')
  pdd=reticulate::py_call(pd$DataFrame, ll)
  if(python) pdd else {
    df=flytable2df(pandas2df(pdd, use_arrow = F))
    toorder=intersect(sql2fields(sql), colnames(df))
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
  flytable_base(base_name = tdf.sel$base_name, workspace_id=tdf.sel$workspace_id)
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
  wsdf=flytable_workspaces(ac=ac)
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
  base=flytable_base(base_name=base_name, workspace_id = workspace_id)
  md=base$get_metadata()
  ll=lapply(md$tables, function(x) as.data.frame(x[c("name", "_id")], check.names=F))
  df=dplyr::bind_rows(ll)
  df1=data.frame(base_name=base_name, workspace_id=workspace_id, stringsAsFactors = F)
  cbind(df1, df)
})


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
#' \donttest{
#' fruit=flytable_list_rows('testfruit')
#' flytable_update_rows(table='testfruit', fruit[c(1,4:6)])
#' }
flytable_update_rows <- function(df, table, base=NULL, chunksize=1000L, ...) {
  if(is.character(base) || is.null(base))
    base=flytable_base(base_name = base, table = table)

  nx=nrow(df)
  if(!isTRUE(nx>0)){
    warning("No rows to update in `df`!")
    return(TRUE)
  }
  # clean up df
  df=df2flytable(df, append = F)

  if(nx>chunksize) {
    nchunks=ceiling(nx/chunksize)
    chunkids=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
    chunks=split(df, chunkids)
    oks=pbapply::pbsapply(chunks, flytable_update_rows, table=table, base=base, chunksize=Inf, ...)
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
  pyx=reticulate::r_to_py(x)
  pyx$to_dict('records')
}

# private function to prepare a dataframe for upload to flytable
df2flytable <- function(df, append=TRUE) {
  stopifnot(is.data.frame(df))
  if(append) {
    stopifnot(is.data.frame(df))
    # check if we have a row_id column
    if(any(c('_id', 'row_id') %in% colnames(df))) {
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
    if(any(is.na(df[['row_id']]) | !nzchar(df[['row_id']])))
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
flytable2df <- function(df) {
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
  df
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
    pyids
  } else {
    res=bb$batch_delete_rows(table_name = table, row_ids = pyids)
    ndeleted=unlist(res)
    if(!isTRUE(ndeleted==length(ids)))
      warning("only able to delete ", ndeleted, " out of ", length(ids), " rows!")
    ndeleted
  }
}
