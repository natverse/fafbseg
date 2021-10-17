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
  st=check_seatable()
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
  reticulate::py_call(st$Account, login_name=user , password = pwd,
                      server_url = url)
  ac$auth()
  # so that it is immediately available
  Sys.setenv(FLYTABLE_TOKEN=ac$token)
  # and permanentyl available
  cat("FLYTABLE_TOKEN='", ac$token, "'\n", sep="", append = TRUE,
      file = path.expand("~/.Renviron"))
  return(invisible(NULL))
}

flytable_base_impl <- function(base_name, url, workspace_id=NULL) {
  ac=flytable_login()

  if(is.null(workspace_id)) {
    ws=ac$list_workspaces()
    wl=sapply(ws$workspace_list, "[[", "table_list", simplify = F)
    wsdf=dplyr::bind_rows(wl[lengths(wl)>0])
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
  invisible(base)
}


#' @description \code{flytable_base} returns a \code{base} object (equivalent to
#'   a mysql database) which allows you to access one or more tables, logging in
#'   to the service if necessary.
#' @details  \code{flytable_base} will use your flytable user name and email to
#'   log into the service.
#' @param base_name Character vector specfying the \code{base}
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
#' hemilineages=flytable_base('hemilineages')
#' }
#'
flytable_base <- memoise::memoise(function(base_name, workspace_id=NULL, url='https://flytable.mrc-lmb.cam.ac.uk/', cached=FALSE){
  if(!cached)
    memoise::forget(flytable_base_impl)
  flytable_base_impl(base_name = base_name, url = url, workspace_id = workspace_id)
  }
)


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
#' flytable_list_rows("hemilineages", "fruit")
#' }
flytable_list_rows <- function(base, table, view_name = NULL, order_by = NULL,
                               desc = FALSE, start = NULL, limit = Inf,
                               python=FALSE) {
  if(is.character(base))
    base=flytable_base(base_name = base)
  if(!is.finite(limit)) limit=NULL
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
  if(python) pdd else reticulate::py_to_r(pdd)
}

#' @description \code{flytable_query} performs a SQL query against a flytable database.
#' @param sql A SQL query string. See examples and
#'   \href{https://seatable.github.io/seatable-scripts/python/query/}{seatable
#'   docs}.
#'
#' @return
#' @export
#' @rdname flytable-queries
#' @examples
#' \donttest{
#' flytable_query("hemilineages", "SELECT person, fruit_name FROM testfruit WHERE person!='Bob'")
#' }
#' \dontrun{
#' flytable_query("hemilineages", "select FLYWIREsvid, hemibrain_match FROM fafb_hemilineages_survey WHERE hemibrain_match!='' limit 5")
#' }
flytable_query <- function(base, sql, python=FALSE) {
  if(is.character(base))
    base=flytable_base(base_name = base)
  ll = base$query(sql)
  pd=reticulate::import('pandas')
  pdd=reticulate::py_call(pd$DataFrame, ll)
  if(python) pdd else reticulate::py_to_r(pdd)
}

flytable_base2 <- function(api_token, url) {
  base = reticulate::py_call(st$Base, api_token, url)
  base$auth()
  invisible(base)
}

