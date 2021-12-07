.chunkedgraph_token <- function(domain=NULL) {
  poss_files = c(
    paste0(domain, "-cave-secret.json"),
    "cave-secret.json",
    "chunkedgraph-secret.json"
  )
  chunkedgraph_credentials_paths = file.path(cv_secretdir(), poss_files)
  token <- NULL
  for(p in chunkedgraph_credentials_paths) {
    if(file.exists(p)){
      token=jsonlite::fromJSON(p)[['token']]
      break
    }
  }
  if(is.null(token)) {
    token=Sys.getenv("CHUNKEDGRAPH_SECRET")
    if(nzchar(token)) {
      warning("Setting tokens by environment variable is deprecated!\n",
              "I will write this one to disk. ",
              "See ?flywire_set_token for the recommended approach!")
      # write to disk since cloudvolume (now) expects this
      flywire_set_token(token)
    }
  }
  if(!nzchar(token)) {
    stop(call. = F, "Unable to find chunked graph credentials!\n",
         "Please set by doing:\n  flywire_set_token()/fanc_set_token() etc \n",
         "For further details see:\n",
         "https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson")
  }
  token
}

.chunkedgraph_token.memo <- memoise::memoise(.chunkedgraph_token)


#' @rdname flywire_set_token
#' @description \code{chunkedgraph_token} returns the current chunked graph
#'   token, optionally for a specified URL / domain name.
#' @param url A URL or domain name which defines the scope of the token
#' @param cached Whether to use a cached version of the token (default yes for
#'   speed, but set to FALSE to reload after writing a new token to disk).
#'
#' @return character vector containing the token (typically 32-44 bytes)
#' @export
chunkedgraph_token <- function(url=NULL, cached=TRUE) {
  domain <- if(!is.null(url)) {
    pu=httr::parse_url(url)
    # this copes with the possibility that we were just given a domain name
    if(!is.null(pu$scheme)) pu$hostname else url
  } else NULL
  if(!cached) memoise::forget(.chunkedgraph_token.memo)
  .chunkedgraph_token.memo(domain=domain)
}


#' Record (and if necessary create) a FlyWire chunkedgraph token
#'
#' Writes a token to a standard location on disk so that it will be found by the
#' cloudvolume python package as well as the fafbseg package and used to
#' authenticate to \url{https://flywire.ai}.
#'
#' Since cloudvolume 3.10.0 April 2021, the recommended token filenames look like
#' \itemize{
#'
#' \item \code{'cave-secret.json'}
#'
#' \item \code{'cave-secret.json'}
#'
#' }
#'
#' @param token Optional character vector containing your token. If missing, a
#'   new token will be requested (note that this will invalidate your previous
#'   token)
#' @param domain Optional fully qualified domain name, which will be prepended
#'   to the filename in which the token will be stored.
#'
#' @return The path to the file storing the token (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Will open browser to get new token
#' flywire_set_token()
#' # Writes a known token to correct location
#' flywire_set_token("2f88e16c4f21bfcb290b2a8288c05bd0")
#' }
flywire_set_token <- function(token=NULL, domain=NULL) {
  zetta=isTRUE(grepl("zetta.ai", domain, fixed = T))

  if(is.null(token)) {
    if(!interactive())
      stop("I can only request tokens in interactive mode!")
    resp=readline("Would you like to generate a new chunkedgraph token in your browser [y/n]?")
    if(!isTRUE(tolower(resp)=="y")) {
      stop("OK! Next time, please pass the token to this function!")
    }
    u <- if(zetta)
      "https://api.zetta.ai/auth/google/login"
    else "https://globalv1.flywire-daf.com/auth/api/v1/refresh_token"
    browseURL(u)
    tok=readline("Please paste in the token and close your browser window: ")
    token=gsub('"', "", fixed = T, tok)
  }
  if(zetta) {
    if(!isTRUE(nchar(token)==44)) {
      if(isTRUE(nchar(token)==43)) {
        token=paste0(token, "=")
        warning("I've added an = which seemed to be missing from the end of your token!")
      } else stop("Sorry. Bad token. Zetta tokens look like:",
           " MEx0YJmZM0pEMWkNLJ4l0MEbSz1cVQtYERRhgeVRMm1=",
           "\n")
    }
  } else if(!isTRUE(nchar(token)==32)) {
    stop("Sorry. Bad token. They should look like: 2f88e16c4f21bfcb290b2a8288c05bd0")
  }
  cvv=cloudvolume_version()
  if(is.na(cvv) || cvv < numeric_version('3.11'))
    warning("You will need to install cloudvolume >=3.11.0 to use your token!\n",
            "You can do this conveniently with `fafbseg::simple_python()`")

  invisible(cv_write_secret(list(token=token), fqdn=domain, type="cave"))
}

cv_secretdir <- function() {
  d=normalizePath("~/.cloudvolume/secrets/", mustWork = F,  winslash = "/")
  d
}

cv_write_secret <- function(body, fqdn=NULL, type=c("cave", "chunkedgraph",
                                                    "google"), force=TRUE) {
  type=match.arg(type)
  if(!is.null(fqdn)) {
    ok=isTRUE(grepl('^[a-z0-9]+(\\.[a-z0-9]+){1,4}$', fqdn))
    if(!ok)
      stop(fqdn, " does not appear to be a fully qualified domain name.")
    type=paste0(fqdn, '-cave')
  }
  if(!is.character(body))
    body=jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE)
  secretdir=cv_secretdir()
  if(!file.exists(secretdir))
    dir.create(secretdir, recursive = TRUE)
  filename = paste0(type, "-secret.json")
  path=file.path(cv_secretdir(), filename)
  if(!isTRUE(force)){
    if(file.exists(path))
      stop('secret file: ', path, ' already exists')
  }
  writeLines(body, path)
  path
}

