.chunkedgraph_token <- function() {
  chunkedgraph_credentials_path = file.path(cv_secretdir(),"chunkedgraph-secret.json")
  if(file.exists(chunkedgraph_credentials_path)){
    token=jsonlite::fromJSON(chunkedgraph_credentials_path)[['token']]
  }
  else {
    token=Sys.getenv("CHUNKEDGRAPH_SECRET")
  }
  if(!nzchar(token)) {
    stop(call. = F, "Unable to find chunked graph credentials!\n",
         "Please set by doing:\n  set_chunkedgraph_token()\n",
         "For further details see:\n",
         "https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson")
  }
  token
}

.chunkedgraph_token.memo <- memoise::memoise(.chunkedgraph_token)

chunkedgraph_token <- function(cached=TRUE) {
  if(!cached) memoise::forget(.chunkedgraph_token.memo)
  .chunkedgraph_token.memo()
}


#' Record (and if necessary create) a FlyWire chunkedgraph token
#'
#' Writes a token to a standard location on disk so that it will be found by the
#' cloudvolume python package as well as the fafbseg package and used to
#' authenticate to \url{https://flywire.ai}.
#'
#' @param token Optional character vector containing your token. If missing, a
#'   new token will be requested (note that this will invalidate your previous
#'   token)
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
flywire_set_token <- function(token=NULL) {
  if(is.null(token)) {
    if(!interactive())
      stop("I can only request tokens in interactive mode!")
    resp=readline("Would you like to generate a new FlyWire chunkedgraph token in your browser [y/n]?")
    if(!isTRUE(tolower(resp)=="y")) {
      stop("OK! Next time, please pass the token to this function!")
    }
    browseURL("https://globalv1.flywire-daf.com/auth/api/v1/refresh_token")
    tok=readline("Please paste in the token and close your browser window: ")
    token=gsub('"', "", fixed = T, tok)
  }
  if(!isTRUE(nchar(token)==32)) {
    stop("Sorry. Bad token. They look like: 2f88e16c4f21bfcb290b2a8288c05bd0")
  }
  invisible(cv_write_secret(list(token=token), type="chunkedgraph"))
}

cv_secretdir <- function() {
  d=normalizePath("~/.cloudvolume/secrets/", mustWork = F)
  d
}

cv_write_secret <- function(body, type=c("chunkedgraph", "google"), force=TRUE) {
  type=match.arg(type)
  if(!is.character(body))
    body=jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE)
  secretdir=cv_secretdir()
  if(!file.exists(secretdir))
    dir.create(secretdir, recursive = TRUE)
  path=file.path(cv_secretdir(),paste0(type, "-secret.json"))
  if(!isTRUE(force)){
    if(file.exists(path))
      stop('secret file: ', path, ' already exists')
  }
  writeLines(body, path)
  path
}

