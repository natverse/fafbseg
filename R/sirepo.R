flywire_sirepo_url <- function() {
  "https://github.com/flyconnectome/flywire_annotations"
}

flywire_sirepo_dir <- function(..., reponame='flywire_annotations', create_basedir=FALSE) {
  udd=path.expand(rappdirs::user_data_dir("rpkg-fafbseg", appauthor=NULL))
  if(create_basedir && !file.exists(udd))
    dir.create(udd, showWarnings = FALSE, recursive = T)
  repodir=file.path(udd, reponame, ...=...)
  repodir
}

flywire_sirepo_download <- function(...) {
  if(!requireNamespace('git2r'))
    stop("Please:\n  install.packages('git2r')\nin order to use this function!")
  url=flywire_sirepo_url()
  localdir = flywire_sirepo_dir(create_basedir = T)

  if(file.exists(localdir)) {
    flywire_sirepo_update(localdir)
  } else {
    git2r::clone(url, localdir, credentials = cred_token(), ...)
  }
}

flywire_sirepo_update <- function(x) {
  repo=try(git2r::repository(x), silent = TRUE)
  if(!inherits(repo, 'try-error'))
    git_pull_helper(repo)
}

git_pull_helper<-function(repo){
  sig=try(git2r::default_signature(repo), silent = TRUE)
  if(inherits(sig, 'try-error')){
    # just make up a user config since we only ever want to pull this repo
    git2r::config(repo, user.name="Anonymous NAT User",
                  user.email="nat@anon.org")
    git2r::pull(repo)
  } else {
    git2r::pull(repo, credentials = cred_token())
  }
}

#' Read or return path to FlyWire annotations manuscript supplementary file
#'
#' @details When \code{TRUE} and \code{p} is a tsv or csv file them the
#'   \code{data.table::\link{fread}} function is used in order to ensure that 64
#'   bit integers are correctly parsed. The default behaviour is to read ids as
#'   character vectors but this can be overridden (see examples).
#'
#' @param p Relative path to file within flywire_annotations repository
#' @param mustWork Whether the path must exists (default \code{NA} =>
#'   \code{TRUE} when reading the file)
#' @param read Whether to read the file. Either a logical value or a function.
#'   When \code{TRUE} and \code{p} is a tsv or csv file a default read function
#'   is used (see details).
#' @param ... Additional arguments passed to the
#'
#' @return A path or (when \code{read=TRUE} or a function) the result of reading
#'   the file (a \code{data.table} for csv/tsv files).
#' @export
#'
#' @examples
#' \dontrun{
#' annpath=flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv',
#'   read=FALSE)
#' # read in annotation file
#' anns=flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv',
#'   read = TRUE)
#' # read in annotation file with ids as 64 bit integers rather than strings
#' anns=flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv',
#'   read = TRUE, integer64="integer64")
#' # same but memoised to avoid checking github / re-reading file
#' anns=flywire_sirepo_file_memo('supplemental_files/Supplemental_file1_annotations.tsv',
#'   read = TRUE, integer64="integer64")
#' }
flywire_sirepo_file <- function(p, mustWork=NA, read=FALSE, ...) {
  rd=try(flywire_sirepo_download())
  if(inherits(rd, 'try-error'))
    message("Trouble downloading supplemental data. ")
  fullp=flywire_sirepo_dir(p)
  if(!isFALSE(read) && is.na(mustWork))
    mustWork=TRUE
  if(mustWork && !file.exists(fullp))
    stop("Path: ", fullp, " does not exist!")
  if(!isFALSE(read)) {
    if(isTRUE(read)) {
      ext=tools::file_ext(fullp)
      if(ext=='csv' || ext=='tsv')
        read=data.table::fread
      else
        stop("Please specify a `read` function for files with extension: ", ext)
    }
    withr::with_options(list("datatable.integer64"='character'),
                        read(fullp, ...))
  }
  else fullp
}

#' @description \code{flywire_sirepo_file_memo()} is a memoised version with a 5
#' minute timeout
#' @export
#' @rdname flywire_sirepo_file
flywire_sirepo_file_memo <- memoise::memoise(flywire_sirepo_file, cache = cachem::cache_mem(max_age = 5*60))
