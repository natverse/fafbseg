flywire_sirepo_url <- function() {
  "https://github.com/flyconnectome/flywire_annotations"
}

flywire_sirepo_dir <- function(..., reponame='flywire_annotations', create_basedir=FALSE) {
  udd=fafbseg_userdir()
  if(create_basedir && !file.exists(udd))
    dir.create(udd, showWarnings = FALSE, recursive = T)
  repodir=file.path(udd, reponame, ...=...)
  repodir
}

flywire_sirepo_download <- function(version=c(630L,783L), ref=NULL, ...) {
  if(is.null(ref)) {
    version=version[1]
    stopifnot(version %in% c(630, 783))
    ref <- if(version==630) 'v1.1.0' else 'staging'
  }

  if(!requireNamespace('git2r'))
    stop("Please:\n  install.packages('git2r')\nin order to use this function!")
  url=flywire_sirepo_url()
  localdir = flywire_sirepo_dir(create_basedir = T)

  if(!file.exists(localdir)){
    cloneres=try(git2r::clone(url, localdir, credentials = git2r::cred_token(), ...))
    if(inherits(cloneres, 'try-error'))
      if(internet_ok())
        stop("Trouble with git clone while downloading cell type annotations!", as.character(cloneres))
      else
        stop("Trying to download cell type annotations but no internet!")
  }
  flywire_sirepo_update(localdir, branch = ref)
}

flywire_sirepo_update <- function(x, branch='main') {
  repo=try(git2r::repository(x), silent = TRUE)
  if(!inherits(repo, 'try-error'))
    git_pull_helper(repo, branch=branch)
}

git_pull_helper<-function(repo, branch='main'){
  sig=try(git2r::default_signature(repo), silent = TRUE)
  if(!internet_ok()) {
    warning("no internet: unable to check for annotation updates!")
  } else if(inherits(sig, 'try-error')){
    # just make up a user config since we only ever want to pull this repo
    git2r::config(repo, user.name="Anonymous NAT User",
                  user.email="nat@anon.org")
    git2r::fetch(repo, name='origin')
  } else {
    git2r::fetch(repo, name='origin', credentials = git2r::cred_token())
  }
  # tags can't be passed to branch arg, have to specify as tag object
  tr <- git2r::tags(repo)
  if(branch %in% names(tr)) {
    git2r::checkout(tr[[branch]])
  } else git2r::checkout(repo, branch = branch)
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
#' @param version An integer CAVE materialisation version (see
#'   \code{\link{flywire_connectome_data_version}})
#' @param ... Additional arguments passed to the function determined by the
#'   \code{read} argument (typically
#'   \code{data.table::\link[data.table]{fread}}).
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
flywire_sirepo_file <- function(p, mustWork=NA, read=FALSE, version=c(630L, 783L), ...) {
  version=version[1]
  if(!isTRUE(version%in% c(630, 783)))
    stop("I only know about versions 630 and 783!")
  rd=try(flywire_sirepo_download(version = version))
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


download_flywire_connection_files <- function(urls=NULL, version=c(630L, 783L)) {
  version=version[1]

  d=file.path(flywire_connectome_basedir(check_contents = FALSE), version)

  if(!file.exists(d))
    dir.create(d, recursive = T)

  prefixes=c(syn='syn_proof_analysis_neuropilv3_filtered_consolidated',
             pre="per_neuron_neuropilv3_filtered_count_pre",
             post='per_neuron_neuropilv3_filtered_count_post')
  if(version>=783) {
    # file names for 783 are different ...
    prefixes=sub("analysis_neuropilv3", "analysis", prefixes)
    prefixes=sub("v3","", prefixes)
  }
  ff=paste0(prefixes,"_", version, ".feather")
  names(ff)=names(prefixes)
  durls <- paste0('https://flyem.mrc-lmb.cam.ac.uk/flyconnectome/flywire_connectivity/',ff)
  names(durls)=names(ff)

  if (is.null(urls))
    urls <- durls
  else if(max(nchar(urls)<10)) {
    urls=match.arg(urls, names(durls))
    urls=durls[urls]
  }

  for(n in names(urls)) {
    fcf=try(flywire_connectome_file(n, cached = F), silent = T)
    # simple check for aborted download
    if(!inherits(fcf, 'try-error') && file.size(fcf) > 10e6)
      urls[n]=''
  }
  urls=urls[nzchar(urls)]
  if(length(urls)==0) return(invisible(NULL))
  check_flywire_principles()
  files=sapply(names(urls), flywire_connectome_file, version=version, mustWork=F)
  curl::multi_download(urls, destfiles = files)
}

#' Download FlyWire connectivity and annotations from public release
#'
#' @details Note that you must accept to abide by the flywire principles in
#'   order to use flywire data.
#'
#'   Version 630 released with the June 2023 bioRxiv manuscripts remains the
#'   default for the time being but there are significant improvements in the
#'   cell typing associated with version 783 which should be released with the
#'   Dec 2023 resubmissions of the core flywire manuscripts.
#'
#' @param which Which data to download. \code{core} gets the most used files
#'   (~300 MB). \code{all} gets some additional useful ones (~900 MB).
#' @param version Which materialisation version to use. See details.
#'
#' @return No return value - just used for its side effect of downloading files.
#'
#' @seealso \code{\link{flywire_connectome_data}},
#'   \code{\link{flywire_partner_summary2}}
#' @export
#'
#' @examples
#' \dontrun{
#' # 300 MB
#' download_flywire_release_data()
#' # 900 MB includes
#' download_flywire_release_data('all')
#' }
download_flywire_release_data <- function(which=c("core","all"), version=c(630L, 730L)) {
  version=version[1]
  if(!isTRUE(version%in% c(630, 783)))
    stop("I only know about versions 630 and 783!")
  which=match.arg(which)
  message("Checking for connectivity files to download")
  if(which=='core')
    download_flywire_connection_files('syn', version = version)
  else
    download_flywire_connection_files(version = version)
  message("Checking for annotation files to download")
  flywire_sirepo_download(version = version)
}

check_flywire_principles <- memoise::memoise(function(FLYWIRE_PRINCIPLES=Sys.getenv("FLYWIRE_PRINCIPLES", unset="NOTAGREED")) {
  if(isTRUE(FLYWIRE_PRINCIPLES=="IAGREETOTHEFLYWIREPRINCIPLES"))
    return(TRUE)

  if(!interactive())
    stop("You must be interactive mode to download flywire release data.")
  cli::cli_inform(("Are you happy to use flywire data according to the flywire principles at https://edit.flywire.ai/principles.html?"))
  ans=c("Yes I'm happy", "No", "What's that?")
  rans=sample(ans)
  utils::menu(rans)==which(rans==ans[1])
}, cache = cachem::cache_mem(max_age = 3600))
