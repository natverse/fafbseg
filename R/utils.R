#' Print information about fafbseg setup including tokens and python modules
#'
#' @description Print information about your \bold{fafbseg} setup including your
#'   FlyWire/ChunkedGraph authentication tokens, Python modules and the
#'   nat.h5reg / java setup required for transforming points between EM and
#'   light level template brains.
#'
#' @param pymodules Additional python modules to check beyond the standard ones
#'   that \bold{fafbseg} knows about such as \code{cloudvolume}. When set to
#'   \code{FALSE}, this turns off the Python module report altogether.
#'
#' @export
#' @examples
#' \donttest{
#' dr_fafbseg(pymodules=FALSE)
#' }
dr_fafbseg <- function(pymodules=NULL) {
  flywire_report()
  cat("\n")
  google_report()
  cat("\n")
  res=py_report(pymodules = pymodules)
  cat("\n")
  if(requireNamespace("nat.h5reg", quietly = T) &&
     utils::packageVersion("nat.h5reg")>="0.4.1")
    nat.h5reg::dr_h5reg()
  invisible(res)
}

google_report <- function() {
  message("Google FFN1 segmentation\n----")
  zipdir=getOption("fafbseg.skelziproot")
  if(isTRUE(nzchar(zipdir))) {
    cat("FFN1 skeletons located at:\n", zipdir, "\n")
  } else {
    ui_todo(paste('Set the `fafbseg.skelziproot` option:\n',
                  "{ui_code('options(fafbseg.skelziproot=\"/path/to/zips\")')}",
                  "\nif you want to use FFN1 skeleton files!"))
  }
}

#' @importFrom usethis ui_todo ui_code
flywire_report <- function() {
  message("FlyWire\n----")
  chunkedgraph_credentials_path = file.path(cv_secretdir(),"chunkedgraph-secret.json")
  if(file.exists(chunkedgraph_credentials_path)) {
    cat("FlyWire/CloudVolume credentials available at:\n", chunkedgraph_credentials_path,"\n")
  }

  token=try(chunkedgraph_token(cached = F), silent = TRUE)

  if(inherits(token, "try-error")) {
    ui_todo(paste('No valid FlyWire token found. Set your token by doing:\n',
                  "{ui_code('flywire_set_token()')}"))
  } else{
    cat("Valid FlyWire ChunkedGraph token is set!\n")
  }

  u=check_cloudvolume_url(set = F)
  cat("\nFlywire cloudvolume URL:", u)
}

#' @importFrom usethis ui_todo ui_code
py_report <- function(pymodules=NULL) {
  message("Python\n----")
  if(!requireNamespace('reticulate', quietly = TRUE)) {

    ui_todo(paste('Install reticulate (python interface) package with:\n',
                     "{ui_code('install.packages(\"reticulate\")')}"))

    cat("reticulate: not installed\n", )
    return(invisible(FALSE))
  }


  print(reticulate::py_discover_config())
  if(isFALSE(pymodules))
    return(invisible(NULL))
  cat("\n")

  pkgs=c("cloudvolume", "DracoPy", "meshparty", "skeletor", "pykdtree",
         "pyembree", "annotationframeworkclient", "pychunkedgraph", "igneous",
         pymodules)

  pyinfo=py_module_info(pkgs)
  print(pyinfo)
  invisible(pyinfo)
}

py_module_info <- function(modules) {
  if(!requireNamespace('reticulate', quietly = TRUE)) {
    return(NULL)
  }
  modules=unique(modules)
  paths=character(length(modules))
  names(paths)=modules
  versions=character(length(modules))
  names(versions)=modules
  available=logical(length(modules))
  names(available)=modules

  for (m in modules) {
    mod=tryCatch(reticulate::import(m), error=function(e) NULL)
    available[m]=!is.null(mod)
    if(!available[m])
      next
    paths[m]=tryCatch(mod$`__path__`, error=function(e) "")
    versions[m]=tryCatch(mod$`__version__`, error=function(e) "")
  }
  df=data.frame(module=modules,
                available=available,
                version=versions,
                path=paths,
                stringsAsFactors = F)
  row.names(df)=NULL
  df
}

# parse an array of python 64 bit integer ids to bit64::integer64 or character
pyids2bit64 <- function(x, as_character=TRUE) {
  check_package_available('bit64')
  tf=tempfile()
  on.exit(unlink(tf))
  x$tofile(tf)
  fi=file.info(tf)
  if(fi$size%%8L != 0) {
    stop("Trouble parsing python int64. Binary data not a multiple of 8 bytes")
  }
  # read in as double but then set class manually
  ids=readBin(tf, what = 'double', n=fi$size/8, size = 8)
  class(ids)="integer64"
  if(as_character) ids=as.character(ids)
  ids
}

check_package_available <- function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop("Please install suggested package: ", pkg)
  }
}

# hidden, also in hemibrainr
add_field_seq <- function(x, entries, field = "bodyid", ...){
  x = nat::as.neuronlist(x)
  if(length(entries)!=length(x)){
    stop("The length of the entries to add must be the same as the length of the neuronlist, x")
  }
  nl = nat::neuronlist()
  for(i in 1:length(x)){
    y = x[[i]]
    entry = entries[i]
    y[[field]] = entry
    nl = c(nl, nat::as.neuronlist(y))
  }
  names(nl) = names(x)
  nl[,] = x[,]
  nl
}
