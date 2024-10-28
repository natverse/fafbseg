#' Print information about fafbseg setup including tokens and python modules
#'
#' @description Print information about your \bold{fafbseg} setup including your
#'   FlyWire/ChunkedGraph authentication tokens, Python modules and the
#'   nat.h5reg / java setup required for transforming points between EM and
#'   light level template brains.
#'
#' @param pypkgs Additional python packages to check beyond the standard ones
#'   that \bold{fafbseg} knows about such as \code{cloudvolume}. When set to
#'   \code{FALSE}, this turns off the Python package report altogether.
#'
#' @export
#' @examples
#' \dontrun{
#' dr_fafbseg(pymodules=FALSE)
#' }
dr_fafbseg <- function(pypkgs=NULL) {
  message("R packages\n----")
  cat("fafbseg package:\n")
  pp=utils::packageDescription('fafbseg')
  pp2=pp[names(pp) %in% c("Version","GithubSHA1", "Packaged")]
  class(pp2)="packageDescription"
  print(pp2)

  pva <- try(utils::packageVersion('arrow'), silent = T)
  if(inherits(pva, 'try-error')) {
    cat("Suggested R arrow package not installed!\n")
  } else {
    cat("R arrow package: ", as.character(pva), "\n")
  }
  check_reticulate()
  flywire_report()
  cat("\n")
  google_report()
  cat("\n")
  res=py_report(pymodules = pypkgs)
  cat("\n")
  message("fafbseg package options\n----")
  fop=options()[grep("^fafbseg\\.", names(options()))]
  print(utils::str(fop))

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

#' @importFrom usethis ui_todo ui_code ui_oops ui_done ui_info
flywire_report <- function() {
  message("FlyWire\n----")
  cat("Checking secrets folder for tokens from R:", cv_secretdir(), "\n")
  token=try(chunkedgraph_token(cached = F), silent = FALSE)
  token_ok=isFALSE(inherits(token, "try-error"))
  cvv=cloudvolume_version()
  if(token_ok) {
    extest=try(flywire_expandurl("https://globalv1.flywire-daf.com/nglstate/5747205470158848"), silent = T)
    if(inherits(extest, 'try-error')) {

      ui_todo(paste('FlyWire token was found by R but is not authorised. Set a new token with:\n',
                    "{ui_code('flywire_set_token()')}"))
      token_ok=FALSE
    } else
      ui_done("Valid FlyWire ChunkedGraph token is set and found by R!\n")
    if(is.na(cvv)) {
      ui_todo("Please use {ui_code('simple_python()')} to install python+cloudvolume for full access to flywire API!\n")
      try(check_cloudvolume_reticulate())
    } else {
      secrets=reticulate::import('cloudvolume.secrets')
      cvtoken=secrets$secretpath('secrets/cave-secret.json')
      cvtokenok=file.exists(cvtoken)
      if(!cvtokenok)
        ui_oops("cloudvolume cannot find a token at {cvtoken}")
      else {
        ui_done("cloudvolume found a token at {cvtoken}")
        # try using said token
        rootid_test=try(flywire_rootid("81489548781649724", method = 'cloudvolume'))
        if(inherits(rootid_test, 'try-error'))
          ui_oops("token found but python+cloudvolume access to FlyWire API is still failing!\n",
                "Please ask for help at https://groups.google.com/g/nat-user using the full output of dr_fafbseg.")
        else ui_done("Flywire API access via python+cloudvolume is working.\n")
      }
    }
    cavem=try(check_cave())
    plaincave=try(flywire_cave_client(datastack_name = NULL))
    if(inherits(plaincave, 'try-error') || inherits(cavem, 'try-error')) {
      usethis::ui_oops(
        "Error with with your python CAVEclient install! Unable to check CAVE accesss!")
    } else {
      gds=try(plaincave$info$get_datastacks())
      if(inherits(gds, 'try-error')) {
        ui_oops("unable to use cave to fetch datasets")
      } else {
        available_datastacks=sort(gds)
        ui_done("You have a working CAVE setup with access to the following datasets:\n{paste(available_datastacks, collapse = ',')}")

        if(!"flywire_fafb_production" %in% available_datastacks) {
          ui_info("You do not have access to the `flywire_fafb_production` production dataset.\n")
          if(!isTRUE(getOption("fafbseg.cave.datastack_name")=='flywire_fafb_public'))
            ui_todo(paste('Choose the public flywire dataset with\n',
                          "{ui_code('choose_segmentation(\"public\")')}"))
          cat("This may limit some fafbseg package functionality.\n")
          cli::cli_text("You can request full access at: {.url https://flywire.ai/brain_access}.")
        } else {
          ui_done("You have CAVE access to the `flywire_fafb_production` dataset")
        }

        if(!"flywire_fafb_public" %in% available_datastacks) {
          ui_oops(paste0(
            "You do not have access to the `flywire_fafb_public` production dataset.\n",
            "This is unexpected. Please ask for help including a copy of this report."
          ))
        } else {
          ui_done("You have CAVE access to the `flywire_fafb_public` dataset")
        }
      }
    }

  } else{
    ui_todo(paste('No valid FlyWire token found. Set your token by doing:\n',
                  "{ui_code('flywire_set_token()')}"))
  }
  secretdir=cv_secretdir()
  ff=dir(secretdir, pattern = '-secret\\.json$')
  if(length(ff)){
    cat("\n", length(ff), " FlyWire/CloudVolume credential files available at\n",
        cv_secretdir(),"\n", sep="")
    print(ff)
    old_cv=isTRUE(try(cvv<numeric_version('4.0')))
    if(!old_cv && token_ok && "chunkedgraph-secret.json" %in% ff) {
      ui_todo(paste0("\n`chunkedgraph-secret.json` is deprecated. Switch to `cave-secret.json`!\n",
                     "You could do this by:\n",
          sprintf("{ui_code('file.rename(\"%s\", \"%s\")')}",
                  file.path(secretdir, "chunkedgraph-secret.json"),
                  file.path(secretdir, "cave-secret.json")
          )))
    }
    if(length(ff)>1 && "cave-secret.json" %in% ff && "chunkedgraph-secret.json" %in% ff) {
      cmd=sprintf('unlink("%s")', file.path(secretdir, "chunkedgraph-secret.json"))
      ui_info(paste0('You have both "cave-secret.json" and "chunkedgraph-secret.json" token files\n',
              "We recommend deleting chunkedgraph-secret.json for example by doing:\n",
              "{ui_code({cmd})}"))
    }
  }

  u=check_cloudvolume_url(set = F)
  cat("\nFlywire cloudvolume URL:", u)
}

check_reticulate <- function(check_python=TRUE) {
  if(!requireNamespace('reticulate', quietly = TRUE)) {
    ui_todo(paste('Install reticulate (python interface) package with:\n',
                  "{ui_code('install.packages(\"reticulate\")')}"))
    cat("reticulate: not installed\n")
    return(invisible(FALSE))
  }
  if(check_python) check_python() else invisible(TRUE)
}

check_python <- function(initialize=TRUE) {
  # if python is already running, then we're fine
  if(reticulate::py_available())
    return(invisible(TRUE))

  nopythonmsg <-
    paste(
      "You do not have python set up for R (required for some fafbseg functions)\n",
      "We strongly recommend installing with\n",
      "{ui_code('simple_python()')}\n",
      "but you can also point the RETICULATE_PYTHON environment variable to a python\n",
      "installation that you manage. See the ?simple_python docs for details."
    )

  ownpythonmsg <-
    paste(
      "You have requested to use your own Python installation at:",
      Sys.getenv('RETICULATE_PYTHON'),
      "but it doesn't seem to be working! dr_fafbseg() may reveal more information.\n",
      "As an alternative, we strongly recommend installing with\n",
      "{ui_code('simple_python()')}\n",
      "See the ?simple_python docs for details."
    )

  if(!ownpythonrequested()) {
    pyfound <- try(reticulate::use_miniconda(getOption('fafbseg.condaenv'),
                                             required = T), silent = T)
    if(inherits(pyfound, 'try-error')) {
      ui_todo(nopythonmsg)
      return(invisible(FALSE))
    }
  }

  pyavail=reticulate::py_available(initialize = initialize)
  if(!initialize|| pyavail) return(invisible(TRUE))
  if(ownpythonrequested()) {
    ui_todo(nopythonmsg)
  } else {
    ui_todo(nopythonmsg)
  }
  return(invisible(FALSE))
}

#' @importFrom usethis ui_todo ui_code
py_report <- function(pymodules=NULL, silent=FALSE) {
  check_reticulate()
  if(!silent) {
    message("Python\n----")
    print(reticulate::py_discover_config())
  }

  if(ownpythonrequested())
    message("You are using your own python specified by:",
            Sys.getenv('RETICULATE_PYTHON'),
            "\nI hope you know what you're doing. ")

  if(isFALSE(pymodules))
    return(invisible(NULL))
  if(!silent)
    cat("\n")

  core_pkgs=c("cloud-volume", "caveclient", "seatable-api")
  pkgs=c(core_pkgs, "pyarrow", 'fafbseg',
         "DracoPy", "meshparty", "skeletor", "pykdtree",
         "pyembree", "pychunkedgraph", "igneous",
         'fastremap', 'ncollpyde',
         pymodules)
  plp=reticulate::py_list_packages()
  plps=subset(plp, plp$package %in% pkgs)
  plps$installed=TRUE
  missing_pkgs=setdiff(pkgs, plps$package)
  extra_rows=seq_along(missing_pkgs)+nrow(plps)
  plps[extra_rows,]=NA
  plps$package[extra_rows]=missing_pkgs
  plps$core=plps$package%in% core_pkgs
  plps=plps[match(pkgs, plps$package),]
  plps$installed[is.na(plps$installed)]=FALSE
  rownames(plps)=NULL

  if(!silent && any(core_pkgs %in% missing_pkgs))
    message("Some core python packages are missing. Installation using ",
            "simple_python()\n is recommended.")

  if(!silent)
    print(plps)
  invisible(plps)
}

module_version <- memoise::memoise(function(module) {
  pmi=try(py_module_info(module), silent = TRUE)
  if(inherits(pmi, 'try-error') || is.null(pmi) || nrow(pmi)<0)
    return(NA_character_)
  v=pmi$version
  if(nzchar(v)) v else NA_character_
})

cloudvolume_version <- function() module_version("cloudvolume")
cloudvolume_secret_path <- function() {
  tryCatch({
    secrets=reticulate::import('cloudvolume.secrets')
    secrets$secretpath('secrets/cave-secret.json')
  },
  error=function(e) NA)
}

pyarrow_version <- function() module_version("pyarrow")

pandas_version <- function() module_version("pandas")

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
    paths[m]=python_module_path(mod)
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

python_module_path <- function(mod) {
  tryCatch({
    path=mod$`__path__`
    if(!is.character(path)) {
      # "_NamespacePath(['/Users/paulbrooks/igneous', ''])"
      path=as.character(path)
      path2=sub(".+?\\[(.+)\\].+?", "\\1",path)
      path <- scan(what="", sep = ",", text = path2, quiet = T)
    } else path
  }, error=function(e) "")
}

# parse an array of python 64 bit integer ids to bit64::integer64 or character
pyids2bit64 <- function(x, as_character=TRUE) {
  np=py_np()
  if(inherits(x, 'python.builtin.list') || inherits(x, 'python.builtin.int') ) {
    x=np$asarray(x, dtype='i8')
  }

  if(isTRUE(x$size==0L))
    return(if(as_character) character() else bit64::integer64())

  if(isFALSE(as.character(x$dtype)=='int64')) {
    if(isFALSE(as.character(x$dtype)=='uint64'))
      stop("I only accept dtype=int64 or uint64 numpy arrays!")
    # we have uint64 input, check that itcan be represented as int64
    max=np$amax(x)
    # convert to string (in python)
    strmax=reticulate::py_str(max)
    maxint64="9223372036854775807"
    # the hallmark of overflow is that character vectors > maxint64 -> maxint64
    if(strmax!=maxint64 && as.integer64(strmax)==maxint64)
      stop("int64 overflow! uint64 id cannot be represented as int64")
  }

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

py_np <- memoise::memoise(function(convert = FALSE) {
  np=reticulate::import('numpy', as='np', convert = convert)
  np
})

# convert R ids (which may be integer64/character/int/numeric) to
# a list of python ints or a numpy array via integer64
rids2pyint <- function(x, numpyarray=F, usefile=NA) {
  check_package_available('reticulate')
  np=py_np(convert=FALSE)
  npa <- if(inherits(x, "np.ndarray")) x
  else if(!isTRUE(usefile) && (length(x)<1e4 || isFALSE(usefile))) {
    ids=as.character(x)
    str=if(length(ids)==1) ids else paste0(ids, collapse=",")
    np$fromstring(str, dtype='i8', sep = ",")
  } else {
    x=as.integer64(x)
    tf <- tempfile(fileext = '.bin')
    on.exit(unlink(tf))
    writeBin(unclass(x), tf, size = 8L)
    np$fromfile(tf, dtype = "i8")
  }
  if(isTRUE(numpyarray)) npa else reticulate::py_call(npa$tolist)
}

# convert 64 bit integer ids to raw bytes
# assume that this should be little endian for flywire servers
# set to current platform when null
rids2raw <-function(ids, endian="little", ...) {
  if(is.null(endian)) endian=.Platform$endian
  ids=as.integer64(ids)
  rc=rawConnection(raw(0), "wb")
  on.exit(close(rc))
  writeBin(unclass(ids), rc, size = 8L, endian=endian, ...)
  rawConnectionValue(rc)
}

check_package_available <- function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop("Please install suggested package: ", pkg)
  }
}

# hidden, also in hemibrainr
add_field_seq <- function(x, entries, field = "bodyid"){
  x = nat::as.neuronlist(x)
  if(length(entries)!=length(x)){
    stop("The length of the entries to add must be the same as the length of the neuronlist, x")
  }
  nl = nat::neuronlist()
  for(i in 1:length(x)){
    y = x[[i]]
    entry = entries[i]
    y[[field]] = entry
    y = nat::as.neuronlist(y)
    names(y) = entry
    nl = nat::union(nl, y)
  }
  nl[,] = x[,]
  nl
}

# hidden
nullToZero <- function(x, fill = 0) {
  if(is.list(x)){
    x[sapply(x, is.null)] <- fill
  }else{
    x = sapply(x, function(y) ifelse(is.null(y)||!length(y), fill, y))
    if(!length(x)){
      x = fill
    }
  }
  x
}

#' Simple Python installation for use with R/fafbseg/FlyWire
#'
#' @description Installs Python via an isolated miniconda environment as well as
#'   recommended packages for fafbseg. If you absolutely do not want to use
#'   miniconda (it is much simpler to get started) please read the Details
#'   section.
#'
#' @details The recommended Python install procedure installs a miniconda Python
#'   distribution. This will not be added to your system \code{PATH} by default
#'   and can be used exclusively by R. If you do not want to use miniconda, then
#'   you should at least a) make a Python virtual environment using virtualenv
#'   (or conda if you are managing your own conda install) and b) specify which
#'   Python you want to use with the \code{RETICULATE_PYTHON} environment
#'   variable. You can set \code{RETICULATE_PYTHON} with
#'   \code{usethis::edit_r_environ()}.
#'
#'   If you decided to stick with miniconda as recommended, some customisation
#'   is still possible. As a halfway house, you can set
#'   \code{options('fafbseg.condaenv')} to specify a non-standard miniconda
#'   virtual environment as an alternative to the default \code{"r-reticulate"}.
#'   Furthermore you can set an environment variable
#'   \code{RETICULATE_MINICONDA_PYTHON_VERSION=3.10} to use a newer version of
#'   Python than the reticulate package recommends.
#'
#'   If this sounds complicated, we strongly suggest sticking to the default
#'   \code{miniconda=TRUE} approach.
#'
#'   Note that that after installing miniconda Python for the first time or
#'   updating your miniconda install, you will likely be asked to restart R.
#'   This is because you cannot restart the Python interpreter linked to an R
#'   session. Therefore if Python was already running in this session, you must
#'   restart R to use your new Python install.
#'
#' @param pyinstall Whether to do a \code{"basic"} install (enough for most
#'   functionality) a \code{"full"} install, which includes tools for accessing
#'   fast/simple skeletons and "dotprops" for NBLAST via the \code{fafbseg-py}
#'   package; \code{"extra"} installs neuron packages that enable high
#'   resolution skeletonisation that are really not necessary for most users and
#'   frankly a pain to install. \code{"cleanenv"} will show you how to clean up
#'   your Python environment removing all packages. \code{"blast"} will show you
#'   how to completely remove your dedicated miniconda installation. Choosing
#'   what="none" skips update/install of Python and recommended packages only
#'   installing extras defined by \code{pkgs}.
#' @param miniconda Whether to use the reticulate package's default approach of
#'   a dedicated python for R based on miniconda (recommended, the default) or
#'   to allow the specification of a different system installed Python via the
#'   \code{RETICULATE_PYTHON} environment variable.
#' @param pkgs Additional python packages to install.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # just the basics
#' simple_python("basic")
#' # if you want to skeletonise meshes
#' simple_python("full")
#'
#' # To install a special package using the recommended approach
#' simple_python(pkgs="PyChunkedGraph")
#' # the same but without touching Python itself or the recommended packages
#' simple_python('none', pkgs='PyChunkedGraph')
#'
#' # install a specific version of cloud-volume package
#' simple_python('none', pkgs='cloud-volume~=3.8.0')
#' # if you really need to insist (e.g. because a newer version is already installed)
#' reticulate::py_install('cloud-volume==8.15.0', pip = TRUE)
#'
#' # install the latest version of a package from github
#' simple_python('none', pkgs="git+git://github.com/schlegelp/skeletor@master")
#'
#' # install a specific earlier version of a package
#' simple_python('none', pkgs="git+git://github.com/seung-lab/DracoPy@v0.0.15")
#'
#' # install all recommended packages but use your existing Python
#' # only do this if you know what you are doing ...
#' simple_python("full", miniconda=FALSE)
#' }
simple_python <- function(pyinstall=c("basic", "full", "extra", "cleanenv",
                                      "blast","none"),
                          pkgs=NULL, miniconda=TRUE) {

  check_reticulate(check_python = F)
  check_python(initialize = F)
  ourpip <- function(...)
    reticulate::py_install(..., pip = T, pip_options='--upgrade --prefer-binary')

  # since we may well change installed modules
  memoise::forget(module_version)
  pyinstall=match.arg(pyinstall)
  if(pyinstall!="none")
    pyinstalled=simple_python_base(pyinstall, miniconda)
  if(pyinstall %in% c("cleanenv", "blast")) return(invisible(NULL))

  if(pyinstall %in% c("basic", "full", "extra")) {
    message("Installing cloudvolume")
    ourpip('cloud-volume')
    message("Install seatable_api (access flytable metadata service)")
    # 2.6.3 had a problem, see
    # https://github.com/seatable/seatable-api-python/issues/76
    ourpip('seatable_api!=2.6.3')
    message("Install CAVEclient (access to extended FlyWire/FANC APIs)")
    ourpip('caveclient')
  }
  if(pyinstall %in% c("full", "extra")) {
    message("Install navis+fafbseg (python access to FlyWire/FANC data)")
    ourpip('fafbseg')
  }
  if(pyinstall%in%c("extra")) {
    message("Installing skeletor (Philipp Schlegel mesh skeletonisation)")
    ourpip('skeletor')
    message("Installing skeletor addons (for faster skeletonisation)")
    ourpip(c('fastremap', 'ncollpyde'))
    message("Installing meshparty (includes Seung lab mesh skeletonisation)")
    ourpip('meshparty')
    message("Installing pyembree package (so meshparty can give skeletons radius estimates)")
    # not sure this wlll always work, but definitely optional
    tryCatch(reticulate::conda_install(packages = 'pyembree'),
             error=function(e) warning(e))
  }
  if(!is.null(pkgs)) {
    message("Installing user-specified packages")
    ourpip(pkgs)
  }
}

# private python/conda related utility functions
#####

simple_python_base <- function(what, miniconda) {
  if(what=="cleanenv") {
    checkownpython(miniconda)
    e <- default_pyenv()
    message(
      "If you really want to clean the packages in your existing miniconda for R virtual env at:\n  ",
      e,
      "\ndo:\n",
      sprintf("  reticulate::conda_remove('%s')", e)
    )
    return(invisible(NULL))
  } else if(what=='blast') {
    checkownpython(miniconda)
    message(
      "If you really want to blast your whole existing miniconda for R install at:\n  ",
      reticulate::miniconda_path(),
      "\ndo:\n",
      "  unlink(reticulate::miniconda_path(), recursive = TRUE)\n\n",
      "**Don't do this without verifying that the path above correctly identifies your installation!**"
    )
    return(invisible(NULL))
  }

  py_was_running <- reticulate::py_available()
  original_python <- current_python()

  pychanged=FALSE
  if(miniconda) {
    if(nzchar(Sys.getenv("RETICULATE_PYTHON")))
      stop(call. = F, "You have chosen a specific Python via the RETICULATE_PYTHON environment variable.\n",
           "simple_python does not recommend this and suggests that you unset this environment variable, e.g. by doing:\n",
           "usethis::edit_r_environ()",
           "However if you are sure you want to use another Python then do:\n",
           "simple_python(miniconda=FALSE)")

    message("Installing/updating a dedicated miniconda python environment for R")
    tryCatch({
      reticulate::install_miniconda()
      pychanged = TRUE
    },
    error = function(e) {
      if (grepl("already installed", as.character(e)))
        pychanged = update_miniconda_base()
    })
    condaenv=getOption("fafbseg.condaenv")
    if(nzchar(condaenv) && condaenv!='r-reticulate')
      reticulate::conda_create(envname = condaenv,
                               conda = reticulate::miniconda_path())
    if(py_was_running && pychanged) {
      stop(call. = F, "You have just updated your version of Python on disk.\n",
           "  But there was already a different Python version attached to this R session.\n",
           "  **Restart R** and run `simple_python` again to use your new Python!")
    }
    reticulate::use_miniconda(getOption("fafbseg.condaenv"))
  } else {
    message("Using the following existing python install. I hope you know what you're doing!")
    print(reticulate::py_config())
    if (!nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
      warning(call. = F,
        "When using a non-standard Python setup, we recommend that you tell R\n",
        "  exactly which non-standard Python install to use\n",
        "  by setting the RETICULATE_PYTHON environment variable. You can do this with:\n",
        "usethis::edit_r_environ()\n",
        "  and adding a line to your .Renviron file like:\n",
        'RETICULATE_PYTHON="/opt/miniconda3/envs/r-reticulate/bin/python"'
      )
    }
  }
  pychanged
}

checkownpython <- function(miniconda) {
  if(ownpythonrequested() || !miniconda)
    stop("You have specified a non-standard Python. Sorry you're on your own!")
}

ownpythonrequested=function() {
  nzchar(Sys.getenv("RETICULATE_PYTHON"))
}

current_python <- function() {
  conf=reticulate::py_discover_config()
  structure(file.mtime(conf$python), .Names=conf$python)
}

default_pyenv <- function() {
  conf=reticulate::py_discover_config()
  conf$pythonhome
  env=sub(":.*", "", conf$pythonhome)
  env
}

# my own update function so I that can check if it actually updated anything
update_miniconda_base <- function() {
  path=reticulate::miniconda_path()
  exe <- if (identical(.Platform$OS.type, "windows"))
    "condabin/conda.bat"
  else "bin/conda"
  conda=file.path(path, exe)

  res=system2(conda, c("update", "--yes", "--json","--name", "base", "conda"), stdout = T)
  if(!jsonlite::validate(res)) {
    print(res)
    stop("Unable to parse results of conda update")
  }

  js=jsonlite::fromJSON(res)
  # true when updated
  return(length(js$actions)>0)
}


# convert a pandas dataframe into an R dataframe using arrow
# this looks after int64 properly
pandas2df <- function(x, use_arrow=TRUE) {
  checkmate::check_class(x, 'pandas.core.frame.DataFrame')
  if(!use_arrow || nrow(x)==0) {
    df=reticulate::py_to_r(x)
    return(if(use_arrow) dplyr::as_tibble(df) else df)
  }
  # remove index to keep arrow happy
  x$reset_index(drop=T, inplace=T)
  if(FALSE) {
    # in future we might prefer this, but for now let's just leave it latent
    pa=reticulate::import('pyarrow')
    at=pa$Table$from_pandas(x)
    return(as.data.frame(at))
  }

  tf=tempfile(fileext = '.feather')
  on.exit(unlink(tf))
  if(isTRUE(pyarrow_version()>='0.17.0') && pandas_version()>='1.1.0' ) {
    comp=ifelse(arrow::codec_is_available('lz4'), 'lz4', 'uncompressed')
    x$to_feather(tf, compression=comp)
  } else x$to_feather(tf)
  arrow::read_feather(tf)
}


#' Convert coordinates to tab separated values. Useful for copy/paste to
#' Seatable
#'
#' @description \code{tabify_coords} can be used to convert a comma separated
#'   value on the clipboard e.g. from neuroglancer to tab separated values
#'   needed by Seatable when coordinates are stored in 3 separate columns. It
#'   also words for multiple coordinates.
#'
#' @param xyz 3D Coordinates in any form compatible with
#'   \code{\link{xyzmatrix}}. When missing these are read from the clipboard.
#' @param FUN a transformer function to apply to the incoming coordinates. As a
#'   convenience if \code{FUN} is missing and \code{xyz} is a function then that
#'   function will be applied to coordinates on the clipboard.
#' @param write_clip Whether to write the result to the clipboard. When missing
#'   (the default) will write to clipboard only if coordinates were read from
#'   the clipboard because \code{xyz=NULL}.
#'
#' @return Character vector tab separated coordinates. When \code{xyz} is
#'   missing these will be returned invisibly and also written to the clipboard.
#' @export
#'
#' @examples
#' \donttest{
#' tabify_coords(1:3)
#' }
#' \dontrun{
#' # copy position from clipboard and write back as TSV
#' tabify_coords()
#' # same but convert from raw coordinates to nm
#' tabify_coords(flywire_raw2nm)
#' }
tabify_coords <- function(xyz=NULL, FUN=NULL, write_clip=NULL) {
  if(!is.null(xyz) && is.null(FUN) && is.function(xyz)) {
    FUN=xyz
    xyz=NULL
  }
  if(is.null(xyz)) {
    if(is.null(write_clip)) write_clip=T
    xyz=clipr::read_clip()
  }
  xyz=xyzmatrix(xyz)
  if(!is.null(FUN)) xyz=FUN(xyz)
  res=xyzmatrix2str(xyz, sep = "\t")
  if(isTRUE(write_clip)) {
    clipr::write_clip(res)
    invisible(res)
  } else res
}


fafbseg_userdir <- function(..., os=NULL) {
  fudd=path.expand(
    rappdirs::user_data_dir(file.path("R", "fafbseg"), appauthor = NULL, os=os))
  apl=pairlist(...)
  if(length(apl)>0) file.path(fudd, ...) else fudd
}


# faster than httr::http_error
internet_ok <- function() {
  !is.null(curl::nslookup('google.com', error = FALSE))
}
