check_reticulate <- function() {
  if (!requireNamespace('reticulate'))
    stop("Please install suggested reticulate pacakge!")
}

check_cloudvolume_reticulate <- memoise::memoise(function() {
  check_reticulate()
  tryCatch(
    cv <- reticulate::import("cloudvolume"),
    error = function(e) {
      stop(
        call. = F,
        "Please install python cloudvolume module as described at:\n",
        "https://github.com/seung-lab/cloud-volume#setup\n",
        "This should normally work:\n",
        "pip3 install cloud-volume\n\n",
        "If you have already installed cloudvolume but it is not found\n",
        "then do:\nusethis::edit_r_environ()\n to point to the right python\n",
        'e.g. RETICULATE_PYTHON="/usr/local/bin/python3"'
      )
    }
  )
  tryCatch(
    reticulate::import("DracoPy"),
    error = function(e) {
      stop(
        call. = F,
        "Please install DracoPy module as described at:\n",
        "https://github.com/seung-lab/cloud-volume#setup\n",
        "This should normally work:\n",
        "pip3 install DracoPy"
      )
    }
  )
  cv
})

cloudvolume_save_obj <- function(segments, savedir=tempfile(),
                                 OmitFailures=TRUE, Force=FALSE, ...,
                                 cloudvolume.url=getOption("fafbseg.cloudvolume.url")) {
  cv=check_cloudvolume_reticulate()
  vol = cv$CloudVolume(cloudvolume.url, use_https=TRUE, ...)

  if(!is.null(savedir) && !isFALSE(savedir)) {
    if(!file.exists(savedir)) {
      dir.create(savedir, recursive = TRUE)
    }
    owd=setwd(savedir)
    on.exit(setwd(owd))
  } else {
    savedir=getwd()
  }
  pb <- progress_bar$new(
    format = "  downloading [:bar] :current/:total eta: :eta",
    total = length(segments), clear = F, show_after = 1)

  ff=file.path(savedir, paste0(segments, '.obj'))
  names(ff)=segments
  for (seg in segments) {
    pb$tick()
    if(!Force && file.exists(ff[seg]))
      next
    if(OmitFailures) {
      t=try(vol$mesh$save(seg, file_format='obj'))
      if(inherits(t, 'try-error'))
        ff[seg]=NA_character_
    }
    else
      vol$mesh$save(seg, file_format='obj')
  }

  na.omit(ff)
}


#' Read meshes from chunked graph (graphene) server via CloudVolume
#'
#' @details You may to use this to fetch meshes from \url{https://flywire.ai}.
#'   It Uses the \href{https://github.com/seung-lab/cloud-volume}{CloudVolume}
#'   serverless Python client for reading data in
#'   \href{https://github.com/google/neuroglancer/}{Neuroglancer} compatible
#'   formats. compatible format. You will therefore need to have a working
#'   python3 install of CloudVolume.
#'
#'   Please install the Python CloudVolume module as described at:
#'   \url{https://github.com/seung-lab/cloud-volume#setup}. You must ensure that
#'   you are using python3 (implicitly or explicitly) as mesh fetching from
#'   graphene servers depends on this. This should normally work: \code{pip3
#'   install cloud-volume}. If you have already installed CloudVolume but it is
#'   not found, then I recommend editing your \code{\link{Renviron}} file to set
#'   an environment variable pointing to the correct Python. You can do this
#'   with \code{usethis::edit_r_environ()} and then setting e.g.
#'   \code{RETICULATE_PYTHON="/usr/local/bin/python3"}.
#'
#'   You will need to set up some kind of authentication in order to fetch data.
#'   See \url{https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson}
#'   for how to get a token and where to save it. You can either save a json
#'   snippet to \code{~/.cloudvolume/secrets/chunkedgraph-secret.json} or set an
#'   environment variable (\code{CHUNKEDGRAPH_SECRET="XXXX"}.
#'
#'   Finally you will also need to set an option pointing to your server. This
#'   might look something like
#'
#'   \code{options(fafbseg.cloudvolume.url='graphene://https://xxx.dynamicannotationframework.com/segmentation/xxx/xxx')}
#'
#'
#'
#'   and you can easily add this to your startup \code{\link{Rprofile}} with
#'   \code{usethis::edit_r_profile()}.
#' @param segments The segment ids to fetch (probably as a character vector)
#' @param cloudvolume.url Optional url from which to fetch meshes normally
#'   specified by the \code{fafbseg.cloudvolume.url} option.
#' @param ... Additional arguments passed to the Python CloudVolume constructor
#'   (see \url{https://github.com/seung-lab/cloud-volume} for details.
#'
#' @return A \code{rgl::shapelist3d} list containing one or more \code{mesh3d}
#'   objects named by the segment id.
#' @export
#'
#' @examples
#' \dontrun{
#' pmn1.flywire=read_cloudvolume_meshes("720575940623979522")
#' pmn1.fafb=read.neuron.catmaid(5321581)
#'
#' nclear3d()
#' plot3d(pmn1.fafb, col='red', lwd=2, WithNodes = F)
#' wire3d(pmn1.flywire)
#'
#' # you can select specific locations like so
#' library(elmr)
#' # CATMAID URL
#' open_fafb(pmn1.flywire[[1]], open=F)
#' # CATMAID coords to paste into PIN location box
#' cat(xyzmatrix(catmaid::catmaid_parse_url(open_fafb(pmn1.flywire[[1]], open=F))), sep=',')
#' # Neuroglancer coords (raw pixels not nm)
#' open_fafb_ngl(pmn1.flywire[[1]], open=F, coords.only = T)
#' }
read_cloudvolume_meshes <- function(segments, ...,
                                    cloudvolume.url=getOption("fafbseg.cloudvolume.url")){
  if(!requireNamespace('readobj'))
    stop("Please install suggested readobj package!")

  td <- tempfile()
  on.exit(unlink(td, recursive=TRUE))
  message("  downloading meshes")
  ff=cloudvolume_save_obj(segments, savedir = td, ...)
  message("  parsing downloaded meshes")
  res=sapply(ff, readobj::read.obj, convert.rgl = TRUE, simplify = FALSE)
  names(res)=tools::file_path_sans_ext(basename(ff))
  if(length(res)>0) {
    # simplify to a single shapelist wrapper
    # (rather than one per segment)
    cc=class(res[[1]])
    res=do.call(c, res)
    class(res)=cc
  }
  res
}
