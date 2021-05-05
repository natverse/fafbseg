check_cloudvolume_reticulate <- memoise::memoise(function() {
  check_reticulate()
  tryCatch(
    cv <- reticulate::import("cloudvolume"),
    error = function(e) {
      stop(
        call. = F,
        "Please install the python cloudvolume package:\n",
        "This should normally work:\n",
        "simple_python('basic')\n",
        "For more details see ?simple_python or the cloud-volume docs",
        "https://github.com/seung-lab/cloud-volume#setup\n",
        "If you have already installed cloudvolume but it is not found\n",
        "then R probably can't find the relevant version of Python\n",
        "Do:\nusethis::edit_r_environ()\n to point to the right python\n",
        'e.g. RETICULATE_PYTHON="/opt/miniconda3/envs/r-reticulate/bin/python"'
      )
    }
  )
  dracopy_available("warning")
  cv
})

dracopy_available <- function(action=c("warning", "stop", "none")) {
  available=isTRUE(reticulate::py_module_available('DracoPy'))
  if(!available && action!="none") {
    FUN=match.fun(action)
    FUN(
      call. = F,
      "The DracoPy module is required to parse FlyWire meshes. ",
      "Please install as described at:\n",
      "https://github.com/seung-lab/cloud-volume#setup\n",
      "This should normally work:\n",
      "pip3 install DracoPy"
    )
  }
  available
}

#' @importFrom stats na.omit
#' @description \code{save_cloudvolume_meshes} saves meshes to disk.
#' @rdname read_cloudvolume_meshes
#' @param Force whether to overwrite a downloaded mesh of the same name
#' @param format whether to save meshes in Wavefront obj or Stanford poly
#'   format. obj is the default but ply is a simpler and more compact format.
#' @inheritParams nat::nlapply
#' @export
#' @examples
#'
#' \dontrun{
#' kcmesh=save_cloudvolume_meshes("720575940623755722", savedir=".")
#' kc=read.neurons(kcmesh)
#' }
save_cloudvolume_meshes <- function(segments, savedir=tempfile(),
                                 OmitFailures=TRUE, Force=FALSE,
                                 format=c("obj", "ply"), ...,
                                 cloudvolume.url=getOption("fafbseg.cloudvolume.url")) {
  cv=check_cloudvolume_reticulate()
  vol = cv$CloudVolume(cloudvolume.url, use_https=TRUE, ...)
  format=match.arg(format)
  if(format=='obj')
    check_package_available('readobj')

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

  ff=file.path(savedir, paste0(segments, paste0('.', format)))
  names(ff)=segments
  for (seg in segments) {
    pb$tick()
    if(!Force && file.exists(ff[seg]))
      next
    if(OmitFailures) {
      t=try(vol$mesh$save(seg, file_format=format))
      if(inherits(t, 'try-error'))
        ff[seg]=NA_character_
    }
    else
      vol$mesh$save(seg, file_format=format)
  }
  invisible(na.omit(ff))
}


#' Read meshes from chunked graph (graphene) server via CloudVolume
#'
#' @description \code{read_cloudvolume_meshes} uses
#'   \code{save_cloudvolume_meshes} internally to save meshes to disk and then
#'   reads them into memory as a \code{\link{neuronlist}}.
#'
#' @details You may to use this to fetch meshes from \url{https://flywire.ai}
#'   among other sources. You may need to select your preferred remote data
#'   source using \code{\link{choose_segmentation}} (see examples). Under the
#'   hood, it uses the
#'   \href{https://github.com/seung-lab/cloud-volume}{CloudVolume} serverless
#'   Python client for reading data in
#'   \href{https://github.com/google/neuroglancer/}{Neuroglancer} compatible
#'   formats. You will therefore need to have a working python3 install of
#'   CloudVolume.
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
#'   You will normally need to set up some kind of authentication in order to
#'   fetch data. For flywire, we recommend the function
#'   \code{\link{flywire_set_token}}. For other data sources or more details,
#'   see \url{https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson}
#'   for how to get a token and where to save it. You can either save a json
#'   snippet to \code{~/.cloudvolume/secrets/chunkedgraph-secret.json} or set an
#'   environment variable (\code{CHUNKEDGRAPH_SECRET="XXXX"}.
#'
#'   Finally you will also need to set an option pointing to your server. This
#'   is most conveniently achieved using e.g.
#'   \code{choose_segmentation('flywire31')}, which is now the default, but for
#'   sources without built-in support, you can also specify a full source URL,
#'   which might look something like
#'
#'   \code{options(fafbseg.cloudvolume.url='graphene://https://xxx.dynamicannotationframework.com/segmentation/xxx/xxx')}
#'
#'   You can easily add this to your startup \code{\link{Rprofile}} with
#'   \code{usethis::edit_r_profile()}.
#' @param segments The segment ids to fetch (probably as a character vector)
#' @param cloudvolume.url Optional url from which to fetch meshes normally
#'   specified by the \code{fafbseg.cloudvolume.url} option.
#' @param savedir Optional path to a directory in which obj format files will be
#'   stored. If not specified, a temporary directory will be created and removed
#'   at the end of the call.
#' @param ... Additional arguments passed to \code{save_cloudvolume_meshes} and
#'   then eventually to the Python CloudVolume constructor (see
#'   \url{https://github.com/seung-lab/cloud-volume} for details.
#'
#' @return A \code{rgl::shapelist3d} list containing one or more \code{mesh3d}
#'   objects named by the segment id.
#' @export
#' @seealso \code{\link{choose_segmentation}}. See \code{\link{simple_python}}
#'   for installation of the necessary Python packages.
#'
#' @examples
#' \dontrun{
#' # The very first time you access FlyWire data you need to get/store a token
#' flywire_set_token()
#'
#' # Each R session, you should choose the default segmentation you want
#' choose_segmentation('flywire31')
#' pmn1.flywire=read_cloudvolume_meshes("720575940623979522")
#' pmn1.fafb=read.neuron.catmaid(5321581)
#'
#' # Read and plot sample KCs from a FlyWire (short) URL
#' u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/6230669436911616"
#' kcs=read_cloudvolume_meshes(u)
#' kcs
#' plot3d(kcs)
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
read_cloudvolume_meshes <- function(segments, savedir=NULL, ...,
                                    cloudvolume.url=getOption("fafbseg.cloudvolume.url")){
  if(is.null(savedir)) {
    savedir <- tempfile()
    on.exit(unlink(savedir, recursive=TRUE))
  } else {
    if(!file.exists(savedir))
      dir.create(savedir, recursive = TRUE)
  }

  segments=ngl_segments(segments, as_character = TRUE, include_hidden = FALSE)

  message("  downloading meshes")
  ff=save_cloudvolume_meshes(segments, savedir = savedir, ...,
                          cloudvolume.url=cloudvolume.url)
  message("  parsing downloaded meshes")
  res=read.neurons(ff)
  res
}

#' @export
#' @importFrom nat boundingbox makeboundingbox
boundingbox.cloudvolume.lib.Bbox <- function(x, ...) {
  stopifnot(isTRUE(x$ndim==3))
  makeboundingbox(rbind(x$minpt, x$maxpt), x$size())
}

#' @export
#' @importFrom nat boundingbox makeboundingbox
boundingbox.cloudvolume.frontends.graphene.CloudVolumeGraphene <- function(x, ...) {
  bb <-
    makeboundingbox(
      rbind(x$voxel_offset * x$resolution, x$volume_size * x$resolution),
      dims = x$volume_size
    )
  bb
}
