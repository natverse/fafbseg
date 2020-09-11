check_meshparty_reticulate <- memoise::memoise(function() {
  check_reticulate()
  tryCatch(
    reticulate::import("meshparty"),
    error = function(e) {
      stop(
        call. = F,
        "Please install python meshparty module as described at:\n",
        "https://github.com/sdorkenw/MeshParty\n",
        "I did the following:\n",
        "conda install pyembree\n",
        "pip3 install meshparty\n",
        "pip3 install annotationframeworkclient\n",
        "If you have already installed meshparty but it is not found\n",
        "then do:\nusethis::edit_r_environ()\n to point to the right python\n",
        'e.g. RETICULATE_PYTHON="/opt/miniconda3/bin/python3"'
      )
    }
  )
})

#' Skeletonize neurons using meshparty python library
#'
#' @param segments neuron ids in any form understood by
#'   \code{\link{ngl_segments}} OR paths to obj files already saved to disk.
#' @param savedir Where to save SWC files (defaults to temporary directory)
#' @param invalidation_d Distance parameter (nm) controlling skeletonisation
#'   level of detail. See meshparty docs.
#' @param ... additional arguments passed to \code{cloudvolume_save_obj}
#' @export
#' @examples
#' \dontrun{
#' meshparty_skeletonize("720575940614134045")
#' meshparty_skeletonize("720575940614134045.obj")
#' }
meshparty_skeletonize <- function(segments, savedir=NULL, invalidation_d=12000, ...) {
  if(is.null(savedir)) {
    savedir <- tempfile()
    on.exit(unlink(savedir, recursive=TRUE))
  } else {
    if(!file.exists(savedir))
      dir.create(savedir, recursive = TRUE)
  }

  if(!is.character(segments) || isFALSE(all(tools::file_ext(segments)=='obj'))) {
    # we need to fetch the segments
    segments <- ngl_segments(segments, as_character = TRUE, include_hidden = FALSE)
    # path to obj file
    if(interactive())
      message("Fetching meshes")
    segments <- cloudvolume_save_obj(segments = segments, savedir=savedir, ...)
  }

  mp <- check_meshparty_reticulate()
  tm <- mp$trimesh_io
  sk <- mp$skeletonize
  mm=tm$MeshMeta()
  use_progress=interactive()
  if(use_progress) {
    pb <- progress_bar$new(
      format = "  downloading [:bar] :current/:total eta: :eta",
      total = length(segments), clear = F, show_after = 1)
  }

  swcs=paste0(tools::file_path_sans_ext(segments), '.swc')
  if(interactive())
    message("Skeletonising meshes")
  for(f in segments) {
    if(use_progress)
      pb$tick()
    mesh=mm$mesh(filename = f)
    skel=sk$skeletonize_mesh(mesh, invalidation_d=invalidation_d)
    outf=paste0(tools::file_path_sans_ext(f), '.swc')
    skel$export_to_swc(outf)
  }
  swcs
}
