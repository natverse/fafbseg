# this is very much still WIP
read_graphene_meshes <- function(segment,
                                 cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                                 dracodecode=c("auto", "dracor", "DracoPy")) {
  baseurl=sub(".*(https://[^/]+)/.*", "\\1", cloudvolume.url)
  manifesturl=sprintf("%s/meshing/1.0/fly_v31/manifest/%s:0?verify=True",
                      baseurl,
                      as.character(segment))
  manifest=flywire_fetch(manifesturl)
  if(!length(manifest$fragments))
    stop("No fragments to fetch!")
  manifest$fragments

  info=flywire_fetch(
    paste0(baseurl, "/segmentation/1.0/fly_v31/info"), cache = TRUE)

  if(!isTRUE(substr(info$data_dir, 1, 5)=="gs://"))
    stop("Cannot parse data directory for meshes!\n", info$data_dir)
  dd=sub("gs://", "https://storage.googleapis.com/", fixed=T,info$data_dir)
  basemeshurl=file.path(dd,info$mesh)

  l=list()
  pb <- progress_bar$new(
    format = "  downloading :what [:bar] :percent eta: :eta",
    total = length(manifest$fragments))

  for(frag in manifest$fragments) {
    pb$tick()
    meshurl=file.path(basemeshurl, frag)
    res=httr::GET(meshurl)
    httr::stop_for_status(res)
    l[[frag]]=httr::content(res, as = 'raw')
  }

  # convert raw data (usually multiple fragments) into mesh3d objects
  ml=lapply(l, draco2mesh3d, method=dracodecode)
  # make a single mesh object from those fragmentary mesh3d objects
  m=simplify_meshlist(ml)
  m
}

#' @importFrom rgl tmesh3d
draco2mesh3d <- function(m, method=c("auto", "dracor", "DracoPy"), ...) {
  method=match.arg(method)
  if(method=='auto'){
    method=if(requireNamespace('dracor', quietly = TRUE))
      "dracor"
    else {
      if(dracopy_available(action = 'none')) "DracoPy"
      else stop("Neither dracor R package or DracoPy python library are available!")
    }
  }
  if(is.raw(m)) {
    if(method=="dracor") {
      m=dracor::draco_decode(m)
      verts=m$points
      inds=m$faces
    } else {
      dracopy_available(action = 'stop')
      dp <- reticulate::import("DracoPy")
      m=dp$decode_buffer_to_mesh(m)
      verts=matrix(m$points, nrow=3)
      inds=matrix(m$faces+1, nrow=3)
    }
  } else if(!inherits(m, "DracoPy.DracoMesh")) {
    stop("This doesn't look like a Draco mesh!")
  }

  tmesh3d(vertices=verts, indices = inds, homogeneous = FALSE, ...)
}

simplify_meshlist <- function(x, ...) {
  if(inherits(x, 'mesh3d'))
    return(x)
  if(!inherits(x[[1]], 'mesh3d'))
    stop("This doesn't look like a list of mesh3d objects!")
  if(length(x)==1)
    return(x[[1]])
  coordsl=lapply(x, xyzmatrix)
  coords=do.call(rbind, coordsl)
  nv=sapply(coordsl, nrow)

  indsl=sapply(x, function(x) t(x$it), simplify = F)
  csnv=cumsum(nv)
  for(i in seq_along(indsl)[-1]) {
    indsl[[i]]=indsl[[i]]+csnv[i-1]
  }
  inds=do.call(rbind, indsl)

  tmesh3d(vertices=t(coords),
          indices = t(inds),
          homogeneous = FALSE, ...)
}
