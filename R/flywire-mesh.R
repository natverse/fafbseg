# this is very much still WIP
read_graphene_meshes <- function(segment,
                                 cloudvolume.url=getOption("fafbseg.cloudvolume.url")) {
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
}
