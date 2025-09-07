connections_princeton_no_threshold <- memoise::memoise(function() {
  fcd=fafbseg:::flywire_connectome_dir(version = 783)
  cpn=file.path(fcd, 'connections_princeton_no_threshold_v783-zstd19.feather')
  if(!file.exists(cpn)) {
    message("downloading princeton synapses!")
    ud='https://flyem.mrc-lmb.cam.ac.uk/flyconnectome/flywire_connectivity/'
    url=paste0(ud, basename(cpn))
    download.file(url, destfile = cpn)
  }
  ds <- arrow::open_dataset(cpn, format = 'arrow')
  ds <- dplyr::rename_with(ds, ~ gsub("_root_", "_pt_root_", .x, fixed = TRUE))
  attr(ds, "version") = basename(dirname(cpn))
  ds
})
