flywire_l2data <- function(rootid=NULL, l2ids=NULL, attributes=NULL, rval=c('data.frame', "list")) {
  if(!is.null(attributes) && is.character(attributes))
    attributes=as.list(attributes)
  rval=match.arg(rval)
  fcc=flywire_cave_client()
  if(is.null(l2ids) && is.null(rootid))
    stop("You must provide one of l2ids or rootid!")
  if(is.null(l2ids)){
    stopifnot(length(rootid)==1L)
    l2ids=flywire_l2ids(rootid)
  }
  pyids=rids2pyint(l2ids)
  res=reticulate::py_call(fcc$l2cache$get_l2data,l2_ids = pyids, attributes=attributes)
  resr=reticulate::py_to_r(res)
  if(rval!='list')
    list2df(resr, lists = 'list')
  else resr
}

flywire_l2volume <- function(rootids, cache=TRUE) {
  rootids=flywire_ids(rootids, integer64 = FALSE)
  fcc = flywire_cave_client()
  if(length(rootids)>1)
    return(pbapply::pbsapply(rootids, flywire_l2volume))

  fl2c=flywire_leaves_cache(subdir = file.path('flywire_l2volume', fcc$datastack_name))
  vol=fl2c$get(rootids)
  if(cachem::is.key_missing(vol)) {
    vols=flywire_l2data(rootids, attributes='size_nm3')
    vol=sum(vols[,1])
    fl2c$set(rootids, vol)
  }
  vol
}
