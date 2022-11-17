
# a simple memory cache that can efficiently store and fetch many small values
vcache <- memoise::memoise(function(cachename, default=NA){
  check_package_available('fastmap')
  fm=fastmap::fastmap(missing_default = default)
  fm
})

vcache64 <- function(cachename) {
  vcache(cachename, default = bit64::as.integer64(0L))
}

vcache_mget <- function(vc, keys) {
  missing_default=environment(vc$mget)$missing_default
  vals=vc$mget(keys)
  ulv=unlist(vals, use.names = FALSE, recursive = F)
  if(bit64::is.integer64(missing_default)) {
    class(ulv)=class(missing_default)
  }
  ulv
}

vcache_mset <- function(vc, keys, values) {
  missing_default=environment(vc$mget)$missing_default
  if(!isTRUE(all.equal(class(values),class(missing_default))))
    stop("class mismatch of values with default missing value")
  if(!isTRUE(length(keys)==length(values)))
     stop("mismatch between length of keys and values!")
  names(values)=keys
  vc$mset(.list=as.list(values))
}
