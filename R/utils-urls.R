# base URLdecode gets very slow for long URLs
urldecode <- function(x) {
  curl::curl_unescape(x)
}

# unfortunately curl::curl_escape escapes slashes in URLs!
# and urltools::url_encode munges the #! that neuroglancer needs
# if you give it a whole URL
urlencode <- function(x) {
  pu=httr::parse_url(x)
  if(is.null(pu$scheme))
    return(curl::curl_escape(x))
  puf=pu$fragment
  if(!is.null(puf) && substr(puf, 1,1)=="!") {
    pu$fragment=paste0("!", curl::curl_escape(substr(puf, 2, nchar(puf))))
  } else {
    pu$fragment=curl::curl_escape(puf)
  }
  httr::build_url(pu)
}

