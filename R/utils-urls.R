# base URLdecode gets very slow for long URLs
urldecode <- function(x) {
  curl::curl_unescape(x)
}

urlencode <- function(x) {
  curl::curl_escape(x)
}
