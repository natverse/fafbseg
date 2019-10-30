.chunkedgraph_token <- memoise::memoise({function() {
  chunkedgraph_credentials_path = path.expand("~/.cloudvolume/secrets/chunkedgraph-secret.json")
  if(file.exists(chunkedgraph_credentials_path)){
    token=jsonlite::fromJSON(chunkedgraph_credentials_path)[['token']]
  }
  else {
    token=Sys.getenv("CHUNKEDGRAPH_SECRET")
  }
  if(!nzchar(token)) {
    stop(call. = F, "Unable to find chunked graph credentials!\n",
         "Please set as described at:\n",
         "https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson")
  }
  token
}
})

chunkedgraph_token <- function(cached=TRUE) {
  if(!cached) memoise::forget(.chunkedgraph_token)
  .chunkedgraph_token()
}
