
#' Get predicted dense core vesicle locations in the flywire dataset
#'
#' @description Preliminary dense core vesicle (DCV) detection results from the Lee group, Stephan Gerhard and Minsu Kim.
#'
#' @importFrom httr POST
#' @inheritParams flywire_fetch
#'
#' @param rootid flywire rootid/rootids
#' @param dataset which DCV data set from Stephan Gerhard to access. 1.0 is just for the antennal lobes of FAFB. 2.0 is the first run at the whole brain.
#' @param project from which project to pull data. At the moment, there is only one option for DCV data.
#' @param islatest logical, whether or not to fetch the latest root_id if the given IDs are out of date.
#' @param simplify logical, if \code{FALSE} each rootid is a separate set of two data frames (one for DCV positions, one for the neuron's synapses).
#' Else, a list of two combined data frames is returned.
#' @param OmitFailures logical, if \code{TRUE} then requests that result in a 500 error are dropped and a warning is displayed
#' but not an error.
#' @param cl A cluster object created by \code{parallel::makeCluster}, or an integer to indicate number of child-processes (integer values are ignored on Windows) for parallel evaluations.
#'
#' @return A list, where the first entry contains DCV locations and the second the synapses for the given rootid, with the nearest DCV precalculated for each synapse..
#' @export
#' @examples
#' \donttest{
#' # Just AL test data set
#' data = flywire_dcvs("720575940629166904", dataset = "dcv.1.0")
#' dcv = data$dcv
#' synapse = data$syns
#'
#' # Whole brain data set
#' dcv = flywire_dcvs(c("720575940631973089","720575940629166904"), dataset = "dcv.2.0")
#' }
#' @seealso \code{\link{braincircuits_login}}
flywire_dcvs <- function(rootid,
                         dataset = c("dcv.3.0","dcv.2.0","dcv.1.0"),
                         project = 'fruitfly_fafb_flywire',
                         islatest = TRUE,
                         simplify = TRUE,
                         return = c("parsed", "text", "response"),
                         token=NULL,
                         simplifyVector = TRUE,
                         include_headers = FALSE,
                         OmitFailures = TRUE,
                         cl = NULL,
                         ...){
  return=match.arg(return)
  project=match.arg(project)
  dataset = match.arg(dataset)
  rootid = as.character(rootid)
  rootid = unique(setdiff(rootid,"0"))
  if(is.null(token))
    token = chunkedgraph_token()

  # Get url
  url = switch(dataset,
               `dcv.3.0` = "https://api.braincircuits.io",
               `dcv.2.0` = "https://radagast.hms.harvard.edu/flywire/%s/dcv",
               `dcv.1.0` = "https://radagast.hms.harvard.edu/flywire/dcv/for_segment",
               stop("Unrecognised value for dataset argument!")
  )

  # Request type
  req.type = switch(dataset,
                    `dcv.3.0` = "POST",
                    `dcv.2.0` = "GET",
                    `dcv.1.0` = "POST",
                    stop("Unrecognised value for dataset argument!")
  )

  # If many rootids
  if(length(rootid)>1 & dataset != "dcv.3.0") {
    res=pbapply::pbsapply(rootid, flywire_dcvs, dataset = dataset,
                          return = return, simplify = FALSE, token = token,
                          simplifyVector = simplifyVector,
                          include_headers = include_headers,
                          OmitFailures = OmitFailures, cl = cl)
    if(simplify & dataset %in% c("dcv.1.0")){
      dcv = data.frame()
      syns = data.frame()
      for(i in 1:length(res)){
        dcv = rbind(dcv, res[[i]]$dcv)
        syns = rbind(syns, res[[i]]$syns)
      }
      res = list(dcv=dcv, syns=syns)
    }else if (dataset %in% c("dcv.2.0")){
      res = do.call(rbind, res)
    }
    return(res)
  }

  # Fetch data for rootid
  if(req.type=="GET"){
    # Get dcv data with a GET request
    req <- memoised_RETRY(
      'GET',
      url = sprintf(url,rootid),
      times = 10L
    )
  }else if(dataset == "dcv.1.0"){
    # Get DCV data with a POST request
    body = list(agglo_id = rootid, auth_token = token)
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
    req <- memoised_RETRY(
      'POST',
      url = url,
      body = body,
      times = 10L
    )
  }else{
    # Get DCV data with a POST request
    atoken = braincircuits_token()
    params = list(project = project)
    body = list(segments = as.list(rootid), project = project)
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
    req <- POST(
      url = file.path(url,"dcv/by"),
      config = httr::add_headers(
        Authorization = paste0("Bearer ", atoken),
        referer = url,
        `Content-Type` = "application/json"
      ),
      query = params,
      body = body
    )
  }

  # Error?
  if (req$status_code == 500 && OmitFailures) {
    warning("Could not retreive flywire ID ", rootid)
    return(NULL)
  }else{
    flywire_errorhandle(req)
  }

  # Parse and return the type of data requested..
  if (return=='parsed') {
    parsed = parse_json(req, simplifyVector = simplifyVector, bigint_as_char=TRUE)
    if (length(parsed) == 2 && isTRUE(names(parsed)[2] == 'error')) {
      stop("flywire error: " , parsed$error)
    }
    if(include_headers) {
      fields_to_include = c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    if(length(parsed) & dataset != "dcv.3.0"){
      untangle_dcv_data(parsed, rootid)
    }else{
      untangle_dcv_data_v3(parsed)
    }
  } else if(return=="text") {
    httr::content(req, as='text', type = 'application/json', encoding = 'UTF-8')
  } else req
}

#' Create and log into a braincircuits account
#'
#' @description  \href{https://braincircuits.io/}{braincircuits} is a website built by Stephan Gerhard that provides some services for FAFB flywire data. For example, information on predictions for dense core vesicles (DCVs).
#' These functions assist you in registering and logging into a braincircuits account in order to query its API. You must first log into the website \href{https://braincircuits.io/app/login}{here}.
#' You then run \code{braincircuits_login}, which will call \code{braincircuits_register}. At the moment, this function is only needed for \code{flywire_dcvs} and is called under the hood there. Therefore, beyond testing that you
#' can connect to \href{https://braincircuits.io/}{braincircuits.io} there is little reason to call this function directly.
#'
#' @param open flywire rootid/rootids
#' @param email the email you have registered/want to register with \href{https://braincircuits.io/}{braincircuits.io}.
#' @param password the password you want to set (when using \code{braincircuits_register}) or have set (when using \code{braincircuits_login}) for your account.
#' @param url the URL for the braincircuits API. Typically you should not need to change this.
#' @param open logical, whether or not to open a browser window to the \href{https://braincircuits.io/app/login}{login page} for \href{https://braincircuits.io/}{braincircuits.io}.
#'
#' @return A bearer access token with which to query the braincircuits API.
#' @examples
#' \dontrun{
#' # Run this function:
#' braincircuits_login()
#' # If you run it for the first time, a window will open
#' # to the braincircuits log in page. Log in with an email.
#' # then when prompted in the R command line, re-give your email address and
#' # give a new password. You can save these in your .Renviron file to prevent having
#' # to do this in future.
#' }
#' @seealso \code{\link{flywire_dcvs}}
#' @export
#' @rdname braincircuits_login
braincircuits_register <- function(open = TRUE, email = NULL, password = NULL, url = "https://api.braincircuits.io"){
  if(open){
    browseURL(url = "https://braincircuits.io/app/login", browser = getOption("browser"), encodeIfNeeded = FALSE)
  }else{
    message("registering with braincircuits.io, make sure you have already made an account with your given email at: https://braincircuits.io/app/login")
  }
  if(is.null(email)){
    email = readline("email: ")
  }
  if(is.null(password)){
    password = readline("new password: ")
  }
  body = list(email = email, password = password)
  body = jsonlite::toJSON(body, auto_unbox = TRUE)
  req <- memoised_RETRY(
    'POST',
    url = file.path(url,"auth/register"),
    body = body,
    times = 10L
  )
  # Error?
  if (req$status_code == 500) {
    warning("Could not register given email", email)
    return(NULL)
  }else{
    flywire_errorhandle(req)
  }
  # Set in R environ
  Sys.setenv(braincircuits_email=email)
  Sys.setenv(braincircuits_password=password)
  message("Your password has been reset. ")
  message(sprintf("Call usethis::edit_r_environ and add the lines: \n  braincircuits_email='%s' \n  braincircuits_password='%s'",email,password))
  invisible()
}

# Get access token
#' @export
#' @rdname braincircuits_login
braincircuits_login <- function(email = NULL, password = NULL, url = "https://api.braincircuits.io"){
  if(is.null(email)){
    email = Sys.getenv("braincircuits_email")
  }
  if(is.null(password)){
    password = Sys.getenv("braincircuits_password")
  }
  if(email==""|password==""){
    braincircuits_register(open = TRUE, url = url)
  }
  body = list(username = email, password = password)
  req <- httr::POST(
    url = file.path(url,"auth/db/login"),
    body = body
  )
  # Error?
  if (req$status_code == 500) {
    warning("Could not register given email", email)
    return(NULL)
  }else{
    flywire_errorhandle(req)
  }
  parsed = parse_json(req, simplifyVector = FALSE, bigint_as_char=TRUE)
  atoken = parsed$access_token
  Sys.setenv(braincircuits_token=atoken)
  message("Your API access token has been reset. ")
  message(sprintf("Call usethis::edit_r_environ and add the line:\n  braincircuits_token='%s'",atoken))
  atoken
}

# Get token
#' @export
#' @rdname braincircuits_login
braincircuits_token <- function(email = NULL, password = NULL, url = "https://api.braincircuits.io"){
  atoken = Sys.getenv("braincircuits_token")
  if(atoken==""){
    atoken = braincircuits_login(email = email, password = password, url = url)
  }
  atoken
}

# hidden
#' @importFrom methods is
untangle_dcv_data <- function(x, rootid){
  if(is(x, "list")){
    dcv = as.data.frame(do.call(rbind, x$dcv))
    dcv = unlist_df(dcv)
    dcv = as.data.frame(dcv)
    syns = as.data.frame(do.call(rbind, x$synaptic_links))
    syns = unlist_df(syns)
    if(nrow(dcv)) dcv$root_id = rootid
    if(nrow(syns)) syns$root_id = rootid
    list(dcv = dcv, syns = syns)
  }else{
    if(nrow(x)) x$root_id = rootid
    x
  }
}

# hidden
untangle_dcv_data_v3 <- function(x){
  if(is(x, "list")) {
    x = do.call(rbind, x)
  }
  dcv = as.data.frame(x)
  v14 = dcv$meta$v14
  colnames(v14) = paste0("v14_", colnames(v14))
  dcv$meta = NULL
  dcv = cbind(dcv, v14)
  if(nrow(dcv)){
    dcv = dcv %>%
      dplyr::mutate(root_id = `segment_id`) %>%
      as.data.frame()
  }
  dcv
}
