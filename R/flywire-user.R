#' Fetch information for one or more users
#'
#' @param uids A vector of user ids or a dataframe with a \code{user_id} column
#' @inheritParams flywire_cave_client
#'
#' @return A dataframe of user information
#' @export
#'
#' @examples
#' \donttest{
#' flywire_user_info(60, datastack_name = "flywire_fafb_public")
#' }
flywire_user_info <- function(uids, datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  fac=flywire_cave_client(datastack_name = datastack_name)
  if(is.data.frame(uids)) {
    df=uids
    stopifnot("user_id" %in% colnames(df))
  } else df=data.frame(user_id=uids)
  # nb must convert numeric to int or we get a silent failure
  df$user_id=checkmate::asInteger(df$user_id, lower = 0)
  res=fac$auth$get_user_information(as.list(df$user_id))
  rdf=dplyr::bind_rows(res)
  rdf=dplyr::left_join(df, rdf, by=c('user_id'="id"))
  if('affiliations' %in% names(rdf) && is.list(rdf[['affiliations']]))
    rdf$affiliations=paste(unlist(rdf$affiliations), collapse = ';')
  rdf
}
