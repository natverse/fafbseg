nginfo <- function(ids, values=NULL, sep='_') {
  if(is.data.frame(ids)) {
    if(ncol(ids)>2)
      values=do.call(paste, c(ids[-1], sep=sep))
    else
      values=ids[[2]]
    ids=ids[[1]]
  }
  l=list(`@type` = "neuroglancer_segment_properties",
       inline = list(
         ids = as.character(ids),
         properties =
           list(
             list(
             id = "label",
             type = "label",
             values = values
           )
         )
       )
     )
  l
}


#' Read and write neuroglancer annotation info json files
#'
#' @details Note that there is nothing specific to flywire about these two
#'   functions - they could be used for any data source.
#'
#' @param f Path to a json file. \code{write_nginfo} will create the enclosing
#'   directory if necessary.
#' @param anndf A data.frame in which the first column contains ids for each
#'   neuron and additional columns contain annotations that will be joined into
#'   a single string.
#' @param sep The separator used to paste multiple columns of annotations
#'   together.
#'
#' @return For \code{read_nginfo} a list containing annotations.For
#'   \code{write_nginfo}, the path \code{f} invisibly.
#' @export
#' @seealso \code{\link{fct2nginfo}}
#' @examples
#' \donttest{
#' tf=tempfile(pattern = "info")
#' df=data.frame(id=c(10000,10002), type=c("DNp01"))
#' write_nginfo(df, tf)
#' }
write_nginfo <- function(anndf, f, sep='_') {
  l <- if(is.list(anndf) && !is.data.frame(anndf)) anndf else nginfo(anndf, sep=sep)
  d <- dirname(f)
  if(!file.exists(d))
    dir.create(d, recursive = T)
  jsonlite::write_json(l, path = f, auto_unbox=T)
  invisible(f)
}

read_nginfo <- function(f) {
  jsonlite::read_json(f, simplifyVector = T, simplifyDataFrame=F)
}

#' Convert flytable cell type information into a neuroglancer info file
#'
#' @param ids FlyWire root ids in any form understood by
#'   \code{\link{flywire_ids}}
#' @param gluestr Optional string passed to \code{glue::glue} which is
#'   interpreted in the context of the annotation data.frame produced by
#'   \code{\link{flytable_meta}}. This allows arbitrary formatting for
#' @param ... Additional arguments passed to \code{\link{flytable_meta}} and
#'   then eventually \code{\link{flytable_cell_types}}.
#' @inheritParams write_nginfo
#' @inheritParams add_celltype_info
#' @seealso \code{\link{write_nginfo}}
#'
#' @return The path \code{f} invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' # all neurons in info table
#' fct2nginfo(f='path/to/flytablev526/info', version=526)
#' fct2nginfo(f='path/to/hemilineagev526/info', version=526,
#'   gluestr="{ito_lee_hemilineage}_{toupper(substr(side,1,1))}")
#' fct2nginfo("MBON%", 'path/to/mboninfo/info')
#' }
fct2nginfo <- function(f, ids=NULL, version=NULL, sep='_', gluestr=NULL, ...) {
  fct <- flytable_meta(ids, version=version, ...)
  anndf <- if(!is.null(gluestr)) {
    check_package_available('glue')
    cbind(fct[1],
          value=glue::glue(gluestr, .envir = fct))
  } else {
    fct$side=toupper(substr(fct$side,1,1))
    fct[c("root_id", "cell_type", "side", "super_class")]
  }
  write_nginfo(anndf, f = f, sep=sep)
}
