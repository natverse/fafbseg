#' Extract annotations from a neuroglancer scene into a dataframe
#'
#' @param x A neuroglancer scene or URL (passed to
#'   \code{\link{ngl_decode_scene}} as necessary) or a neuroglancer layers
#'   (\code{\link{nglayers}}) extracted from such a scene.
#' @param layer Optional index vector specifying the layers within a scene from
#'   which to extract annotations. It is probably safest to use a character
#'   vector of layer names (what appears in neuroglancer). When missing all
#'   annotation layers are processed.
#' @param types Which annotation types to process (currently only points and
#'   lines by default)
#' @param points What to do with point coordinates.
#'
#' @return A data.frame with columns defined by the contents of the annotation
#'   layer and the \code{types}/\code{points} arguments. Additional annotation
#'   features are stored as attributes on the data.frame.
#' @export
#' @seealso \code{\link{ngl_annotation_layers}} to make new annotation layers
#'
#' @examples
#' \donttest{
#' u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5963849085747200"
#' adf=ngl_annotations(u)
#' str(attr(adf, 'ann_attrs'))
#' }
ngl_annotations <- function(x, layer=NULL, types=c("point", "line"),
                         points=c('collapse', 'expand', 'list')) {
  points=match.arg(points)
  types=match.arg(types, several.ok = TRUE)
  x <- if(inherits(x, 'nglayers')) x else ngl_decode_scene(x)
  anns <- if(is.null(layer)) {
    ngl_layers(x, type=="annotation")
  } else {
    if(is.list(layer)) layer else ngl_layers(x)[layer]
  }
  ann_attrs <- sapply(anns, function(x) x[names(x)!="annotations"], simplify = F)

  anndf=sapply(anns, process_one_layer, simplify = F, points=points)
  anndf=dplyr::bind_rows(anndf, .id = 'layer')
  attr(anndf, 'ann_attrs')=ann_attrs
  anndf
}


process_one_layer <- function(anns, points=c('collapse', 'expand', 'list'), types=c("point", "line")) {
  anntypes=sapply(anns$annotations,"[[", "type")
  pts=anns$annotations[anntypes %in% types]
  if(length(pts)==0) return(NULL)
  anndf = list2df(pts, points=points, convert_numeric=FALSE)
  anndf
}


list2df <- function(x, points=c('collapse', 'expand', 'list'),
                    lists=c("collapse", "list"), collapse=",", convert_numeric=TRUE, ...) {
  points=match.arg(points)
  lists=match.arg(lists)
  cns=unique(unlist(sapply(x, names, simplify = F)))

  collapse_col <- function(col) sapply(col, paste, collapse=collapse)
  l=list()
  for(i in cns) {
    raw_col = lapply(x, "[[", i)
    raw_col[sapply(raw_col, is.null)]=NA
    sublens=lengths(raw_col)
    raw_col[sublens==0]=NA
    if(all(sublens==1)){
      raw_col=unlist(raw_col)
      if(is.character(raw_col)) {
        raw_col[!nzchar(raw_col)]=NA_character_
        raw_col[raw_col=='NA']=NA_character_
        # if any non-NA columns
        num_nas=sum(is.na(raw_col))
        num_nums=sum(!is.na(suppressWarnings(as.numeric(raw_col))))
        if(convert_numeric && (num_nums+num_nas)==length(raw_col))
          raw_col=as.numeric(raw_col)
      }
    } else if(grepl("^point", i) && all(sublens==3L)) {
      if(points=='expand') {
        raw_col=lapply(1:3, function(j) sapply(raw_col, "[[", j))
        names(raw_col) <- paste0(i, c("X","Y","Z"))
        l[names(raw_col)]=raw_col
        next
      } else if(points=='collapse')
        raw_col=collapse_col(raw_col)
    } else if(lists=='collapse')
      raw_col=collapse_col(raw_col)

    l[[i]]=raw_col
  }
  # l
  tibble::as_tibble(l, ...)
}

normalise_cave_annotation_df <- function(x, colpal=NULL, rawcoords=NA) {
  x=dplyr::rename_with(x, function(x) sub("^pt_","", x))
  x=dplyr::rename_with(x, function(x) sub("^colo[u]*r$","col", x))
  cx=colnames(x)
  if("position" %in% cx)
    x=dplyr::rename(x, point=position)
  if(is.na(rawcoords)) {
    ir=is_rawcoord(x$point)
    rawcoords = all(ir)
    if(!rawcoords && any(ir))
      warning("You appear to have a mix of nm and raw coordinates. Please check and specify rawcoords=T/F accordingly!")
  }
  if(!rawcoords) x$point=xyzmatrix2list(flywire_nm2raw(x$point))

  if(all(c("supervoxel_id", "root_id") %in% cx))
    x$segments=paste(x$supervoxel_id, x$root_id, sep=",")
  else if("supervoxel_id" %in% cx)
    x$segments=as.character(x$supervoxel_id)
  else if("root_id" %in% cx)
    x$segments=as.character(x$root_id)

  if("col" %in% cx)
    x$col=col2hex(x$col)

  if(all(c("layer","col") %in% cx)) {
    # color and layer must be consistent
    nlayers=dplyr::n_distinct(x$layer)
    ncols=dplyr::n_distinct(x$col)
    if(nlayers==1 && ncols>1) {
      warning("Ignoring layer column since there are multiple colours")
      x$layer=NULL
    } else if(nlayers>1 && ncols>1 & ncols!=nlayers) {
      stop("Mismatch between layer and colour specification")
    } else if(nlayers>1 && ncols>1) {
      tt=table(x$layer, x$col)
      stopifnot(isTRUE(dim(tt)[1]==dim(tt)[2]))
      ncols_layer=rowSums(tt>0)
      if(any(ncols_layer>1))
        stop("Discrepancy between layer and colour specification. Make sure they match or provide only one!")
    }
  } else if("layer" %in% cx && !is.null(colpal)) {
    if(is.function(colpal))
      colpal=colpal(dplyr::n_distinct(x$layer))
    colpal=col2hex(colpal)
    levs=names(colpal)
    if(is.null(levs)) levs=unique(x$layer)
    x$col=colpal[match(x$layer, levs)]
    if(any(is.na(x$col))) {
      x$col[is.na(x$col)]='white'
      warning("Missing levels in colour palette; setting to white!")
    }
  } else if("col" %in% cx) {
    ucols=unique(x$col)
    ncols=length(ucols)
    x$layer <- if(ncols==1) 'annotation'
    else paste('annotation', match(x$col, ucols))
  } else {
    if(!"layer" %in% colnames(x))
      x$layer="annotations"
  }
  selcols=intersect(c("layer", "point", "segments", "col"),
                   colnames(x))
  x[selcols]
}


#' Construct one or more neuroglancer annotation layers
#'
#' @details If you supply a dataframe for the \code{ann} argument then you can
#'   have columns called \itemize{
#'
#'   \item \code{point} or \code{position} or \code{pt_position} to
#'   define the position. This should contain x,y,z coordinates formatted as a
#'   character vector (\code{\link{xyzmatrix2str}}) or a \code{list} of
#'   \code{numeric} \code{vector}s (\code{\link{xyzmatrix2list}}).
#'
#'   \item \code{layer} optionally name a layer for each point
#'
#'   \item \code{col} optionally specify a color for each point.
#'
#'   \item \code{root_id} optionally specify a supervoxel id that the point maps onto
#'
#'   \item \code{supervoxel_id} optionally specify a supervoxel id that the point maps onto
#'
#'   }
#'
#'   Neuroglancer only allows one colour per annotation layer, so if you specify
#'   both \code{col} and \code{layer} they must be consistent.
#'
#'   Neuroglancer annotations are specified in raw coordinates. Although this
#'   function can try to convert nm coordinates to raw, this will only work for
#'   points in the brain space defined by the current fafb segmentation (see
#'   \code{\link{choose_segmentation}}). For this reason you should used
#'   \code{rawcoords=FALSE} and convert coordinates yourself if you are working
#'   with other brain spaces.
#' @param ann An annotation dataframe (see details) or any object containing 3D
#'   vertices from which \code{\link{xyzmatrix}} can successfully extract
#'   points.
#' @param rawcoords Whether points have been provided in raw (voxel) coordinates
#'   or in calibrated (nm) positions. The default of \code{NA} will try to infer
#'   this based on the coordinate values but see details for limitations.
#'
#' @param colpal A function or named character vector of colours that will be
#'   used to set the colour for each layer. Colours should be specified by name
#'   or hex format.
#'
#' @return A list of additional class \code{nglayers} which can be added to an
#'   \code{ngscene} object as produced by \code{\link{ngl_decode_scene}}.
#' @export
#' @seealso \code{\link{ngl_annotations}} to extract annotations from a scene.
#' @examples
#' \dontrun{
#' ## as an example label proofread neurons by institution
#' psp=flywire_cave_query('proofreading_status_public_v1')
#' fwusers=googlesheets4::read_sheet('1G0zqA5DTrfd-a2LuebV4kcqNfl4q1ehlzHBrwT6ZMoc')
#' psp2=dplyr::left_join(psp, fwusers, by=c("user_id"="id"))
#' psp2$layer=psp2$institution
#' # sample 3000 neurons to be a more manageable as an example.
#' psp2s=dplyr::slice_sample(psp2, n=3000) %>%
#'   dplyr::filter(!is.na(layer))
#' # the layers will be rainbow coloured
#' al=ngl_annotation_layers(psp2s[c("pt_position", "layer")], colpal=rainbow)
#' # make a blank scene
#' sc=ngl_blank_scene()
#' # or decode a URL that you've copied from your browser
#' sc=ngl_decode_scene(clipr::read_clip())
#' # and the add your annotations as new layer(s) to that scene
#' sc2=sc+al
#' # and make a URL
#' u=as.character(sc2)
#' # and copy that to clipboard
#' clipr::write_clip(u)
#' # ... or open directly in your browser
#' browseURL(u)
#' # It is a good idea to shorten when there are many annotations.
#' # This will load much faster in the browser and be easier to work with
#' su=flywire_shortenurl(u)
#' browseURL(su)
#' }
ngl_annotation_layers <- function(ann, rawcoords=NA, colpal=NULL) {
  if(!is.data.frame(ann)) ann=data.frame(point=xyzmatrix2str(ann))
  stopifnot(is.data.frame(ann))
  ann=normalise_cave_annotation_df(ann, colpal=colpal, rawcoords=rawcoords)
  uls=unique(ann$layer)
  if(any(is.na(uls))) {
    warning("Some annotation rows have an `NA` layer - these will be dropped!")
    uls=na.omit(uls)
  }
  layers=list()
  for(l in uls) {
    annl=ann[ann$layer==l,,drop=F]
    col <- if(is.null(annl[['col']])) col2hex('red') else unique(annl[['col']])
    if(length(col)!=1)
      stop("You can only add 1 colour per layer!")
    layers[[l]]=ngl_annotation_layer(annl$point,
                                     segments = annl[['segments']],
                                     name = l,
                                     annotationColor = col)
  }
  class(layers)=c("nglayers", "list")
  layers
}

ngl_annotation_layer <- function(pos, name='annotations', annotationColor=NULL, segments=NULL) {
  # data.frame is confused by a list of points and tries to turn each
  # point into a column of its own
  nv=if(is.null(nrow(pos))) length(pos) else nrow(pos)
  pointdf=data.frame(point=NA, type="point", id=random_id(nv))
  # convert to list of 3-vectors
  pointdf$point=xyzmatrix2list(pos)
  if(!is.null(segments)) {
    if(isTRUE(segments)) {
      svids=flywire_xyz2id(pos, rawcoords = T, root = FALSE)
      rootids=flywire_rootid(svids, integer64 = F)
      segments=lapply(seq_along(svids), function(i) c(svids[i], rootids[i]))
    }
    pointdf[['segments']]=segments
  }

  if(!requireNamespace('purrr', quietly = TRUE))
    stop("Please install.packages('purrr') to use ngl_annotation_layers() function!")
  pointdf=purrr::transpose(pointdf)

  if(!is.null(annotationColor))
    annotationColor=col2hex(annotationColor)
  else annotationColor='red'
  layer=list(type="annotation",
             name=name,
             annotations=pointdf)
  if(!is.null(annotationColor))
    layer[['annotationColor']]=annotationColor

  layer
}

# make random ids as used for neuroglancer point annotations
random_id <- function(n=1, chunksize=100) {
  chunks=character()
  if(n>chunksize) {
    nchunks=floor(n/chunksize)
    chunks=unlist(replicate(nchunks, random_idchunked(chunksize), simplify = F))
    n=n%%chunksize
  }
  if(n>0) c(chunks, random_idchunked(n)) else chunks
}

# this is designed to speed up what would otherwise be a slow process
# if ids were generated one at a time
random_idchunked <- function(n=5000) {
  # dput(unname(sapply(c(0:9,"a", "b", "c", "d", "e", "f"),charToRaw)))
  cr=as.raw(c(0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
              0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66))
  m=matrix(sample(cr, n*41, replace = T), nrow=41)
  # replace row 41 with a null terminator
  m[41,]=as.raw(0L)
  readBin(as.raw(m), what = 'char', n=n)
}
