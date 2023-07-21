#' Decode and manipulate Neuroglancer scenes (from URLs or JSON blocks)
#'
#' @description \code{ngl_decode_scene} takes a Neuroglancer scene from your web
#'   browser and turns it into an R \code{list} object that can be
#'   programmatically manipulated e.g. to add/remove segments. Manipulate these
#'   scenes with \code{\link{ngl_segments}}, \code{\link{ngl_layers}}. See
#'   \code{\link{ngl_encode_url}} to turn a scene back into a URL to open in your
#'   browser.
#' @param x Character vector containing single Neuroglancer URL or a json block
#' @param return.json When \code{TRUE} extracts the JSON block in a URL does not
#'   parse it to an R list
#' @param ... additional arguments passed to \code{jsonlite::\link{fromJSON}}
#' @inheritParams jsonlite::fromJSON
#' @return An R list with additional class \code{ngscene} describing the scene,
#'   or, when \code{return.json=TRUE}, a character vector.
#' @export
#' @family neuroglancer-urls
#' @seealso \code{\link[utils]{URLdecode}}, \code{\link[jsonlite]{fromJSON}}
#' @aliases ngscene
#' @examples
#'
#' \donttest{
#' # get sample FlyWire URL
#' fw_url=with_segmentation('flywire31', getOption('fafbseg.sampleurl'))
#' # only a 0 (dummy) segment id present
#' ngl_segments(fw_url)
#' #
#' fw_sc=ngl_decode_scene(fw_url)
#' fw_sc
#' # add two segments
#' fw_sc=fw_sc+c("720575940621039145", "720575940626877799")
#' ngl_segments(fw_sc)
#' # remove that 0 segment
#' fw_sc=fw_sc-0
#' ngl_segments(fw_sc)
#' # repeated segments are ignored i.e. no duplicates
#' ngl_segments(fw_sc+"720575940621039145")
#' # convert back to a URL, nb this depends on choose_segmentation
#' ngl_encode_url(fw_sc)
#'
#' \dontrun{
#' # open in your default browser
#' browseURL(ngl_encode_url(fw_sc))
#' }
#' }
#'
#' \dontrun{
#' ngl_decode_scene("<someneuroglancerurl>")
#'
#' # decode scene from URL currently on clipboard
#' scene=ngl_decode_scene(clipr::read_clip())
#'
#' # open a Neuroglancer URL in CATMAID
#' ngu="<someurl>"
#' library(elmr)
#' open_fafb(ngl_decode_scene(ngu))
#' # Or store the URL rather than opening it
#' cmu=open_fafb(ngl_decode_scene(ngu), open=FALSE)
#' }
ngl_decode_scene <- function(x, return.json=FALSE, simplifyVector = TRUE,
                             simplifyDataFrame = FALSE, ...) {
  saved_url <- NULL
  if (is.list(x)) {
    if(isTRUE(return.json))
      stop("Cannot return JSON when scene is an R object.",
           "See ?ngl_encode_url for that.")
    if(is.ngscene(x, strict=TRUE)) return(x)
    if(!is.ngscene(x, strict=FALSE))
      stop(deparse(substitute(x)),
           " is neither a valid ngscene object or a string!")
    # if it looks like it contains valid data, we'll fix it up at the end of the
    # function
    res <- x
  } else if(is.character(x)) {
    if (length(x) == 1) {
      if (isTRUE(substr(x, 1, 4) == "http")) {
        # This looks like a URL
        # special case, expand shortened flywire URLs
        if (!isFALSE(su <- shorturl(x))) {
          saved_url = flywire_expandurl(su, json.only = FALSE, ...)
          x <- ngl_decode_scene(saved_url, return.json = T)
        } else {
          saved_url <- x
          uu = urldecode(x)
          x = sub("[^{]+(\\{.*\\})$", "\\1", uu)
          if (nchar(x) == nchar(uu))
            stop("I couldn't extract a JSON fragment from that URL")
        }
      } else if (length(x) == 1 && file.exists(x)) {
        # looks like a file on disk
        x <- readLines(x, warn = FALSE)
      }
    }
    if (return.json)
      return(x)
    res=try(jsonlite::fromJSON(x, simplifyVector = simplifyVector,
                               simplifyDataFrame = simplifyDataFrame, ...), silent = T)
    if(inherits(res,'try-error')){
      stop("Invalid JSON scene description!\n",
           as.character(attr(res,'condition')))
    }
  }
  else stop(deparse(substitute(x)), " is neither an ngscene object or a string!")

  class(res)=c('ngscene','list')
  attr(res, 'url')=saved_url
  res[['layers']]=ngl_layers(res)
  res
}

is.ngscene <- function(x, strict=FALSE) {
  if(isTRUE(strict)) return(inherits(x, "ngscene"))
  # when not strict check it looks like an ngscene ...
  inherits(x, "ngscene") || (is.list(x) && "layers" %in% names(x))
}

shorturl <- function(x) {
  # must start https
  if(!isTRUE(try(substr(x,1,8)=='https://', silent = T)))
    return(FALSE)
  # must be valid URL
  px=try(httr::parse_url(x), silent = TRUE)
  if(inherits(px, 'try-error')) return(FALSE)
  if(px$hostname %in% c("tinyurl.com"))
    return(x)
  # looks like fully expanded fragment
  if(!is.null(px$fragment)) return(FALSE)
  if(!is.null(px$query$json_url))
    return(px$query$json_url)
  # may have been a bare URL, but in that case check path
  if(isTRUE(grepl("^nglstate(/api/v[0-9])*/[0-9]+$", px$path)))
    x
  else FALSE
}

#' @export
xyzmatrix.ngscene <- function(x, ...) {
  pos=x$navigation$pose$position
  if(is.null(pos)) stop("scene contains no position information")
  matrix(pos[['voxelCoordinates']]*pos$voxelSize, ncol=3, dimnames = list(NULL, c("X","Y","Z")))
}

#' @export
#' @importFrom nat voxdims
voxdims.ngscene <- function(x, ...) {
  vd=x$navigation$pose$position$voxelSize
  if(is.null(vd))
    stop("Unable to extract voxel dimensions from scene!")
  vd
}

#' Encode scene information into a neuroglancer URL
#'
#' @description \code{ngl_encode_url} converts an R list containing a
#'   neuroglancer scene into a URL that you can open in your browser.
#'
#' @param body A text file or character vector with JSON data or an R list
#'   object of class \code{ngscene}.
#' @param baseurl A URL specifying the neuroglancer server (if missing, uses the
#'   URL from which \code{body} was decoded if that was recorded or, failing
#'   that, \code{options("fafbseg.sampleurl")}). You can use any neuroglancer
#'   URL as will be appropriately truncated if it encodes scene information.
#' @param auto_unbox For expert use only. See \code{\link[jsonlite]{toJSON}} for
#'   details.
#' @param ... Additional arguments for \code{\link[jsonlite]{toJSON}}
#'
#' @details We take pains to ensure that entries that neuroglancer expects to be
#'   JSON arrays are (including \code{segments} and \code{hiddenSegments}) are
#'   always mapped to a JSON array (even when length 1).
#'
#'   The default baseurl depends on the current segmentation chosen by
#'   \code{\link{choose_segmentation}}.
#'
#' @return Character vector containing encoded URL
#' @seealso \code{\link{URLencode}}, \code{\link{open_fafb_ngl}},
#'   \code{\link[jsonlite]{toJSON}}
#' @export
#' @family neuroglancer-urls
#' @examples
#' \donttest{
#' # get sample FlyWire URL
#' fw_url=with_segmentation('flywire31', getOption('fafbseg.sampleurl'))
#' # only a 0 (dummy) segment id present
#' ngl_segments(fw_url)
#' #
#' fw_sc=ngl_decode_scene(fw_url)
#' # add a segment
#' fw_sc$layers[[2]]$segments=union(fw_sc$layers[[2]]$segments,
#'   "720575940626877799")
#' # convert back to a URL, nb this depends on choose_segmentation
#' ngl_encode_url(fw_sc)
#' # another way to do this, which long time R users may find more intuitive
#' as.character(fw_sc)
#'
#' \dontrun{
#' # open in your default browser
#' browseURL(ngl_encode_url(fw_sc))
#' # ... or
#' browseURL(as.character(fw_sc))
#' }
#' }
#'
#' \dontrun{
#' # copy JSON scene information from {} symbol at top right of neuroglancer
#' # now make a permanent URL for the scene
#' ngl_encode_url(clipr::read_clip())
#' }
ngl_encode_url <- function(body, baseurl=NULL,
                           auto_unbox=TRUE, ...) {
  json <- if(is.character(body)) {
    # if this looks like a file read it, otherwise assume it is json
    if(isTRUE(tools::file_ext(body)=='json')) readLines(body) else body
  } else {
    # the layers were named for convenience but neuroglancer doesn't want this
    names(body[['layers']]) <- NULL
    if(auto_unbox) {
      # wrapping length 1 segment vectors with I()
      # avoids a formatting error where auto_unbox produces a json scalar
      # when neuroglancer wants to see a json array
      preserve_array <- function(x, fields=c("segments", "hiddenSegments", "filterBySegmentation")) {
        for(fn in fields) {
          xs=x[[fn]]
          if(length(xs)==1)
            x[[fn]]=I(xs)
        }
        x
      }
      body[['layers']] <- lapply(body[['layers']], preserve_array)

      # 2 fields in annotations also need protecting
      fix_annotations <- function(x) {
        annotations=x[['annotations']]
        if(length(annotations)>=1 && is.list(annotations) && !is.data.frame(annotations)){
          x[['annotations']] = lapply(annotations, preserve_array,
                                      fields = c("segments", "tagIds"))
        }
        x
      }
      body[['layers']] <- lapply(body[['layers']], fix_annotations)
    }
    jsonlite::toJSON(body, auto_unbox=auto_unbox, ...)
  }
  json <- jsonlite::minify(json)
  # get baseurl from input object
  if(is.null(baseurl)) baseurl=attr(body, 'url')
  baseurl=baseurl_from_url(baseurl)
  paste0(baseurl, urlencode(json))
}

#' Add colours to the neuroglancer scene
#'
#' @param x neuroglancer scene in any form acceptable to \code{\link{ngl_decode_scene}} (including as a URL)
#' @param colours A dataframe with two columns, where the first is the id and
#'   the second is the colour, OR a character vector of colours named by the ids
#'   or one colour which would be added to all the displayed neurons. See
#'   \code{\link[grDevices]{col2rgb}} for additional details of how col can be
#'   specified.
#' @param layer Optional character vector specifying the layer to colour. When
#'   \code{lyaer=NULL} (the default) will choose a layer of type
#'   segmentation_with_graph if one exists.
#'
#' @return A neuroglancer scene object (see \code{\link{ngl_decode_scene}})
#' @export
#' @importFrom stats setNames
#' @importFrom grDevices col2rgb rgb
#' @examples
#' fw_url=with_segmentation('flywire31', getOption('fafbseg.sampleurl'))
#' ngl_add_colours(fw_url, colours=c("720575940614404544"="red"))
#'
#' \dontrun{
#' # colour all neurons in the URL on the clipboard red.
#' # Then convert back to URL and open in default browser
#' browseURL(as.character(ngl_add_colours(clipr::read_clip(), col="red")))
#'
#' # Let's colour neurons from these 3 scenes in red, green and blue
#' u1="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5695474417795072"
#' u2="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5198787572137984"
#' u3="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5673953041317888"
#' # sequentially build up a data.frame with the colour information
#' # note that col will be recycled to the same length as the number of segments
#' colourdf=data.frame(ids=ngl_segments(u1), col='red')
#' colourdf=rbind(colourdf, data.frame(ids=ngl_segments(u2), col='green'))
#' colourdf=rbind(colourdf, data.frame(ids=ngl_segments(u3), col='blue'))
#' # apply that to the first URL
#' sc=ngl_add_colours(u1, colourdf)
#' browseURL(as.character(sc))
#' }
#'
ngl_add_colours <- function(x, colours, layer=NULL) {

  if(!is.ngscene(x)) x <- ngl_decode_scene(x)

  layers <- if(is.null(layer)) {
    l=ngl_layers(x, type=='segmentation_with_graph')
    if(length(l)==0)
      l=ngl_layers(x, type=='segmentation')
    if(length(l)!=1)
      stop("Please use the layer argument to specify the layer containing segments!")
    l
  } else {
    ngl_layers(x)[layer]
  }
  layername=names(layers)

  if(length(layername) != 1)
    stop("Need exactly one layer.")
  if(is.data.frame(colours)) {
    if(ncol(colours)!=2)
      stop("The colours dataframe must have 2 columns")

    colours = as.list(setNames(as.character(colours[[2]]),
                               as.character(colours[[1]])))
  }

  if(!is.vector(colours))
    stop("I need a dataframe or a named vector of colours or one colour!")

  oldids=ngl_segments(x)
  if(is.null(names(colours))) {
    if(length(colours) != 1)
      stop("I need a dataframe or a named vector of colours or one colour!")
    colours=rep(colours, length(oldids))
    names(colours) <- oldids
  }
  if(!is.list(colours)) colours=as.list(colours)

  # add ids if necessary
  colourids=names(colours)
  if(!all(valid_id(colourids)))
    stop("colours argument contains invalid ids!")
  ngl_segments(x) <- union(oldids, colourids)

  # add colours, overwriting any previously specified
  oldcolours = ngl_layers(x)[[layername]][["segmentColors"]]
  if(!is.null(oldcolours)) {
    oldcolours[names(colours)]=colours
    colours=oldcolours
  }
  # sort by id for consistency
  colours=colours[sort(names(colours))]
  # convert to hex format since neuroglancer will do this anyway
  colours=col2hex(colours)
  ngl_layers(x)[[layername]][["segmentColors"]] = colours
  x
}

# utility function to convert R colours
col2hex <- function(x) {
  if(is.list(x)) {
    return(sapply(x, col2hex, simplify = F))
  }
  hexmatrix=col2rgb(x)
  rgb(hexmatrix[1,], hexmatrix[2,], hexmatrix[3,], maxColorValue = 255)
}


#' @export
#' @rdname ngl_encode_url
#' @param x the \code{ngscene} object to be converted to a URL
#' @description \code{as.character.ngscene} is another way to convert a
#'   neuroglancer scene object to a URL.
as.character.ngscene <- function(x, ...) {
  ngl_encode_url(x, ...)
}

#' Construct Neuroglancer URL based on 3D location data
#'
#' @details Neuroglancer scenes seem to be specified in a single URL that
#'   encodes a json object defining layers to display, position etc. This
#'   function works by taking a sample URL defining such a scene and then
#'   editing it to point to a new 3D location / adjust zoom.
#'
#'   This package comes with 4 different default scene urls specified via
#'   \code{\link{choose_segmentation}} or \code{\link{with_segmentation}}. This
#'   is the easiest way to choose a particular segmentation. You can also
#'   specify a different sample URL via the \code{sampleurl} argument; it will
#'   be remembered for the rest of the R session. If you regularly use a
#'   particular kind of scene URL, you can set \code{options(fafbseg.sampleurl)}
#'   in your \code{\link{Rprofile}} file.
#'
#' @param x A numeric vector OR any object compatible with
#'   \code{\link[nat]{xyzmatrix}} OR a CATMAID URL (see details)
#' @param s Optional selection function of the type returned by
#'   \code{\link[rgl]{select3d}}
#' @param zoomFactor The Neuroglancer zoomFactor (bigger means zoomed out)
#' @param sample,reference Template space of the input object \code{sample} and
#'   target (\code{reference}). See examples and \code{\link{xform_brain}} for
#'   details of how these are specified.
#' @param sampleurl A sample URL that defines your Neuroglancer dataset.
#' @param coords.only Return raw coordinate string for pasting into Neuroglancer
#'   position widget (top left of screen)
#' @param open Whether or not to open the URL in a browser - this defaults to
#'   \code{TRUE} in interactive use.
#' @param ... Additional arguments passed to \code{\link[jsonlite]{fromJSON}} to
#'   control JSON parsing.
#' @return A character vector with a Neuroglancer URL or coordinate string
#'   (invisibly when \code{open=TRUE})
#' @importFrom jsonlite read_json
#' @importFrom catmaid catmaid_parse_url
#' @importFrom utils browseURL
#' @export
#' @family neuroglancer-urls
#' @examples
#' u=paste0("https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&",
#' "yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1")
#'
#' # translate URL but don't open browser
#' open_fafb_ngl(u, open=FALSE)
#'
#' # produce an x,y,z string to paste into Neuroglancer
#' open_fafb_ngl(u, coords.only=TRUE)
#'
#' # translate URL converting from FAFB14 to FlyWire coordinates
#' # (only a small shift)
#' open_fafb_ngl(u, sample="FAFB14", reference="FlyWire", open=FALSE)
#' \dontrun{
#' # copy CATMAID URL from clipboard and Neuroglancer coords to clipboard
#' clipr::write_clip(open_fafb_ngl(clipr::read_clip(), coords.only=TRUE))
#'
#' # Open a location in MB peduncle with current preferred segmentation
#' open_fafb_ngl(c(433440, 168344, 131200))
#'
#' # choose a particular segmentation (Google FAFB)
#' with_segmentation("20190805", open_fafb_ngl(c(433440, 168344, 131200),
#' zoomFactor=2))
#'
#' # or FlyWire
#' with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200)))
#'
#' # ... and translate FAFB14 to FlyWire coordinates
#' with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200),
#' sample="FAFB14", reference="FlyWire", zoomFactor=2))
#'
#' # open a CATMAID URL in Neuroglancer
#' open_fafb_ngl(u)
#'
#' # Set an existing scene URL (pointing to any old location) to act as
#' # the template for open_fafb_ngl
#' # nb the package sets one for you on startup if you haven't set yourself
#' options(fafbseg.sampleurl="https://<neuroglancerlurl>")
#' # Edit your R profile if you want to set a different default
#' usethis::edit_r_profile()
#' }
#' @importFrom nat.templatebrains xform_brain
open_fafb_ngl <- function(x, s = rgl::select3d(), zoomFactor=8,
                          coords.only=FALSE, open=interactive() && !coords.only,
                          sample=NULL, reference=NULL, sampleurl=NULL,
                          ...) {
  if(is.character(x)) {
    x=catmaid_parse_url(x)
  }
  if (is.vector(x, mode = "numeric") && length(x) == 3) {
    xyz = matrix(x, ncol = 3)
  }
  else {
    xyz = xyzmatrix(x)
    if (nrow(xyz) > 1) {
      xyz = colMeans(xyz[s(xyz), , drop = F])
      xyz = matrix(xyz, ncol = 3)
    }
  }
  if(!is.null(sample) && !is.null(reference))
    xyz=xform_brain(xyz, sample=sample, reference = reference, ...)

  sampleurl=check_sampleurl(sampleurl)

  # f=system.file('neuroglancer/split3xfill.json', package = 'fafbseg')
  # j=read_json(f, simplifyVector = T)
  j=ngl_decode_scene(sampleurl)
  if(is.null(j$navigation$pose$position))
    stop("Sorry, this scene URL does not seem to have any navigation information!")
  j$navigation$pose$position$voxelCoordinates=as.vector(xyz/j$navigation$pose$position$voxelSize)
  j$navigation$zoomFactor=zoomFactor

  u <- if(coords.only) {
    paste(j$navigation$pose$position$voxelCoordinates, collapse = ',')
    # nb make sure that we use a base URL matching the sample URL we were given
  } else ngl_encode_url(j, baseurl = baseurl_from_url(sampleurl))
  if(open) {
    browseURL(u)
    invisible(u)
  } else u
}


# helper function to make base url from sample URL
baseurl_from_url <- function(url=NULL,
                                    fragment='!') {
  # use sampleurl option if unset, but never set the option
  url <- check_sampleurl(url, set = FALSE)
  pu <- httr::parse_url(url)
  pu$path=NULL
  pu$fragment <- if(isTRUE(nzchar(fragment))) fragment else NULL
  baseurl <- httr::build_url(pu)
  baseurl
}

check_sampleurl <- function(sampleurl=NULL, set=NA) {
  op=getOption('fafbseg.sampleurl')
  if(is.null(sampleurl)) {
    if(is.null(op))
      stop("You must specify sampleurl at least once per R session",
           ", using choose_segmentation() or in your .Rprofile!")
    sampleurl=op
  } else {
    # we were passed a sampleurl argument and option was unset
    if(!isFALSE(set) && is.null(op)) {
      if(interactive())
        message(sprintf("Setting options(fafbseg.sampleurl='%s')", sampleurl))
      options('fafbseg.sampleurl'=sampleurl)
    }
  }
  sampleurl
}

check_cloudvolume_url <- function(cloudvolume.url=NULL, set=NA) {
  op=getOption('fafbseg.cloudvolume.url')
  if(is.null(cloudvolume.url)) {
    if(is.null(op))
      stop("You must specify cloudvolume.url at least once per R session",
           ", using choose_segmentation() or in your .Rprofile!")
    cloudvolume.url=op
  } else {
    # we were passed a cloudvolume.url argument and option was unset
    if(!isFALSE(set) && is.null(op)) {
      if(interactive())
        message(sprintf("Setting options(fafbseg.cloudvolume.url='%s')", cloudvolume.url))
      options('fafbseg.cloudvolume.url'=cloudvolume.url)
    }
  }
  cloudvolume.url
}

#' Return a blank neuroglancer scene based on a specified segmentation
#'
#' @description defaults to the current segmentation defined by
#'   \code{\link{choose_segmentation}} when \code{release=NULL}
#' @param return.url Whether to return a URL rather than a \code{ngscene}
#'   object.
#' @inheritParams choose_segmentation
#' @seealso \code{\link{choose_segmentation}}
#' @family neuroglancer-urls
#' @export
#' @examples
#' # blank scene for current segmentation
#' ngl_blank_scene()
#' # add a specific id
#' ngl_blank_scene()+"720575940623755722"
#' # a different segmentation
#' ngl_blank_scene("202004")
#' u=ngl_blank_scene("202004", return.url=TRUE)
#' \dontrun{
#' u=ngl_blank_scene("sandbox")
#' browseURL(u)
#' }
ngl_blank_scene <- function(release=NULL, return.url=FALSE) {
  u=if(!is.null(release)) {
    with_segmentation(release=release, getOption("fafbseg.sampleurl"))
  } else getOption("fafbseg.sampleurl")

  sc=ngl_decode_scene(u)
  ngl_segments(sc) <- NULL
  sc
  if(isTRUE(return.url)) ngl_encode_url(sc, baseurl = u) else sc
}
