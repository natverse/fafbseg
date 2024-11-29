#' @export
as.neuron.navis.core.skeleton.TreeNeuron <- function(x, ...) {
  tf=tempfile(fileext = '.swc')
  on.exit(unlink(tf))
  x$to_swc(tf)
  n=nat::read.neuron(tf)
  n$units=as.character(x$units$units)
  n
}

#' @export
as.neuronlist.navis.core.base.BaseNeuron <- function(l, ...) {
  nl=neuronlist(navis2nat_neuron(l), ...)
  names(nl)=pyids2bit64(l$id)
  attr(nl, 'units')=as.character(l$units$units)
  nl
}

#' @export
as.neuronlist.navis.core.neuronlist.NeuronList <- function(l, df=NULL, ...) {
  inl=if(inherits(l$neurons, 'python.builtin.object'))
    reticulate::py_to_r(l$neurons) else l
  if(length(inl)==0)
    return(neuronlist())
  nl=nlapply(inl, navis2nat_neuron, ...)
  names(nl)=pyids2bit64(l$id$tolist())
  if(!is.null(df))
    data.frame(nl)=df
  attr(nl, 'units')=as.character(l$units$units)
  nl
}

#' @export
as.neuron.navis.core.dotprop.Dotprops <- function(x, ...) {
  y=x$convert_units('micron')
  dp=nat::as.dotprops(list(points=y$points, alpha=NULL, vect=y$vect))
  attr(dp, 'id')=as.character(x$id)
  attr(dp, 'filename')=as.character(x$id)
  dp
}

navis2nat_neuron <- function(x, ...) {
  if(inherits(x, "navis.core.skeleton.TreeNeuron"))
    as.neuron.navis.core.skeleton.TreeNeuron(x, ...)
  else if(inherits(x, "navis.core.dotprop.Dotprops"))
    as.neuron.navis.core.dotprop.Dotprops(x, ...)
  else stop("Unrecognised navis data type:",
            paste(class(x), collapse = ','))
}

navis2nat_neuronlist <- function(x, ...) {
  if(inherits(x, 'navis.core.neuronlist.NeuronList'))
    as.neuronlist.navis.core.neuronlist.NeuronList(x, ...)
  else if(inherits(x, 'navis.core.base.BaseNeuron'))
    as.neuronlist.navis.core.base.BaseNeuron(x, ...)
  else stop("Unrecognised navis data type:",
            paste(class(x), collapse = ','))
}

#' Read L2 skeleton or dotprops for FlyWire data sources using fafbseg-py
#'
#' @param id One or more flywire ids
#' @param OmitFailures Whether or not to drop neurons that cannot be read from
#'   the results (rather than erroring out). Default \code{TRUE}.
#' @param datastack_name A CAVE datastack_name. When missing will use the
#'   default implied by the segmentation chosen by
#'   \code{\link{choose_segmentation}}.
#' @param ... Additional arguments passed to the
#'   \code{fafbseg.flywire.l2_skeleton} or
#'   \code{fafbseg.flywire.l2_dotprops}functions.
#'
#' @description \code{read_l2skel} reads one or more neurons as simplified L2
#'   skeletons.
#'
#' @description \code{read_l2dp} reads one or more neurons as simplified
#'   dotprops format. See details.
#'
#' @details \code{read_l2dp} is generally recommended rather than fetching a
#'   skeleton and then calculating dotprops because it is much faster and also
#'   computes better direction vectors. However if you wish to simplify a
#'   skeleton (e.g. to find the cell body fibre) then you will need to take the
#'   two step approach. This also has the possible advantage that you can
#'   specify the step size at which direction vectors are generated along the
#'   neuron. Note also that \code{read_l2dp} may drop some regions of the neuron
#'   (likely thin ones) that define only a very small mesh volume.
#'
#'   These functions depends on Philipp Schlegel's \code{fafbseg-py} package.
#'   You can install this using \code{\link{simple_python}}.
#'
#'   The \code{datastack_name} argument is optional because the correct
#'   datastack name and corresponding cloud volume URL will be read from options
#'   set by \code{\link{choose_segmentation}}; this is generally the preferred
#'   way for end users to select an active dataset. Neverthless, if a
#'   \code{datastack_name} it will be used to look up the correct segmentation
#'   URL and fafbseg-py will be correctly set up using these two pieces of
#'   information.
#'
#' @export
#' @examples
#' \dontrun{
#' # install full set of recommended packages including fafbseg-py
#' simple_python("full")
#' kcsvids=c("78603674556915608", "78462662124123765", "77547662357982001",
#' "78533168373869635", "78251418452635714", "78323024281482155",
#' "78322062208411707", "78533649477402370", "77829412279715493",
#' "77899643517979532", "78814230967028270", "78533993141739277",
#' "78041274292494941", "78252449311896359", "77618924522629940",
#' "77618237260576979", "78673768356594679", "78182148951479619",
#' "78392293379997680", "77688812230426430")
#' kcids=flywire_rootid(kcsvids)
#' kcs=read_l2skel(kcids)
#'
#' library(nat.nblast)
#' kcdps=read_l2dp(kcids)
#' # nb these are in microns
#' boundingbox(kcdps)
#' kcaba=nblast_allbyall(kcdps)
#' kchc=nhclust(scoremat = kcaba)
#' plot(kchc)
#' # 3d plot using the skeletons rather than dotprops versions of the neurons
#' # gamma neurons seprate from the rest
#' plot3d(kchc, k=2, db=kcs)
#' }
read_l2skel <- function(id, OmitFailures=TRUE, datastack_name=NULL, ...) {
  id=flywire_ids(id, must_work = T)
  fp=fafbsegpy4dataset(datastack_name=datastack_name)
  # handle changes in function name
  FUN=grep("l2_skeleton", names(fp$flywire), value = T)
  sk=fp$flywire[[FUN]](id, omit_failures = OmitFailures, ...)
  navis2nat_neuronlist(sk)
}

#' @rdname read_l2skel
#' @export
read_l2dp <- function(id, OmitFailures=TRUE, datastack_name=NULL, ...) {
  id=flywire_ids(id, must_work = T)
  fp=fafbsegpy4dataset(datastack_name=datastack_name)
  # handle changes in function name
  FUN=grep("l2_dotprops", names(fp$flywire), value = T)
  sk=fp$flywire[[FUN]](id, omit_failures = OmitFailures, ...)
  navis2nat_neuronlist(sk)
}


fafbsegpy4dataset <- function(datastack_name = NULL) {
  if(is.null(datastack_name)) {
    datastack_name <- getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")
    url=flywire_cloudvolume_url()
  } else {
    fcc=flywire_cave_client(datastack_name = datastack_name)
    url=fcc$info$segmentation_source()
  }
  fp=check_fafbsegpy()
  if("set_default_dataset" %in% names(fp$flywire))
    fp$flywire$set_default_dataset(datastack_name)
  else {
    # older installation of fafbseg-py, need to set datastack+url explicitly
    fp$flywire$utils$CAVE_DATASETS[[datastack_name]]=datastack_name
    fp$flywire$utils$FLYWIRE_URLS[[datastack_name]]=url
  }
  fp
}


check_fafbsegpy <- memoise::memoise(function(
    min_version=NULL, convert=FALSE,
    datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  check_reticulate()
  tryCatch(
    cv <- reticulate::import("fafbseg", convert=convert),
    error = function(e) {
      stop(
        call. = F,
        "Please install the python fafbseg package:\n",
        "This should normally work:\n",
        "fafbseg::simple_python(pkgs='fafbseg')\n",
        "For more details see ?simple_python",
        "\nDetailed error message: ",
        e
      )
    }
  )
  if(!is.null(min_version)) {
    warning("min_version not yet implemented for fafbseg")
    #   cvv=numeric_version(cloudvolume_version())
    #   if(!isTRUE(cvv >= min_version))
    #     stop("You need cloudvolume version: ", min_version, " but you have: ", cvv,
    #          "\n  Please update e.g. using\n",
    #          "fafbseg::simple_python('basic')")
  }
  cv
})

