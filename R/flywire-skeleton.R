# Skeletonise flywire neurons

#' Skeletonise neuron meshes using skeletor
#'
#' @description You can skeletonise complex neuron meshes using skeletor \href{https://github.com/schlegelp/skeletor}{skeletor}.
#' Skeletor is a python library and this function wraps a series of skeletor functions in order to smoothly process neurons
#' for use with the \href{http://natverse.org/}{natverse}.
#' Note, the default settings optimise performance for fast skeletonisation of \href{https://ngl.flywire.ai}{flywire} meshes.
#'
#' @param segments The segment ids to fetch (probably as a character vector).
#' Meshes are read from the specified CloudVolume (\code{cloudvolume.url}).
#' @param obj character. Path of a \code{obj} file or a folder of such files.
#' These files are read as meshes and then skeletonised. If \code{segments} is given, this argument is overrriden.
#' @param mesh3d logical. If \code{TRUE} then the neuron's volume is added to each \code{neuron} object in the resultant \code{neuronlist]} at \code{neuron$mesh3d}.
#' @param cloudvolume.url Optional url from which to fetch meshes normally
#'   specified by the \code{fafbseg.cloudvolume.url} option.
#' @param clean logical. If \code{TRUE} then, in python, \code{skeletor.clean} is used
#' to collapse twigs that have line of sight to each other and ove nodes outside the mesh back inside.
#' Note that this is not a magic bullet and some of this will not work (well)
#' if the original mesh was degenerate (e.g. internal faces or not watertight) to begin with.
#' @param radius Logical. Whether or not to return radius information for each skeleton node.
#' If you want to make use of radii, you will need to have the \code{ncollpyde}
#' python3 module installed. You can get this with \code{pip3 install ncollpyde}.
#' @param method.radii the method by which to determine each node's radius. \code{"knn"}
#' uses k-nearest-neighbors to get radii: fast but potential for being very wrong. \code{"ray"} uses ray-casting to get radii: slower but sometimes less wrong.
#' @param ratio numeric, 0-1. Factor to which to reduce mesh faces. For example,
#' a ratio of 0.5 will reduce the number of faces to 50 percent.
#' @param epsilon numeric. Target contraction rate as measured by the sum of all face
#' areas in the contracted versus the original mesh. Algorithm
#' will stop once mesh is contracted below this threshold.
#' Depending on your mesh (number of faces, shape) reaching a
#' strong contraction can be extremely costly with comparatively
#' little benefit for the subsequent skeletonization. Note that
#' the algorithm might stop short of this target if \code{iter_lim}
#' is reached first or if the sum of face areas is increasing
#' from one iteration to the next instead of decreasing.
#' @param iter_lim integer. Maximum rounds of contractions.
#' @param precision numeric. Sets the precision for finding the least-square solution.
#' This is the main determinant for speed vs quality: lower
#' values will take (much) longer but will get you closer to an
#' optimally contracted mesh. Higher values will be faster but
#' the iterative contractions might stop early.
#' @param SL numeric. Factor by which the contraction matrix is multiplied for
#' each iteration. In theory, lower values are more likely to
#' get you an optimal contraction at the cost of needing more iterations.
#' @param WHO numeric. Initial weight factor for the attraction constraints.
#' The ratio of the initial weights \code{WL0} (\code{1e-3 * sqrt(A)}) and \code{WH0}
#' controls the smoothness and the degree of contraction of the
#' first iteration result, thus it determines the amount of
#' details retained in subsequent and final contracted meshes.
#' @param validate If \code{True}, will try to fix potential issues with the mesh
#' (e.g. infinite values, duplicate vertices, degenerate faces)
#' before collapsing. Degenerate meshes can lead to effectively
#' infinite runtime for this function!
#' @param method Skeletonisation comes in two flavours with different Pros
#' and Cons. \code{"vertex_clusters"} groups and collapses vertices based
#' on their geodesic distance along the mesh's surface. It's
#' fast and scales well but can lead to oversimplification.
#' Good for quick & dirty skeletonisations. \code{"edge_collapse"} implements skeleton extraction by edge collapse described Au et al. 2008.
#' It's rather slow and doesn't scale well but is really good at preserving topology.
#' @param sampling_dist numeric. Maximal distance at which vertices are clustered. This
#' parameter should be tuned based on the resolution of your mesh.
#' @param ... Additional arguments passed to \code{reticulate::py_run_string}.
#'
#' @return A \code{nat::neuronlist} containing neuron skeleton objects.
#'
#' @details This pipeline:
#'
#' 1. Reads specified meshes from a CloudVolume source.
#'
#' 2. Simplifies each mesh (python: \code{skeletor.simplify})
#'
#' 3. Contract the mesh (python: \code{skeletor.contract})
#'
#' 4. Skeletonises the mesh (python: \code{skeletor.skeletonize})
#'
#' 5. Optionally, cleans the mesh (python: \code{skeletor.clean})
#'
#' 6. Optionally, add radius information to the skeleton (python: \code{skeletor.radii})
#'
#' You will therefore need to have a working python3 install of skeletor, which uses CloudVolume. You do not requir meshparty.
#' Please install the Python skeletor module as described at: \url{https://github.com/schlegelp/skeletor}. You must ensure that
#' you are using python3 (implicitly or explicitly) as mesh fetching from
#' graphene servers depends on this. This should normally work: \code{pip3 install git+git://github.com/schlegelp/skeletor@master}.
#' If you have already installed skeletor but it is
#' not found, then I recommend editing your \code{\link{Renviron}} file to set
#' an environment variable pointing to the correct Python. You can do this
#' with \code{usethis::edit_r_environ()} and then setting e.g. \code{RETICULATE_PYTHON="/usr/local/bin/python3"}.
#' (Though best practice would be to create a conda environment
#' for your natverse R sessions and direct R there using your environ file.)
#'
#' You will need to set up some kind of authentication in order to fetch volume data for skeletonisation.
#' See \url{https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson}
#' for how to get a token and where to save it. You can either save a json
#' snippet to \code{~/.cloudvolume/secrets/chunkedgraph-secret.json} or set an
#' environment variable (\code{CHUNKEDGRAPH_SECRET="XXXX"}.
#'
#' Finally you will also need to set an option pointing to your server. This is the server hosting th mesh data you are interested in. This
#' might look something like: \code{options(fafbseg.cloudvolume.url='graphene://https://xxx.dynamicannotationframework.com/segmentation/xxx/xxx')}
#' and you can easily add this to your startup \code{\link{Rprofile}}
#' with \code{usethis::edit_r_profile()}. For example, for the flywire data set, it is currently: 'graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31'
#'
#' @examples
#' \dontrun{
#' choose_segmentation("flywire")
#' nx=xform_brain(elmr::dense_core_neurons, ref="FlyWire", sample="FAFB14")
#' xyz =xyzmatrix(nx)
#' ids = unique(flywire_xyz2id(xyz[sample(1:nrow(xyz),100),]))
#' neurons = skeletor(ids)
#' plot3d(neurons) # note, in flywire space
#' plot3d(nx, col="black", lwd  =2) # note, in flywire space
#' }
#' @export
skeletor <- function(segments = NULL,
                     obj = NULL,
                     mesh3d = TRUE,
                     cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                     clean = TRUE,
                     radius = TRUE,
                     ratio = .1,
                     SL = 10,
                     WHO = 2,
                     iter_lim = 4,
                     epsilon=0.05,
                     precision=1e-6,
                     validate = TRUE,
                     method.radii=c("knn","ray"),
                     method=c('vertex_clusters','edge_collapse'),
                     sampling_dist=500,
                    ...){
  if(is.null(segments)&is.null(obj)){
    stop("Either the argument segments or obj must be given.")
  }
  if(!is.null(obj)){
    if(!grepl(".obj$",obj)){
      obj = list.files(obj,pattern = "obj$")
    }
    if(length(obj)==0){
      stop("No .obj files could be found")
    }else{
      segments=obj
    }
  }
  method.radii = match.arg(method.radii)
  method = match.arg(method)
  segments = unique(segments)
  py_skel_imports(cloudvolume.url=cloudvolume.url)
  neurons = nat::neuronlist()
  pb <- progress_bar$new(
    format = "  downloading [:bar] :current/:total eta: :eta",
    total = length(segments), clear = FALSE, show_after = 1)
  for(x in segments){
    pb$tick()
    tryCatch({
      swc = suppressWarnings(suppressMessages(py_skeletor(x,
                                         mesh3d = mesh3d,
                                         clean = clean,
                                         radius = radius,
                                         ratio = ratio,
                                         SL = SL,
                                         WHO = WHO,
                                         iter_lim = iter_lim,
                                         epsilon=epsilon,
                                         precision=precision,
                                         validate = validate,
                                         method.radii=method.radii,
                                         method=method)))
      swc = nat::as.neuronlist(swc)
      attr(swc,"df") = data.frame(id = x)
      names(swc) = gsub("*./","",x)
      neurons = c(neurons, swc)
      },
      error = function(e) {
        message("Failed: ", x)
        warning(e)
      })
  }
  diff = length(segments) - length(neurons)
  if(diff>0){
    warning(diff," ids could not be read and converted to skeletons")
  }
  neurons
}

# hidden
py_skel_imports <-function(cloudvolume.url=getOption("fafbseg.cloudvolume.url")){
  check_cloudvolume_reticulate()
  reticulate::py_run_string("from cloudvolume import CloudVolume")
  reticulate::py_run_string("import skeletor as sk")
  reticulate::py_run_string("import trimesh as tm")
  reticulate::py_run_string(sprintf("vol = CloudVolume('%s', use_https=True)",cloudvolume.url))
}

# hidden
py_skeletor <- function(id,
                        cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                        mesh3d = TRUE,
                        clean = TRUE,
                        radius = TRUE,
                        ratio = .1,
                        SL = 10,
                        WHO = 2,
                        iter_lim = 4,
                        epsilon=0.05,
                        precision=1e-6,
                        validate = TRUE,
                        method.radii=c("knn","ray"),
                        method=c('vertex_clusters','edge_collapse'),
                        sampling_dist=500,
                        ...){
  if(is.null(tryCatch(reticulate::py$vol,error=function(e) NULL))){
    py_skel_imports(cloudvolume.url=cloudvolume.url)
  }
  if(grepl("obj$",id)){
    reticulate::py_run_string(sprintf("m=tm.load_mesh('%s',process=False)",id))
    if(mesh3d){
      mesh = readobj::read.obj(id)
    }
  }else{
    reticulate::py_run_string(sprintf("id=%s",id))
    reticulate::py_run_string("m = vol.mesh.get(id, deduplicate_chunk_boundaries=False)[id]")
    mesh = NULL
  }
  reticulate::py_run_string("m = tm.Trimesh(m.vertices, m.faces)")
  reticulate::py_run_string(sprintf("simp = sk.simplify(m, ratio=%s)",ratio))
  reticulate::py_run_string(sprintf("cntr = sk.contract(simp, SL=%s, WH0=%s, iter_lim=%s, epsilon=%s, precision=%s, validate=%s, progress=False)",
                                    SL,WHO,iter_lim,epsilon,precision,ifelse(validate,"True","False")))
  reticulate::py_run_string(sprintf("swc = sk.skeletonize(cntr, method='%s', sampling_dist=500, progress=False)",
                                    method, sampling_dist))
  if(clean){
    reticulate::py_run_string("swc = sk.clean(swc, simp)")
  }
  if(radius){
   reticulate::py_run_string(sprintf("swc['radius'] = sk.radii(swc, simp, method='%s', n=5, aggregate='mean')",method.radii))
  }else{
    reticulate::py_run_string("swc['radius'] = 0")
  }
  reticulate::py_run_string("for c in ['x', 'y', 'z']: swc[c] = swc[c].astype(int)")
  swc = reticulate::py$swc
  colnames(swc) = c("PointNo","Parent","X","Y","Z","W")
  neuron = nat::as.neuron(swc)
  if(mesh3d){
    if(is.null(mesh)){
      savedir <- tempdir()
      ff=file.path(savedir, paste0(id, '.obj'))
      reticulate::py_run_string(sprintf("m.export('%s')",ff))
      res=sapply(ff, readobj::read.obj, convert.rgl = TRUE, simplify = FALSE)
      mesh=res[[1]][[1]]
      neuron$mesh3d = mesh
      class(neuron) = c(class(neuron), "neuronmesh")
      on.exit(unlink(savedir, recursive=TRUE))
    }
  }
  neuron
}
