# Skeletonise flywire neurons

#' Skeletonise neuron meshes using skeletor
#'
#' @description You can skeletonise complex neuron meshes using skeletor \href{https://github.com/schlegelp/skeletor}{skeletor}.
#' Skeletor is a python library and this function wraps a series of skeletor functions in order to smoothly process neurons
#' for use with the \href{http://natverse.org/}{natverse}.
#' Note, the default settings optimise performance for fast skeletonisation of \href{https://ngl.flywire.ai}{flywire} meshes.
#'
#' @param segments The segment ids to fetch (probably as a character vector),
#' e.g. flywire IDs or hemibrain bodyids.
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
#' You will need to have the \code{ncollpyde}
#' python3 module installed. You can get this with \code{pip3 install ncollpyde}. If you get issues
#' related to this module, best to set this to \code{FALSE}.
#' @param theta numeric. Used if \code(clean==TRUE). For each twig we generate the dotproduct between the tangent
#' vectors of it and its parents. If these line up perfectly the
#' dotproduct will equal 1. \code{theta} determines how much that
#' value can differ from 1 for us to still prune the twig: higher
#' theta = more pruning.
#' @param radius logical. Whether or not to return radius information for each skeleton node.
#' If you want to make use of radii, you will need to have the \code{ncollpyde}
#' python3 module installed. You can get this with \code{pip3 install ncollpyde}. If you get issues
#' related to this module, best to set this to \code{FALSE}.
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
#' @param WH0 numeric. Initial weight factor for the attraction constraints.
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
#' @param heal logical. Whether or not, if the neuron id fragmented, to stitch multiple fragments into single neuron using minimum spanning tree.
#' @param heal.threshold numeric. The threshold distance above which new vertices will not be connected
#' (default=Inf disables this feature). This parameter prevents the merging of vertices that are so far away from the main neuron that they are likely to be spurious.
#' @param heal.k integer. The number of nearest neighbours to consider when trying to merge different clusters.
#' @param reroot logical. Whether or not to re-root the neuron at an estimated 'soma'. A soma is usually a large ball in the neuron, which will
#' skeletonise into something of a hair ball. We can try to detect it quickly and reroot the skeleton there. We and do this by finding the nearest leafnodes to each leafnode,
#' and seeing if they are going off in divergent directions.
#' @param k.soma.search integer. The number of leaf nodes to find, around each leaf node of radius \code{radius.soma.search}, for the rerooting process. The larger the number, the better but slower.
#' @param radius.soma.search numeric. The distance within which to search for fellow leaf nodes for the rerooting process. Will be inaccurate at values that are too high or too low.
#' Should be about the size of the expected soma.
#' @param x a \code{nat::neuron} object.
#' @param brain a \code{mesh3d} or \code{hxsurf} object within which a soma cannot occur. For the re-rooting process. (Insect somata tend to lie outside the brain proper)
#' @param n For \code{method.radii == "knn"}. Radius will be the mean over \code{n} nearest-neighbors.
#' @param n_rays integer. For \code{method.radii == "knn"}.For \code{method.radii == "ray"}. Number of rays to cast for each node.
#' @param projection For \code{method.radii == "ray"}. Whether to cast rays in a sphere around each node or in a circle orthogonally to the node's tangent vector.
#' @param fallback For \code{method.radii == "ray"}. If a point is outside or right on the surface of the mesh
#' the raycasting will return nonesense results. We can either
#' ignore those cases (\code{"None"}), assign a arbitrary number or
#' we can fall back to radii from k-nearest-neighbors (\code{"knn"}).
#' @param sampling_dist numeric. For \code{method == "vertex_clusters"}. Maximal distance at which vertices are clustered. This
#' parameter should be tuned based on the resolution of your mesh.
#' @param cluster_pos numeric. For \code{method == "vertex_clusters"}. How to determine the x/y/z coordinates of the collapsed
#' vertex clusters (i.e. the skeleton's nodes). \code{"median"}: Use the vertex closest to cluster's center of mass.
#' \code{"center"}: Use the center of mass. This makes for smoother skeletons but can lead to nodes outside the mesh.
#' @param shape_weight numeric. For \code{method == "edge_collapse"}. Weight for shape costs which penalize collapsing edges that would drastically change the shape of the object.
#' @param sample_weight numeric.For \code{method == "edge_collapse"}. Weight for sampling costs which penalise collapses that would generate prohibitively long edges.
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
#' 7. Optionally, heal the skeleton if there are breaks (\code{nat::stitch_neurons_mst})
#'
#' 8. Optionally, attempts to re-root the neuron at a 'hairball', i.e. approximate the soma (\code{reroot_hairball}).
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
#' neurons = skeletor(ids, brain = elmr::FAFB14.surf)
#' plot3d(neurons) # note, in flywire space
#' plot3d(nx, col="black", lwd  =2) # note, in flywire space
#' }
#' @export
skeletor <- function(segments = NULL,
                     obj = NULL,
                     mesh3d = TRUE,
                     cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                     clean = TRUE,
                     theta = 0.01,
                     radius = TRUE,
                     ratio = .1,
                     SL = 10,
                     WH0 = 2,
                     iter_lim = 4,
                     epsilon=0.05,
                     precision=1e-6,
                     validate = TRUE,
                     method.radii=c("knn","ray"),
                     method=c('vertex_clusters','edge_collapse'),
                     heal=TRUE,
                     heal.k=10L,
                     heal.threshold=Inf,
                     reroot = TRUE,
                     k.soma.search = 10,
                     radius.soma.search = 2500,
                     brain = NULL,
                     n = 5,
                     n_rays = 20,
                     projection = c("sphere", "tangents"),
                     fallback = "knn",
                     sampling_dist=500,
                     cluster_pos = c("median", "center"),
                     shape_weight = 1,
                     sample_weight = 0.1,
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
  projection = match.arg(projection)
  cluster_pos = match.arg(cluster_pos)
  segments = unique(segments)
  py_skel_imports()
  neurons = nat::neuronlist()
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :current/:total eta: :eta",
    total = length(segments), clear = FALSE, show_after = 1)
  for(x in segments){
    pb$tick()
    swc <- tryCatch({
      suppressWarnings(suppressMessages(py_skeletor(x,
                                         mesh3d = mesh3d,
                                         clean = clean,
                                         theta = theta,
                                         radius = radius,
                                         ratio = ratio,
                                         SL = SL,
                                         WH0 = WH0,
                                         iter_lim = iter_lim,
                                         epsilon=epsilon,
                                         precision=precision,
                                         validate = validate,
                                         method.radii=method.radii,
                                         method=method,
                                         heal=heal,
                                         heal.k=heal.k,
                                         heal.threshold=heal.threshold,
                                         reroot = reroot,
                                         k.soma.search = k.soma.search,
                                         radius.soma.search = radius.soma.search,
                                         brain = brain,
                                         n = n,
                                         n_rays = n_rays,
                                         projection = projection,
                                         fallback = fallback,
                                         sampling_dist=sampling_dist,
                                         cluster_pos = cluster_pos,
                                         shape_weight = shape_weight,
                                         sample_weight = sample_weight,
                                         ...)))
      },
      error = function(e) {
        message("Failed: ", x)
        warning(e)
        NULL
      })
    if(!is.null(swc)){
      swc = nat::as.neuronlist(swc)
      attr(swc,"df") = data.frame(id = x)
      names(swc) = gsub("*./","",x)
      neurons = c(neurons, swc)
    }
  }
  diff = length(segments) - length(neurons)
  if(diff>0){
    warning(diff," ids could not be read and converted to skeletons")
  }
  neurons
}

# hidden
py_skel_imports <-function(...){
  cv <- check_cloudvolume_reticulate()
  reticulate::py_run_string("from cloudvolume import CloudVolume", ...)
  reticulate::py_run_string("import skeletor as sk", ...)
  reticulate::py_run_string("import trimesh as tm", ...)
  cv
}

py_cloudvolume <- function(cloudvolume.url=getOption("fafbseg.cloudvolume.url"), ...) {
  py_skel_imports(...)
  reticulate::py_run_string(
    sprintf("vol = CloudVolume('%s', use_https=True)",cloudvolume.url), ...)
}

# hidden
py_skeletor <- function(id,
                        cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                        mesh3d = TRUE,
                        clean = TRUE,
                        theta = 0.01,
                        radius = TRUE,
                        ratio = .1,
                        SL = 10,
                        WH0 = 2,
                        iter_lim = 4,
                        epsilon=0.05,
                        precision=1e-6,
                        validate = TRUE,
                        method.radii=c("knn","ray"),
                        method=c('vertex_clusters','edge_collapse'),
                        heal=TRUE,
                        heal.k=10L,
                        heal.threshold=Inf,
                        reroot = TRUE,
                        k.soma.search = 10,
                        radius.soma.search = 2500,
                        brain = NULL,
                        n = 5,
                        n_rays = 20,
                        projection = c("sphere", "tangents"),
                        fallback = "knn",
                        sampling_dist=500,
                        cluster_pos = c("median", "center"),
                        shape_weight = 1,
                        sample_weight = 0.1,
                        ...){
  stopifnot(length(id)==1)
  method.radii = match.arg(method.radii)
  method = match.arg(method)
  projection = match.arg(projection)
  cluster_pos = match.arg(cluster_pos)
  if(grepl("obj$",id)){
    reticulate::py_run_string(sprintf("m=tm.load_mesh('%s',process=False)",id), ...)
    if(mesh3d){
      mesh = readobj::read.obj(id)
    }
  }else{
    if(is.null(tryCatch(reticulate::py$vol,error=function(e) NULL))){
      py_cloudvolume(cloudvolume.url=cloudvolume.url)
    }
    reticulate::py_run_string(sprintf("id=%s",id), ...)
    reticulate::py_run_string("m = vol.mesh.get(id, deduplicate_chunk_boundaries=False)[id]", ...)
    mesh = NULL
  }
  reticulate::py_run_string("m = tm.Trimesh(m.vertices, m.faces)", ...)
  reticulate::py_run_string(sprintf("simp = sk.simplify(m, ratio=%s)",ratio), ...)
  reticulate::py_run_string(sprintf("cntr = sk.contract(simp, SL=%s, WH0=%s, iter_lim=%s, epsilon=%s, precision=%s, validate=%s, progress=False)",
                                    SL,WH0,iter_lim,epsilon,precision,ifelse(validate,"True","False")), ...)
  skeletonize.params <- if(method=="vertex_clusters"){
    sprintf("sampling_dist=%s, cluster_pos='%s'",sampling_dist,cluster_pos)
  }else{
    sprintf("shape_weight=%s, sample_weight=%s",shape_weight,sample_weight)
  }
  reticulate::py_run_string(sprintf("swc = sk.skeletonize(cntr, method='%s', %s, progress=False)",
                                    method, skeletonize.params), ...)
  if(clean){
    reticulate::py_run_string(sprintf("swc = sk.clean(swc=swc, mesh=simp, theta=%s)", theta), ...)
  }
  if(radius){
   radius.params <- if(method.radii=="knn"){
     sprintf("n=%s", n)
   }else{
     if(fallback=="knn"){
       fallback = "'knn'"
     }
     sprintf("n_rays=%s, projection='%s', fallback=%s", n_rays, projection, fallback)
   }
   reticulate::py_run_string(sprintf("swc['radius'] = sk.radii(swc, simp, method='%s', %s, aggregate='mean')",method.radii, radius.params), ...)
  }else{
    reticulate::py_run_string("swc['radius'] = 0", ...)
  }
  reticulate::py_run_string("for c in ['x', 'y', 'z']: swc[c] = swc[c].astype(int)", ...)
  swc = reticulate::py$swc
  colnames(swc) = c("PointNo","Parent","X","Y","Z","W")
  neuron = nat::as.neuron(swc)
  if(heal){
    neuron = suppressMessages(nat::stitch_neurons_mst(x = neuron, threshold = heal.threshold, k = heal.k))
  }
  if(reroot){
    neuron = reroot_hairball(neuron, k.soma.search = k.soma.search, radius.soma.search = radius.soma.search, brain = brain)
  }
  if(mesh3d){
    if(is.null(mesh)){
      # we need to get python to export it
      savedir <- tempdir()
      ff=file.path(savedir, paste0(id, '.obj'))
      reticulate::py_run_string(sprintf("m.export('%s')",ff), ...)
      # this means that we will have a mesh3d object
      mesh=nat::read.neurons(ff)[[1]]
    }
    neuron$mesh3d = mesh
    class(neuron) = union("neuronmesh", class(neuron))
  }
  neuron$id = id
  neuron
}


#' @rdname skeletor
#' @export
reroot_hairball <- function(x,
                           k.soma.search = 10,
                           radius.soma.search = 2500,
                           brain = NULL){

  # Get end and branch points, as vectors
  e = nat::endpoints(x)
  if(!is.null(brain)){
    pin = !nat::pointsinside(x = x$d, surf = brain)
    ins = 1:nrow(x$d)[pin]
    ee = intersect(e, ins)
    if(length(ee)>=1){
      e=ee
    }
  }
  d = nat::dotprops(x)
  v = d$vect[e,]
  p = d$points[e,]

  # Find new root
  if(length(e)==1){
    root = e
  }else{
    # Find those within range
    near=knn(p, query = p, k = k.soma.search, eps = 0, searchtype = 1L, radius = radius.soma.search)
    idx = near$nn.idx[,-1]
    dists = near$nn.dists[,-1]
    dists[is.infinite(dists)] = radius.soma.search
    rownames(idx) = rownames(v) = rownames(dists) = e
    dists = dists[apply(idx,1,function(r) sum(r>0)>1),]
    idx = idx[apply(idx,1,function(r) sum(r>0)>3),]

    # Asses vector direction
    l = lapply(rownames(idx), function(r) sum(abs(apply(v[idx[r,],],1,function(vr) crossprod3D(vr, v[r,],i=3) ) )))
    u = unlist(l)
    root = rownames(idx)[which.max(u)]
  }

  # Re-root neuron
  somid = x$d$PointNo[match(root, 1:nrow(x$d))]
  y = nat::as.neuron(nat::as.ngraph(x$d), origin = somid)
  y$tags$soma = somid
  y$tags$soma.edit = "estimated"
  y

}

# hidden
crossprod3D <- function(x, y, i=1:3) {
  # Project inputs into 3D, since the cross product only makes sense in 3D.
  To3D <- function(x) utils::head(c(x, rep(0, 3)), 3)
  x <- To3D(x)
  y <- To3D(y)

  # Indices should be treated cyclically (i.e., index 4 is "really" index 1, and
  # so on).  Index3D() lets us do that using R's convention of 1-based (rather
  # than 0-based) arrays.
  Index3D <- function(i) (i - 1) %% 3 + 1

  # The i'th component of the cross product is:
  # (x[i + 1] * y[i + 2]) - (x[i + 2] * y[i + 1])
  # as long as we treat the indices cyclically.
  return (x[Index3D(i + 1)] * y[Index3D(i + 2)] -
            x[Index3D(i + 2)] * y[Index3D(i + 1)])
}
