# Skeletonise flywire neurons

#' Skeletonise neuron meshes using skeletor
#'
#' @description You can skeletonise complex neuron meshes using skeletor
#'   \href{https://github.com/schlegelp/skeletor}{skeletor-1.0.0}. Skeletor is a
#'   python library and this function wraps a series of skeletor functions in
#'   order to smoothly process neurons for use with the
#'   \href{http://natverse.org/}{natverse}. Note, the default settings optimise
#'   performance for fast skeletonisation of
#'   \href{https://ngl.flywire.ai}{flywire} meshes. For casual users we
#'   recommend using the 'wave' method, which is fast and simple in terms of
#'   parameters, i.e. it just uses \code{waves} and \code{step_size}. A value of
#'   1 for both often works well.
#'
#' @param segments The segment ids to fetch (probably as a character vector),
#'   e.g. flywire IDs or hemibrain bodyids. Meshes are read from the specified
#'   CloudVolume (\code{cloudvolume.url}).
#' @param obj character. Path of a \code{obj} file or a folder of such files.
#'   These files are read as meshes and then skeletonised. If \code{segments} is
#'   given, this argument is overridden.
#' @param mesh3d logical. If \code{TRUE} then the neuron's volume is added to
#'   each \code{neuron} object in the resultant \code{neuronlist]} at
#'   \code{neuron$mesh3d}.
#' @param save.obj character. Path to which to save \code{.obj} file for neuron
#'   volumes. If \code{NULL}, .obj files are not saved (default).
#' @param cloudvolume.url Optional url from which to fetch meshes normally
#'   specified by the \code{fafbseg.cloudvolume.url} option.
#' @param operator Which Laplacian operator to use for mesh contraction.
#'   \code{"contangent"} takes topology and geometry of the mesh into account
#'   and so is a better descriptor of curvature flow. The \code{"umbrella"},
#'   'uniform weighting' operator uses only topological features, making it more
#'   robust to mesh flaws.
#' @param clean logical. If \code{TRUE} then, in python,
#'   \code{skeletor.post.clean_up} is used to collapse twigs that have line of
#'   sight to each other and move nodes outside the mesh back inside. Note that
#'   this is not a magic bullet and some of this will not work (well) if the
#'   original mesh was degenerate (e.g. internal faces or not watertight) to
#'   begin with. You will need to have the \code{ncollpyde} python3 module
#'   installed. You can get this with \code{pip3 install ncollpyde}. If you get
#'   issues related to this module, best to set this to \code{FALSE}.
#'   \code{skeletor.pre.fix_mesh} is also used to remove seemingly erroneous
#'   vertices and remove other common mesh problems.
#' @param remove_disconnected, integer or 'False'. If a number is given and
#'   \code{clean==TRUE},\ will iterate over the mesh's connected components and
#'   remove those consisting of less than the given number of vertices. For
#'   example, ``remove_fragments=5`` will drop parts of the mesh that consist of
#'   five or less connected vertices.
#' @param theta numeric. Used if \code{clean=TRUE}. For each twig we generate
#'   the dot product between the tangent vectors of it and its parents. If these
#'   line up perfectly the dot product will equal 1. \code{theta} determines how
#'   much that value can differ from 1 for us to still prune the twig: higher
#'   theta = more pruning.
#' @param radius logical. Whether or not to return radius information for each
#'   skeleton node. If you want to make use of radii, you will need to have the
#'   \code{ncollpyde} python3 module installed. You can get this with \code{pip3
#'   install ncollpyde}. If you get issues related to this module, best to set
#'   this to \code{FALSE}.
#' @param method.radii the method by which to determine each node's radius.
#'   \code{"knn"} uses k-nearest-neighbours to get radii: fast but potential for
#'   being very wrong. \code{"ray"} uses ray-casting to get radii: slower but
#'   sometimes less wrong.
#' @param ratio numeric, 0-1. Factor to which to reduce mesh faces. For example,
#'   a ratio of 0.5 will reduce the number of faces to 50 percent.
#' @param epsilon numeric. Target contraction rate as measured by the sum of all
#'   face areas in the contracted versus the original mesh. Algorithm will stop
#'   once mesh is contracted below this threshold. Depending on your mesh
#'   (number of faces, shape) reaching a strong contraction can be extremely
#'   costly with comparatively little benefit for the subsequent
#'   skeletonisation. Note that the algorithm might stop short of this target if
#'   \code{iter_lim} is reached first or if the sum of face areas is increasing
#'   from one iteration to the next instead of decreasing.
#' @param iter_lim integer. Maximum rounds of contractions.
#' @param precision numeric. Sets the precision for finding the least-square
#'   solution. This is the main determinant for speed vs quality: lower values
#'   will take (much) longer but will get you closer to an optimally contracted
#'   mesh. Higher values will be faster but the iterative contractions might
#'   stop early.
#' @param SL numeric. Factor by which the contraction matrix is multiplied for
#'   each iteration. In theory, lower values are more likely to get you an
#'   optimal contraction at the cost of needing more iterations.
#' @param WH0 numeric. Initial weight factor for the attraction constraints. The
#'   ratio of the initial weights \code{WL0} (\code{1e-3 * sqrt(A)}) and
#'   \code{WH0} controls the smoothness and the degree of contraction of the
#'   first iteration result, thus it determines the amount of details retained
#'   in subsequent and final contracted meshes.
#' @param validate If \code{True}, will try to fix potential issues with the
#'   mesh (e.g. infinite values, duplicate vertices, degenerate faces) before
#'   collapsing. Degenerate meshes can lead to effectively infinite runtime for
#'   this function!
#' @param method Skeletonisation comes in two flavours with different Pros and
#'   Cons. \code{"vertex_clusters"} groups and collapses vertices based on their
#'   geodesic distance along the mesh's surface. It's fast and scales well but
#'   can lead to oversimplification. Good for quick & dirty skeletonisations.
#'   \code{"edge_collapse"} implements skeleton extraction by edge collapse
#'   described Au et al. 2008. It's rather slow and doesn't scale well but is
#'   really good at preserving topology.
#' @param heal logical. Whether or not, if the neuron id fragmented, to stitch
#'   multiple fragments into single neuron using minimum spanning tree.
#' @param heal.threshold numeric. The threshold distance above which new
#'   vertices will not be connected (default=\code{Inf} disables this feature).
#'   This parameter prevents the merging of vertices that are so far away from
#'   the main neuron that they are likely to be spurious.
#' @param heal.k integer. The number of nearest neighbours to consider when
#'   trying to merge different clusters.
#' @param reroot logical. Whether or not to re-root the neuron at an estimated
#'   'soma'. A soma is usually a large ball in the neuron, which will
#'   skeletonise into something of a hair ball. We can try to detect it quickly
#'   and reroot the skeleton there. We do this by finding the nearest leaf nodes
#'   to each leaf node, and seeing if they are going off in divergent
#'   directions.
#' @param k.soma.search integer. The number of leaf nodes to find, around each
#'   leaf node of radius \code{radius.soma.search}, for the rerooting process.
#'   The larger the number, the better but slower.
#' @param radius.soma.search numeric. The distance within which to search for
#'   fellow leaf nodes for the rerooting process. Will be inaccurate at values
#'   that are too high or too low. Should be about the size of the expected
#'   soma.
#' @param reroot_method whether to try to reroot the neuron based on
#'  mixed direction of vectors in the neuron at nearby point('direction') or
#'  proximity of points alone ('density').
#' @param x a \code{nat::neuron} object.
#' @param brain a \code{mesh3d} or \code{hxsurf} object within which a soma
#'   cannot occur. For the re-rooting process. (Insect somata tend to lie
#'   outside the brain proper)
#' @param n For \code{method.radii = "knn"}. Radius will be the mean over
#'   \code{n} nearest-neighbours.
#' @param n_rays integer. For \code{method.radii = "knn"}.For \code{method.radii
#'   = "ray"}. Number of rays to cast for each node.
#' @param projection For \code{method.radii = "ray"}. Whether to cast rays in a
#'   sphere around each node or in a circle orthogonally to the node's tangent
#'   vector.
#' @param fallback For \code{method.radii = "ray"}. If a point is outside or
#'   right on the surface of the mesh the ray casting will return nonsense
#'   results. We can either ignore those cases (\code{"None"}), assign a
#'   arbitrary number or we can fall back to radii from k-nearest-neighbours
#'   (\code{"knn"}).
#' @param waves integer. For \code{method = "wavefront"}. Number of waves to run
#'   across the mesh. Each wave is initialised at a different vertex which
#'   produces slightly different rings. The final skeleton is produced from a
#'   mean across all waves. More waves produce higher resolution skeletons but
#'   also introduce more noise.
#' @param step_size integer, Values greater 1 effectively lead to binning of
#'   rings. For example a stepsize of 2 means that two adjacent vertex rings
#'   will be collapsed to the same center. This can help reduce noise in the
#'   skeleton (and as such counteracts a large number of waves)
#' @param sampling_dist numeric. For \code{method = "vertex_clusters"}. Maximal
#'   distance at which vertices are clustered. This parameter should be tuned
#'   based on the resolution of your mesh.
#' @param cluster_pos numeric. For \code{method = "vertex_clusters"}. How to
#'   determine the x/y/z coordinates of the collapsed vertex clusters (i.e. the
#'   skeleton's nodes). \code{"median"}: Use the vertex closest to cluster's
#'   centrer of mass. \code{"center"}: Use the center of mass. This makes for
#'   smoother skeletons but can lead to nodes outside the mesh.
#' @param shape_weight numeric. For \code{method = "edge_collapse"}. Weight for
#'   shape costs which penalize collapsing edges that would drastically change
#'   the shape of the object.
#' @param sample_weight numeric.For \code{method = "edge_collapse"}. Weight for
#'   sampling costs which penalise collapses that would generate prohibitively
#'   long edges.
#' @param inv_dist numeric.For \code{method = "teasar"}. Distance along the mesh
#'   used for invalidation of vertices. This controls how detailed (or noisy)
#'   the skeleton will be.
#' @param resample stepsize by which to resample a neuron once skeletonised, but
#' before healing or root finding. If \code{NULL}, no resampling attempt is made.
#' @param cpu double (of length one). Set a limit on the total cpu time in
#'   seconds.
#' @param elapsed double (of length one). Set a limit on the total elapsed cpu
#'   time in seconds
#' @param ... Additional arguments passed to \code{reticulate::py_run_string}.
#'
#' @return A \code{nat::neuronlist} containing neuron skeleton objects.
#'
#' @details This pipeline:
#'
#'   1. Reads specified meshes from a CloudVolume source.
#'
#'   2. Simplifies each mesh (python: \code{skeletor.pre.simplify})
#'
#'   3. Contract the mesh (python: \code{skeletor.pre.contract})
#'
#'   4. Skeletonises the mesh (python: \code{skeletor.skeletonize})
#'
#'   5. Optionally, cleans the mesh (python: \code{skeletor.post.clean_up})
#'
#'   6. Optionally, add radius information to the skeleton (python:
#'   \code{skeletor.post.radii})
#'
#'   7. Optionally, heal the skeleton if there are breaks
#'   (\code{nat::stitch_neurons_mst})
#'
#'   8. Optionally, attempts to re-root the neuron at a 'hairball', i.e.
#'   approximate the soma (\code{reroot_hairball}).
#'
#'   You will therefore need to have a working python3 install of skeletor,
#'   which uses CloudVolume. You do not require meshparty. Please install the
#'   Python skeletor module as described at:
#'   \url{https://github.com/schlegelp/skeletor}. You must ensure that you are
#'   using python3 (implicitly or explicitly) as mesh fetching from graphene
#'   servers depends on this. This should normally work: \code{pip3 install
#'   git+git://github.com/schlegelp/skeletor@master}. If you have already
#'   installed skeletor but it is not found, then I recommend editing your
#'   \code{\link{Renviron}} file to set an environment variable pointing to the
#'   correct Python. You can do this with \code{usethis::edit_r_environ()} and
#'   then setting e.g. \code{RETICULATE_PYTHON="/usr/local/bin/python3"}.
#'   (Though best practice would be to create a conda environment for your
#'   natverse R sessions and direct R there using your environ file.)
#'
#'   You will need to set up some kind of authentication in order to fetch
#'   volume data for skeletonisation. See
#'   \url{https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson} for
#'   how to get a token and where to save it. You can either save a json snippet
#'   to \code{~/.cloudvolume/secrets/cave-secret.json} or set an
#'   environment variable (\code{CHUNKEDGRAPH_SECRET="XXXX"}.
#'
#'   Finally you will also need to set an option pointing to your server. This
#'   is the server hosting the mesh data you are interested in. This might look
#'   something like:
#'   \code{options(fafbseg.cloudvolume.url='graphene://https://xxx.dynamicannotationframework.com/segmentation/xxx/xxx')}
#'    and you can easily add this to your startup \code{\link{Rprofile}} with
#'   \code{usethis::edit_r_profile()}. For example, for the flywire data set, it
#'   is currently:
#'   \code{'graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31'}.
#'
#'
#'   Roughly in decreasing order of impact on speed:
#'
#'   Ratio: lower ratio = less vertices = faster
#'
#'   epsilon: lower target contraction rate = less steps = faster
#'
#'   SL: faster contraction = pot. less steps to target contraction rate =
#'   faster
#'
#'   precision: lower precision = faster least-square computation = faster
#'
#'   sampling_dist: larger dist = faster collapse of mesh into skeleton = faster
#'
#' @seealso \code{\link{simple_python}} for installation of the necessary Python
#'   packages.
#' @examples
#' \dontrun{
#' choose_segmentation("flywire")
#' nx=xform_brain(elmr::dense_core_neurons, ref="FlyWire", sample="FAFB14")
#' xyz = xyzmatrix(nx)
#' ids = unique(flywire_xyz2id(xyz[sample(1:nrow(xyz),100),]))
#' neurons = skeletor(ids, brain = elmr::FAFB14.surf)
#' plot3d(neurons) # note, in flywire space
#' plot3d(nx, col="black", lwd  =2) # note, in flywire space
#'
#' # Also plot their meshes
#' neuron.meshes = read_cloudvolume_meshes(ids)
#'
#'
#' # We can also just save the .obj files
#' dir.create("obj")
#' download_neuron_obj(ids, save.obj = "obj")
#'
#' # remove
#' unlink("obj", recurvise = TRUE)
#'
#' }
#' @export
skeletor <- function(segments = NULL,
                     obj = NULL,
                     mesh3d = FALSE,
                     save.obj = NULL,
                     cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                     operator = c("umbrella","contangent"),
                     clean = FALSE,
                     remove_disconnected=10,
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
                     method=c('wavefront','vertex_clusters','edge_collapse','teasar','tangent_ball'),
                     heal=TRUE,
                     heal.k=10L,
                     heal.threshold=Inf,
                     reroot = TRUE,
                     k.soma.search = 10,
                     radius.soma.search = 2500,
                     reroot_method = c("direction","density"),
                     brain = NULL,
                     n = 5,
                     n_rays = 20,
                     projection = c("sphere", "tangents"),
                     fallback = "knn",
                     waves=2,
                     step_size=1,
                     sampling_dist=500,
                     cluster_pos = c("median", "center"),
                     shape_weight = 1,
                     sample_weight = 0.1,
                     inv_dist = 100,
                     cpu = Inf,
                     elapsed = Inf,
                     resample = NULL,
                    ...){
  if(is.null(segments)&&is.null(obj)){
    stop("Either the argument segments or obj must be given.")
  }else if(!inherits(segments,c("character","integer64","integer"))&&!inherits(obj,c("character","integer64","integer"))){
    stop("segments/obj must be a character vector")
  }
  msg="downloading & processing"
  if(!is.null(obj)){
    if(!grepl(".obj$",obj)){
      obj = list.files(obj,pattern = "obj$", full.names = TRUE)
    }
    if(length(obj)==0){
      stop("No .obj files could be found")
    }else{
      segments=obj
      msg="processing"
    }
  }
  method.radii = match.arg(method.radii)
  method = match.arg(method)
  projection = match.arg(projection)
  cluster_pos = match.arg(cluster_pos)
  reroot_method = match.arg(reroot_method)
  segments = unique(segments)
  py_skel_imports()
  py_cloudvolume(cloudvolume.url) # ...
  neurons = nat::neuronlist()
  pb <- progress::progress_bar$new(
    format = sprintf("  %s [:bar] :current/:total eta: :eta", msg),
    total = length(segments), clear = FALSE, show_after = 1)
  for(x in segments){
    pb$tick()
    swc <- tryCatch({
      try_with_time_limit(suppressWarnings(suppressMessages(py_skeletor(x,
                                         cloudvolume.url=cloudvolume.url,
                                         mesh3d = mesh3d,
                                         save.obj = save.obj,
                                         operator = operator,
                                         clean = clean,
                                         remove_disconnected=remove_disconnected,
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
                                         reroot_method = reroot_method,
                                         brain = brain,
                                         n = n,
                                         n_rays = n_rays,
                                         projection = projection,
                                         fallback = fallback,
                                         waves=waves,
                                         step_size=step_size,
                                         sampling_dist=sampling_dist,
                                         cluster_pos = cluster_pos,
                                         shape_weight = shape_weight,
                                         sample_weight = sample_weight,
                                         inv_dist = inv_dist,
                                         resample = resample,
                                         ...))),
                          cpu = cpu,
                          elapsed = elapsed)
      },
      error = function(e) {
        message(as.character(e))
        NULL
      })
    if(!is.null(swc)){
      swc = nat::as.neuronlist(swc)
      attr(swc,"df") = data.frame(id = x)
      names(swc) = gsub("*./","",x)
      neurons = c(neurons, swc)
    }else{
      message("Failed: ", x)
    }
  }
  diff = length(segments) - length(neurons)
  if(diff>0){
    warning(diff," ids could not be read and converted to skeletons")
  }
  neurons
}

# hidden
try_with_time_limit <- function(expr, cpu = Inf, elapsed = Inf, error = NULL){
  y <- try({setTimeLimit(cpu, elapsed, transient = TRUE); expr}, silent = TRUE)
  clear <- gc()
  if(inherits(y, "try-error")){
    warning(y)
    error
  }else{
    y
  }
}

# hidden
py_skel_imports <-function(...){
  cv <- check_cloudvolume_reticulate()
  reticulate::py_run_string("from cloudvolume import CloudVolume", ...)
  reticulate::py_run_string("import skeletor as sk", ...)
  reticulate::py_run_string("import trimesh as tm", ...)
  cv
}

# hidden
py_cloudvolume <- function(cloudvolume.url=getOption("fafbseg.cloudvolume.url"), ...) {
  py_skel_imports(...)
  reticulate::py_run_string(
    sprintf("vol = CloudVolume('%s', use_https=True)",cloudvolume.url), ...)
}

# hidden
py_skeletor <- function(id,
                        cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                        mesh3d = FALSE,
                        save.obj = NULL,
                        operator = c("umbrella","contangent"),
                        clean = FALSE,
                        remove_disconnected=10,
                        theta = 0.01,
                        radius = TRUE,
                        ratio = .2,
                        SL = 10,
                        WH0 = 2,
                        iter_lim = 4,
                        epsilon=0.05,
                        precision=1e-6,
                        validate = TRUE,
                        method.radii=c("knn","ray"),
                        method=c('wavefront','vertex_clusters','edge_collapse','teasar','tangent_ball'),
                        heal=TRUE,
                        heal.k=10L,
                        heal.threshold=Inf,
                        reroot = TRUE,
                        k.soma.search = 10,
                        radius.soma.search = 2500,
                        reroot_method = c('direction','density'),
                        brain = NULL,
                        n = 5,
                        n_rays = 20,
                        projection = c("sphere", "tangents"),
                        fallback = "knn",
                        waves=2,
                        step_size=1,
                        sampling_dist=500,
                        cluster_pos = c("median", "center"),
                        shape_weight = 1,
                        sample_weight = 0.1,
                        inv_dist = 100,
                        resample = NULL,
                        ...){
  stopifnot(length(id)==1)
  operator = match.arg(operator)
  method.radii = match.arg(method.radii)
  method = match.arg(method)
  projection = match.arg(projection)
  cluster_pos = match.arg(cluster_pos)
  reroot_method = match.arg(reroot_method)
  if(grepl("obj$",id)){
    obj.file = TRUE
    reticulate::py_run_string(sprintf("m=tm.load_mesh('%s',process=False)",id), ...)
    if(mesh3d){
      mesh = readobj::read.obj(id)
    }
  }else{
    obj.file = FALSE
    if(is.null(tryCatch(reticulate::py$vol,error=function(e){
      warning(e)
      NULL
    }))){
      py_cloudvolume(cloudvolume.url=cloudvolume.url)
    }
    reticulate::py_run_string(sprintf("id=%s",id)) # ..
    # this can error out with a bad connection to the server
    counter = 3 # we'll try 3 times
    while(counter>0){
      res = try(reticulate::py_run_string("m = vol.mesh.get(id, deduplicate_chunk_boundaries=False)[id]", ...), silent=TRUE)
      if(class(res)[1]=="try-error"){
        if(grepl("HTTPError|Server",res)){
          counter = counter - 1
          Sys.sleep(1)
        }else{
          warning(res)
          break
        }
      }else{
        break
      }
    }
    if(class(res)[1]=="try-error"){
      stop(res)
    }
    mesh = NULL
  }
  reticulate::py_run_string("m = sk.utilities.make_trimesh(m, validate=False)", ...)
  if(clean){
    reticulate::py_run_string(sprintf("m = sk.pre.fix_mesh(mesh=m, remove_disconnected=%s, inplace=True)", remove_disconnected), ...)
    if(method!="wavefront"){
      reticulate::py_run_string(sprintf("m = sk.pre.simplify(m, ratio=%s)",ratio), ...)
    }
  }
  if(method %in% c("vertex_clusters","edge_collapse")){
    reticulate::py_run_string(sprintf("m = sk.pre.contract(m, SL=%s, WH0=%s, iter_lim=%s, epsilon=%s, precision=%s, validate=%s, operator='%s', progress=False)",
                                      SL,WH0,iter_lim,epsilon,precision,ifelse(validate,"True","False"), operator),...)
  }
  skeletonize.params <- if(method=="vertex_clusters"){
    sprintf("sampling_dist=%s, cluster_pos='%s'",sampling_dist,cluster_pos)
  }else if (method=="edge_collapse"){
    sprintf("shape_weight=%s, sample_weight=%s",shape_weight,sample_weight)
  }else if (method=="wavefront"){
    sprintf("waves=%s, step_size=%s",waves,step_size)
  }else if (method=="teasar"){
    sprintf("inv_dist=%s",inv_dist)
  }else{
    NULL
  }
  reticulate::py_run_string(sprintf("swc = sk.skeletonize.by_%s(mesh=m, %s, progress=False)",
                                    method, skeletonize.params), ...)
  if(clean && method !="wavefront"){
    reticulate::py_run_string(sprintf("swc = sk.post.clean_up(s=swc, mesh=m, theta=%s)", theta), ...)
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
   reticulate::py_run_string(sprintf("sk.post.radii(s=swc, mesh=m, method='%s', %s, aggregate='mean')",method.radii, radius.params), ...)
  }else{
    reticulate::py_run_string("swc['radius'] = 0", ...)
  }
  skel = reticulate::py$swc
  swc = skel$swc
  colnames(swc) = c("PointNo","Parent","X","Y","Z","W")
  neuron = nat::as.neuron(swc)
  if(!is.null(resample)){
    neuron <- nat::resample(neuron, stepsize = resample)
  }
  if(neuron$nTree>1){
    if(heal){
      neuron = suppressMessages(nat::stitch_neurons_mst(x = neuron,
                                                        threshold = heal.threshold,
                                                        k = heal.k))
    }else{
      neuron = subtree(neuron)
    }
  }
  if(reroot){
    neuron = tryCatch(reroot_hairball(neuron,
                                      k.soma.search = k.soma.search,
                                      radius.soma.search = radius.soma.search,
                                      reroot_method = reroot_method,
                                      brain = brain),
                      error = function(e){
                        warning(e)
                        neuron
                      })
  }
  if(mesh3d|!is.null(save.obj)){
      # we need to get python to export it
      savedir <- if(!is.null(save.obj)){
        save.obj
      }else{
        dir.create(td<-tempfile())
        on.exit(unlink(td, recursive=TRUE))
        td
      }
      if(obj.file){
        ff = id
      }else{
        ff=file.path(savedir, paste0(id, '.obj'))
      }
      reticulate::py_run_string(sprintf("s = m.export('%s')",ff), ...)
      # this means that we will have a mesh3d object
      if(mesh3d){
        mesh=nat::read.neurons(ff)[[1]]
        neuron$mesh3d = mesh
        class(neuron) = union("neuronmesh", class(neuron))
      }
  }
  neuron$id = basename(gsub("\\.obj","",id))
  neuron
}


#' @rdname skeletor
#' @export
reroot_hairball <- function(x,
                           k.soma.search = 10,
                           radius.soma.search = 2500,
                           brain = NULL,
                           reroot_method = c("direction","density")){

  # Get end and branch points, as vectors
  reroot_method = match.arg(reroot_method)
  e = nat::endpoints(x)
  if(!is.null(brain)){
    pin = !nat::pointsinside(x = x$d, surf = brain)
    pin[is.na(pin)|is.infinite(pin)|is.nan(pin)] = FALSE
    ins = c(1:nrow(x$d))[pin]
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
    if(reroot_method=='direction'){
      l = lapply(rownames(idx), function(r) sum(abs(apply(v[idx[r,],],1,function(vr) crossprod3D(vr, v[r,],i=3) ) )))
      u = unlist(l)
      root = rownames(idx)[which.max(u)]
    }else{
      # Point with the most near points
      a=apply(dists,1,sum)
      root=names(which.min(a))
    }
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

#' @rdname skeletor
#' @export
download_neuron_obj <- function(segments,
                                save.obj = getwd(),
                                ratio = 1,
                                cloudvolume.url=getOption("fafbseg.cloudvolume.url"),
                                ...){
  py_skel_imports()
  py_cloudvolume(cloudvolume.url, ...)
  neurons = nat::neuronlist()
  message("Saving .obj files in: ", save.obj)
  pb <- progress::progress_bar$new(
    format = "  downloading [:bar] :current/:total eta: :eta",
    total = length(segments), clear = FALSE, show_after = 1)
  for(id in segments){
    pb$tick()
    reticulate::py_run_string(sprintf("id=%s",id), ...)
    counter = 3 # we'll try 3 times
    while(counter>0){
      res = try(reticulate::py_run_string("m = vol.mesh.get(id, deduplicate_chunk_boundaries=False)[id]", ...), silent=TRUE)
      if(class(res)[1]=="try-error"){
        if(grepl("HTTPError|Server",res)){
          counter = counter - 1
          Sys.sleep(1)
        }else{
          break
        }
      }else{
        break
      }
    }
    if(class(res)[1]=="try-error"){
      warning(as.character(res))
    }else{
      reticulate::py_run_string("m = tm.Trimesh(m.vertices, m.faces)", ...)
      if(ratio!=1){
        reticulate::py_run_string(sprintf("m = sk.pre.simplify(m, ratio=%s)",ratio), ...)
      }
      ff=file.path(save.obj, paste0(id, '.obj'))
      reticulate::py_run_string(sprintf("s = m.export('%s')",ff), ...)
    }
  }
}

#' Get flywire IDs that map onto CATMAID neurons
#'
#' @description Provide this function with a CATMAID query (skeleton IDs or an
#'   annotation term, as you could provide to \code{catmaid::catmaid_skids}) or
#'   a \code{nat::neuronlist} object in FAFB14 space, and it will return a
#'   ranked list of flywire IDs that map onto the given neuron(s).
#'
#' @param search one or more skids or a CATMAID query expression. Else, a
#'   neuronlist of neurons in FAFB14 space.
#' @param only.root only return one \code{root_id} at the location of the root node
#' of the given CATMAID neuron(s)..
#' @param only.biggest only return one \code{root_id} per CATMAID \code{skid}
#' i.e. the biggest overlapping fragment.
#' @param OmitFailures logical, whether to omit neurons that cannot be read from CATMAID.
#' @param ... Additional arguments passed to \code{nat::nlapply}.
#'
#' @inheritParams catmaid::read.neuron.catmaid
#'
#' @return A \code{data.frame} of ranked flywire IDs, and the number of points
#'   in each CATMAID neuron that maps to that ID.
#'
#' @examples
#' \dontrun{
#' # a specific skid
#' df=fafb14_to_flywire_ids(16)
#' head(df)
#'
#' # Get neurons from a specific CATMAID environment
#' ## See catmaid package help for  details on how to 'login'
#'
#' # This is the Drosophila anatomy ontology identifier for DL1
#' # adult antennal lobe projection neuron DL1 adPN
#' # see \url{https://virtualflybrain.org} for details.
#' hits=fafb14_to_flywire_ids(search="FBbt:00067353", conn=catmaid::vfbcatmaid())
#' head(hits)
#' }
#' @export
fafb14_to_flywire_ids <- function(search,
                                  only.root = FALSE,
                                  only.biggest = FALSE,
                                  pid = 1L,
                                  conn = NULL,
                                  fetch.annotations = FALSE,
                                  OmitFailures = FALSE,
                                  ...){
  if(nat::is.neuronlist(search)){
    neurons = search
  }else if(nat::is.neuron(search)){
    neurons = nat::as.neuronlist(search)
  }else{
    skids = catmaid::catmaid_skids(search, pid=pid, conn=conn, several.ok=TRUE)
    neurons = nlapply(skids, robust_read_catmaid_neuron, pid=pid, conn=conn, OmitFailures = OmitFailures)
    neurons = neurons[unlist(sapply(neurons,nat::is.neuron))]
  }
  fw.df = nat::nlapply(X = neurons, FUN = fafb14_to_flywire_ids_timed.neuron, only.root=only.root, only.biggest=only.biggest, OmitFailures = OmitFailures, ...)
  df = do.call(rbind, fw.df)
  if(!only.root){
    df = df[order(df$hits, decreasing = TRUE),]
  }
  rownames(df) = NULL
  df
}

# private function capable of dealing with single node neurons
robust_read_catmaid_neuron <- function(skid, pid = 1L, conn = NULL, ...) {
  n=try(catmaid::read.neuron.catmaid(skid, pid=pid, conn=conn, ...), silent = T)
  if(inherits(n, 'try-error')) {
    r=catmaid::catmaid_get_compact_skeleton(skid, pid=pid, conn=conn, ...)
    colnames(r$nodes)[4:6]=c("X","Y","Z")
    n=structure(list(d=r$nodes, skid=skid, StartPoint=1), class='neuron')
  }
  n
}


# hidden
fafb14_to_flywire_ids.neuron <- function(x,
                                         only.root = FALSE,
                                         only.biggest = FALSE
){
  count = 0
  if(only.root){
    pos.orig = nat::xyzmatrix(x)
    pos = matrix(pos.orig[nat::rootpoints(x),], ncol = 3)
  }else{
    pos = nat::xyzmatrix(x)
  }
  fw.xyz = nat.templatebrains::xform_brain(pos, sample = "FAFB14", reference = "FlyWire", OmitFailures = FALSE, Verbose=FALSE)
  fw.ids = suppressWarnings(flywire_xyz2id(fw.xyz, rawcoords = FALSE))
  if (only.root){
    if(fw.ids == "0"){
      while (count < 10 & fw.ids=="0"){
        p = sample(1:nrow(pos.orig),1)
        pos = matrix(pos.orig[p,], ncol = 3)
        fw.xyz = nat.templatebrains::xform_brain(pos, sample = "FAFB14", reference = "FlyWire", OmitFailures = FALSE, Verbose=FALSE)
        fw.ids = suppressWarnings(flywire_xyz2id(fw.xyz, rawcoords = FALSE))
        count = count + 1
      }
    }
  }else{
    fw.ids = fw.ids[!fw.ids%in%"0"]
  }
  df = as.data.frame(table(as.character(fw.ids)), stringsAsFactors = FALSE)
  df = df[order(df$Freq, decreasing = TRUE),]
  skid=as.character(x$skid)
  df$skid = ifelse(length(skid), skid, NA_character_)
  colnames(df) = c("root_id","hits","skid")
  if(only.biggest){
    df=df[1,]
  }else if (only.root){
    df$fw.xyz = paste_coords(fw.xyz)
    if(count>0){
      df$hits = "other"
    }else{
      df$hits = "root"
    }
  }
  df
}

# hidden
fafb14_to_flywire_ids_timed.neuron <- function(x=x, only.root = FALSE, only.biggest=FALSE, cpu = Inf, elapsed = 3000, error = NA){
  try_with_time_limit(fafb14_to_flywire_ids.neuron(x,only.root=only.root,only.biggest=only.biggest), cpu = cpu, elapsed = elapsed)
}

# hidden
paste_coords <- function(xyz, sep = ", ", brackets = TRUE){
  paste0(ifelse(brackets,"(",NULL),paste(xyz,sep=sep,collapse=sep),ifelse(brackets,")",NULL))
}

# hidden
subtree <- function(neuron, subtree = 1){
  if(is.null(neuron$nTrees)){
    return(neuron)
  }
  if(neuron$nTrees>1){
    v = unique(unlist(neuron$SubTrees[subtree]))
    neuron = nat::prune_vertices(neuron, verticestoprune = v, invert = TRUE)
  }
  neuron
}

# hidden
#' @importFrom nat neuronlist as.neuronlist
subtree.neuronlist <- function(someneuronlist, subtree = 1){
  neurons.fragments = neuronlist()
  for(id in names(someneuronlist)){
    neuron = someneuronlist[id][[1]]
    df = someneuronlist[id,]
    for(t in 1:neuron$nTrees){
      if(length(unlist(neuron$SubTrees[t]))>1){
        subt = subtree(neuron, subtree = t)
      }else{
        subt = neuron
      }
      subt = as.neuronlist(subt)
      attr(subt,"df") = df
      names(subt) = paste0(id,"_",t)
      neurons.fragments = c(neurons.fragments, subt)
    }
  }
  neurons.fragments
}
