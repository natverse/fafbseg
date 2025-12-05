# Skeletonise neuron meshes using skeletor

You can skeletonise complex neuron meshes using skeletor
[skeletor-1.0.0](https://github.com/schlegelp/skeletor). Skeletor is a
python library and this function wraps a series of skeletor functions in
order to smoothly process neurons for use with the
[natverse](http://natverse.org/). Note, the default settings optimise
performance for fast skeletonisation of
[flywire](https://ngl.flywire.ai) meshes. For casual users we recommend
using the 'wave' method, which is fast and simple in terms of
parameters, i.e. it just uses `waves` and `step_size`. A value of 1 for
both often works well.

## Usage

``` r
skeletor(
  segments = NULL,
  obj = NULL,
  mesh3d = FALSE,
  save.obj = NULL,
  cloudvolume.url = getOption("fafbseg.cloudvolume.url"),
  operator = c("umbrella", "contangent"),
  clean = FALSE,
  remove_disconnected = 10,
  theta = 0.01,
  radius = TRUE,
  ratio = 0.1,
  SL = 10,
  WH0 = 2,
  iter_lim = 4,
  epsilon = 0.05,
  precision = 1e-06,
  validate = TRUE,
  method.radii = c("knn", "ray"),
  method = c("wavefront", "vertex_clusters", "edge_collapse", "teasar", "tangent_ball"),
  heal = TRUE,
  heal.k = 10L,
  heal.threshold = Inf,
  reroot = TRUE,
  k.soma.search = 10,
  radius.soma.search = 2500,
  reroot_method = c("direction", "density"),
  brain = NULL,
  n = 5,
  n_rays = 20,
  projection = c("sphere", "tangents"),
  fallback = "knn",
  waves = 2,
  step_size = 1,
  sampling_dist = 500,
  cluster_pos = c("median", "center"),
  shape_weight = 1,
  sample_weight = 0.1,
  inv_dist = 100,
  cpu = Inf,
  elapsed = Inf,
  resample = NULL,
  ...
)

reroot_hairball(
  x,
  k.soma.search = 10,
  radius.soma.search = 2500,
  brain = NULL,
  reroot_method = c("direction", "density")
)

download_neuron_obj(
  segments,
  save.obj = getwd(),
  ratio = 1,
  cloudvolume.url = getOption("fafbseg.cloudvolume.url"),
  ...
)
```

## Arguments

- segments:

  The segment ids to fetch (probably as a character vector), e.g.
  flywire IDs or hemibrain bodyids. Meshes are read from the specified
  CloudVolume (`cloudvolume.url`).

- obj:

  character. Path of a `obj` file or a folder of such files. These files
  are read as meshes and then skeletonised. If `segments` is given, this
  argument is overridden.

- mesh3d:

  logical. If `TRUE` then the neuron's volume is added to each `neuron`
  object in the resultant `neuronlist]` at `neuron$mesh3d`.

- save.obj:

  character. Path to which to save `.obj` file for neuron volumes. If
  `NULL`, .obj files are not saved (default).

- cloudvolume.url:

  Optional url from which to fetch meshes normally specified by the
  `fafbseg.cloudvolume.url` option.

- operator:

  Which Laplacian operator to use for mesh contraction. `"contangent"`
  takes topology and geometry of the mesh into account and so is a
  better descriptor of curvature flow. The `"umbrella"`, 'uniform
  weighting' operator uses only topological features, making it more
  robust to mesh flaws.

- clean:

  logical. If `TRUE` then, in python, `skeletor.post.clean_up` is used
  to collapse twigs that have line of sight to each other and move nodes
  outside the mesh back inside. Note that this is not a magic bullet and
  some of this will not work (well) if the original mesh was degenerate
  (e.g. internal faces or not watertight) to begin with. You will need
  to have the `ncollpyde` python3 module installed. You can get this
  with `pip3 install ncollpyde`. If you get issues related to this
  module, best to set this to `FALSE`. `skeletor.pre.fix_mesh` is also
  used to remove seemingly erroneous vertices and remove other common
  mesh problems.

- remove_disconnected, :

  integer or 'False'. If a number is given and `clean==TRUE`,\\ will
  iterate over the mesh's connected components and remove those
  consisting of less than the given number of vertices. For example,
  “remove_fragments=5“ will drop parts of the mesh that consist of five
  or less connected vertices.

- theta:

  numeric. Used if `clean=TRUE`. For each twig we generate the dot
  product between the tangent vectors of it and its parents. If these
  line up perfectly the dot product will equal 1. `theta` determines how
  much that value can differ from 1 for us to still prune the twig:
  higher theta = more pruning.

- radius:

  logical. Whether or not to return radius information for each skeleton
  node. If you want to make use of radii, you will need to have the
  `ncollpyde` python3 module installed. You can get this with
  `pip3 install ncollpyde`. If you get issues related to this module,
  best to set this to `FALSE`.

- ratio:

  numeric, 0-1. Factor to which to reduce mesh faces. For example, a
  ratio of 0.5 will reduce the number of faces to 50 percent.

- SL:

  numeric. Factor by which the contraction matrix is multiplied for each
  iteration. In theory, lower values are more likely to get you an
  optimal contraction at the cost of needing more iterations.

- WH0:

  numeric. Initial weight factor for the attraction constraints. The
  ratio of the initial weights `WL0` (`1e-3 * sqrt(A)`) and `WH0`
  controls the smoothness and the degree of contraction of the first
  iteration result, thus it determines the amount of details retained in
  subsequent and final contracted meshes.

- iter_lim:

  integer. Maximum rounds of contractions.

- epsilon:

  numeric. Target contraction rate as measured by the sum of all face
  areas in the contracted versus the original mesh. Algorithm will stop
  once mesh is contracted below this threshold. Depending on your mesh
  (number of faces, shape) reaching a strong contraction can be
  extremely costly with comparatively little benefit for the subsequent
  skeletonisation. Note that the algorithm might stop short of this
  target if `iter_lim` is reached first or if the sum of face areas is
  increasing from one iteration to the next instead of decreasing.

- precision:

  numeric. Sets the precision for finding the least-square solution.
  This is the main determinant for speed vs quality: lower values will
  take (much) longer but will get you closer to an optimally contracted
  mesh. Higher values will be faster but the iterative contractions
  might stop early.

- validate:

  If `True`, will try to fix potential issues with the mesh (e.g.
  infinite values, duplicate vertices, degenerate faces) before
  collapsing. Degenerate meshes can lead to effectively infinite runtime
  for this function!

- method.radii:

  the method by which to determine each node's radius. `"knn"` uses
  k-nearest-neighbours to get radii: fast but potential for being very
  wrong. `"ray"` uses ray-casting to get radii: slower but sometimes
  less wrong.

- method:

  Skeletonisation comes in two flavours with different Pros and Cons.
  `"vertex_clusters"` groups and collapses vertices based on their
  geodesic distance along the mesh's surface. It's fast and scales well
  but can lead to oversimplification. Good for quick & dirty
  skeletonisations. `"edge_collapse"` implements skeleton extraction by
  edge collapse described Au et al. 2008. It's rather slow and doesn't
  scale well but is really good at preserving topology.

- heal:

  logical. Whether or not, if the neuron id fragmented, to stitch
  multiple fragments into single neuron using minimum spanning tree.

- heal.k:

  integer. The number of nearest neighbours to consider when trying to
  merge different clusters.

- heal.threshold:

  numeric. The threshold distance above which new vertices will not be
  connected (default=`Inf` disables this feature). This parameter
  prevents the merging of vertices that are so far away from the main
  neuron that they are likely to be spurious.

- reroot:

  logical. Whether or not to re-root the neuron at an estimated 'soma'.
  A soma is usually a large ball in the neuron, which will skeletonise
  into something of a hair ball. We can try to detect it quickly and
  reroot the skeleton there. We do this by finding the nearest leaf
  nodes to each leaf node, and seeing if they are going off in divergent
  directions.

- k.soma.search:

  integer. The number of leaf nodes to find, around each leaf node of
  radius `radius.soma.search`, for the rerooting process. The larger the
  number, the better but slower.

- radius.soma.search:

  numeric. The distance within which to search for fellow leaf nodes for
  the rerooting process. Will be inaccurate at values that are too high
  or too low. Should be about the size of the expected soma.

- reroot_method:

  whether to try to reroot the neuron based on mixed direction of
  vectors in the neuron at nearby point('direction') or proximity of
  points alone ('density').

- brain:

  a `mesh3d` or `hxsurf` object within which a soma cannot occur. For
  the re-rooting process. (Insect somata tend to lie outside the brain
  proper)

- n:

  For `method.radii = "knn"`. Radius will be the mean over `n`
  nearest-neighbours.

- n_rays:

  integer. For `method.radii = "knn"`.For `method.radii = "ray"`. Number
  of rays to cast for each node.

- projection:

  For `method.radii = "ray"`. Whether to cast rays in a sphere around
  each node or in a circle orthogonally to the node's tangent vector.

- fallback:

  For `method.radii = "ray"`. If a point is outside or right on the
  surface of the mesh the ray casting will return nonsense results. We
  can either ignore those cases (`"None"`), assign a arbitrary number or
  we can fall back to radii from k-nearest-neighbours (`"knn"`).

- waves:

  integer. For `method = "wavefront"`. Number of waves to run across the
  mesh. Each wave is initialised at a different vertex which produces
  slightly different rings. The final skeleton is produced from a mean
  across all waves. More waves produce higher resolution skeletons but
  also introduce more noise.

- step_size:

  integer, Values greater 1 effectively lead to binning of rings. For
  example a stepsize of 2 means that two adjacent vertex rings will be
  collapsed to the same center. This can help reduce noise in the
  skeleton (and as such counteracts a large number of waves)

- sampling_dist:

  numeric. For `method = "vertex_clusters"`. Maximal distance at which
  vertices are clustered. This parameter should be tuned based on the
  resolution of your mesh.

- cluster_pos:

  numeric. For `method = "vertex_clusters"`. How to determine the x/y/z
  coordinates of the collapsed vertex clusters (i.e. the skeleton's
  nodes). `"median"`: Use the vertex closest to cluster's centrer of
  mass. `"center"`: Use the center of mass. This makes for smoother
  skeletons but can lead to nodes outside the mesh.

- shape_weight:

  numeric. For `method = "edge_collapse"`. Weight for shape costs which
  penalize collapsing edges that would drastically change the shape of
  the object.

- sample_weight:

  numeric.For `method = "edge_collapse"`. Weight for sampling costs
  which penalise collapses that would generate prohibitively long edges.

- inv_dist:

  numeric.For `method = "teasar"`. Distance along the mesh used for
  invalidation of vertices. This controls how detailed (or noisy) the
  skeleton will be.

- cpu:

  double (of length one). Set a limit on the total cpu time in seconds.

- elapsed:

  double (of length one). Set a limit on the total elapsed cpu time in
  seconds

- resample:

  stepsize by which to resample a neuron once skeletonised, but before
  healing or root finding. If `NULL`, no resampling attempt is made.

- ...:

  Additional arguments passed to
  [`reticulate::py_run_string`](https://rstudio.github.io/reticulate/reference/py_run.html).

- x:

  a [`nat::neuron`](https://rdrr.io/pkg/nat/man/neuron.html) object.

## Value

A [`nat::neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)
containing neuron skeleton objects.

## Details

This pipeline:

1\. Reads specified meshes from a CloudVolume source.

2\. Simplifies each mesh (python: `skeletor.pre.simplify`)

3\. Contract the mesh (python: `skeletor.pre.contract`)

4\. Skeletonises the mesh (python: `skeletor.skeletonize`)

5\. Optionally, cleans the mesh (python: `skeletor.post.clean_up`)

6\. Optionally, add radius information to the skeleton (python:
`skeletor.post.radii`)

7\. Optionally, heal the skeleton if there are breaks
([`nat::stitch_neurons_mst`](https://rdrr.io/pkg/nat/man/stitch_neurons_mst.html))

8\. Optionally, attempts to re-root the neuron at a 'hairball', i.e.
approximate the soma (`reroot_hairball`).

You will therefore need to have a working python3 install of skeletor,
which uses CloudVolume. You do not require meshparty. Please install the
Python skeletor module as described at:
<https://github.com/schlegelp/skeletor>. You must ensure that you are
using python3 (implicitly or explicitly) as mesh fetching from graphene
servers depends on this. This should normally work:
`pip3 install git+git://github.com/schlegelp/skeletor@master`. If you
have already installed skeletor but it is not found, then I recommend
editing your [`Renviron`](https://rdrr.io/r/base/Startup.html) file to
set an environment variable pointing to the correct Python. You can do
this with
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
and then setting e.g. `RETICULATE_PYTHON="/usr/local/bin/python3"`.
(Though best practice would be to create a conda environment for your
natverse R sessions and direct R there using your environ file.)

You will need to set up some kind of authentication in order to fetch
volume data for skeletonisation. See
<https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson> for
how to get a token and where to save it. You can either save a json
snippet to `~/.cloudvolume/secrets/cave-secret.json` or set an
environment variable (`CHUNKEDGRAPH_SECRET="XXXX"`.

Finally you will also need to set an option pointing to your server.
This is the server hosting the mesh data you are interested in. This
might look something like:
`options(fafbseg.cloudvolume.url='graphene://https://xxx.dynamicannotationframework.com/segmentation/xxx/xxx')`
and you can easily add this to your startup
[`Rprofile`](https://rdrr.io/r/base/Startup.html) with
[`usethis::edit_r_profile()`](https://usethis.r-lib.org/reference/edit.html).
For example, for the flywire data set, it is currently:
`'graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31'`.

Roughly in decreasing order of impact on speed:

Ratio: lower ratio = less vertices = faster

epsilon: lower target contraction rate = less steps = faster

SL: faster contraction = pot. less steps to target contraction rate =
faster

precision: lower precision = faster least-square computation = faster

sampling_dist: larger dist = faster collapse of mesh into skeleton =
faster

## See also

[`simple_python`](https://natverse.org/fafbseg/reference/simple_python.md)
for installation of the necessary Python packages.

## Examples

``` r
if (FALSE) { # \dontrun{
choose_segmentation("flywire")
nx=xform_brain(elmr::dense_core_neurons, ref="FlyWire", sample="FAFB14")
xyz = xyzmatrix(nx)
ids = unique(flywire_xyz2id(xyz[sample(1:nrow(xyz),100),]))
neurons = skeletor(ids, brain = elmr::FAFB14.surf)
plot3d(neurons) # note, in flywire space
plot3d(nx, col="black", lwd  =2) # note, in flywire space

# Also plot their meshes
neuron.meshes = read_cloudvolume_meshes(ids)


# We can also just save the .obj files
dir.create("obj")
download_neuron_obj(ids, save.obj = "obj")

# remove
unlink("obj", recurvise = TRUE)

} # }
```
