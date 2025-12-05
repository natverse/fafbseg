# Installing python modules: cloudvolume, meshparty and skeletor

## tl;dr

Experts disagree on the best method to install Python and its associated
packages. For those users whose principal use of Python will be from R,
we have found that following the
[reticulate](https://rstudio.github.io/reticulate/) package’s default of
setting up a dedicated miniconda installation is a simple and efficient
way to install a Python environment for R. This provides access to the
[cloud-volume](https://github.com/seung-lab/cloud-volume) package that
powers many of the functions required to talk to the flywire
environment. Extension packages for mesh skeletonisation also install
cleanly with this method.

``` r
# If you don't already have the R -> python bridge package
if(!requireNamespace('reticulate')) install.packages("reticulate")

# just the basics for python
simple_python("basic")
# if you want to skeletonise meshes
simple_python("full")
```

Now that you have Python installed check that everything went well

``` r
fafbseg::dr_fafbseg()
# dr_fafbseg may tell you to get a flywire authorisation token (one time step)
flywire_set_token()
```

Caveats

- To avoid complications, we recommend starting a clean R session before
  installation. You will be told to restart your R session if you update
  Python on disk but already have a running Python interpreter attached
  to your R session.
- We that you have not taken any steps to tweak which version of Python
  to use (e.g. via the `RETICULATE_PYTHON` environment variable) or
  which [python virtual environment R should
  use](https://rstudio.github.io/reticulate/articles/versions.html). If
  you want to use a different Python then you can do:

``` r
simple_python("full", miniconda = FALSE)
```

and we recommend you set `RETICULATE_PYTHON` to point explicitly to your
preferred Python. For more details see
[`simple_python()`](https://natverse.org/fafbseg/reference/simple_python.md)
help or consult the more extended instructions below.

## Background

We need cloudvolume and meshparty for python in order to get flywire
meshes. This help article is written for those with not much experience
using python. For more experienced users, see cloud-volume install and
basic command info [here](https://github.com/seung-lab/cloud-volume).

If you are new to python, especially to `pip`, and `conda`, this [blog
post](https://jakevdp.github.io/blog/2016/08/25/conda-myths-and-misconceptions/)
can be helpful in understanding what’s what.

Bottom line is that `conda` can install python and non-python software
to a specific environment, trying to keep that environment functional by
taking different dependencies of packages into consideration, while
`pip` installs only python packages to wherever you want, whatever you
tell it to install. In general there are more packages that you can
`pip` install (from PyPi), but you can also do this in a `conda`
environment without a problem in most cases.

Since there are a bunch of non-python dependencies for **cloud-volume**
and **meshparty** , `conda` looks better for what we want.

## Install conda

[Download](https://docs.conda.io/en/latest/miniconda.html) **miniconda**
and install it, this includes only `conda`, python, and a few very basic
packages, including `pip`:

Make sure you know where you install it (e.g. usually here:
`/opt/miniconda3`), and update it to the most recent version in the
Terminal:

`$ conda update conda`

Hit ‘y’ to proceed if prompted.

After this the default python is `/opt/miniconda3/bin/python`, you can
check with

`$ which python`

## Work with a new environment

Use **conda** to create and activate an environment where we can install
everything we need:

    $ conda create --name flywire_env python=3.7
    $ conda activate flywire_env
    $ conda install nomkl

(To deactivate an active environment, use
`$ conda deactivate flywire_env`)

At this point we are in our environment and we can use
`conda install <package_name>` simply for anything that’s available
[here](https://anaconda.org/anaconda/repo), and `pip` should also
install packages in this environment, unless your PATH variable in your
bash.profile file tells it otherwise. Check which `pip` is default:

`$ which pip`

This should give you something under your miniconda folder, like
`/opt/miniconda3/envs/flywire_env/bin/pip`.

However, you might have your system python’s `pip`. You can comment out
the PATH variable for python in your .bash.profile by running:

`$ touch ~/.bash_profile; open ~/.bash_profile`

and adding a `#` before lines
`PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:${PATH}"`
and `export PATH`. After this `which pip` will return the right thing,
same thing applies to `which python`.

Alternatively you can always specify in the terminal which `pip` you
want to use, but this is suboptimal
(e.g. `/opt/miniconda3/envs/flywire_env/bin/pip install cloud-volume`).

## Install cloudvolume

Now we can install **cloud-volume** with

`$ pip install cloud-volume`

You will need a compiler (XCode on Mac) to install everything. If you
don’t have XCode, you either need to update your OSX to the most recent
version to download it from AppStore, or you can get any version
compatible with your current system from here:
<https://developer.apple.com/support/xcode/>

We have not tried installing on Windows, try to get the right compiler
with help from Google. In the thread on [FlyWire
Slack](https://flywire-forum.slack.com/archives/CM86C8H7F/p1570470089014000)
you might find other useful things.

To use **cloud-volume**, you need to get a token for authorisation here:
<https://globalv1.flywire-daf.com/auth/api/v1/refresh_token>, and add
this to a `.json` file that you create in this location:
`~/.cloudvolume/secrets/cave-secret.json`, see help for this
[here](http://natverse.org/fafbseg/articles/articles/accessing-graphene-server.md).

## Install meshparty

Install **meshparty**:

`$ pip install meshparty`

This errored out for me with a failure to install the dependency:
**pykdtree** `clang: error: unsupported option '-fopenmp'`. This is an
issue with my C++ compiler version (probably), good thing we’re using
`conda` now so we can do:

`$ conda install -c conda-forge pykdtree`

After this `pip` installing **meshparty** worked fine with the previous
command.

Now we have both **meshparty** and **cloud-volume** in this environment,
but **meshparty** has a few more optional dependencies for
skeletonisation, that we need to install:

    $ conda install -c conda-forge pyembree
    $ pip install caveclient

In order to use `meshparty_skeletonisation` I also had to install
**PyChunkedGraph**, which has a dependency called **igneous**, both are
from the Seung lab.

Install **igneous** from GitHub, and then `pip` install
**PyChunkedGraph**:

    $ git clone git@github.com:seung-lab/igneous.git
    $ cd igneous
    $ pip install -r requirements.txt
    $ python setup.py develop
    $ pip install PyChunkedGraph

## Working with R and python

If we want to use these packages in `R` through reticulate with all the
**natverse** and **fafbseg** functions and objects then we need to have
the package **reticulate** in `R` (`install.packages("reticulate")`) and
tell it to use this python version in our `conda` environment.

Run in `R`
console:[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
and add the line
`RETICULATE_PYTHON="/opt/miniconda3/envs/flywire_env/bin/python"` with
your path to the python under the `conda` environment. Be sure to leave
a blank line at the end of the file.

Restart R session, load **reticulate** and **fafbseg** (if necessary
install/update with `remotes::install_github("jefferis/fafbseg")`. Check
your python version with
[`reticulate::py_config()`](https://rstudio.github.io/reticulate/reference/py_config.html),
it should show the same path what you gave to the `RETICULATE_PYTHON`
variable.

It is possible that your RStudio session will crash with
[`reticulate::py_config()`](https://rstudio.github.io/reticulate/reference/py_config.html).
This may be because multiple copies of the OpenMP runtime have been
linked into the program. That is dangerous, since it can degrade
performance or cause incorrect results. To fix, try:

    $ conda uninstall numpy scikit-learn
    $ conda install nomkl numpy scikit-learn
    $ conda remove mkl mkl-service

### The natverse and flywire meshes

Let’s set up R to look at the write flywire data. You need to have
access to the production node of the flywire environment (i.e. passed
the test).

~~Use
[`usethis::edit_r_profile`](https://usethis.r-lib.org/reference/edit.html),
and in your R profile add:~~ GJ: Don’t do this. Just use
`choose_segmentation("flywire")`

    # Production flywire
    options(fafbseg.cloudvolume.url='graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31')

After this you should be able to read meshes, skeletonise them, and plot
both with FAFB:

    # Load packages
    library(natverse)
    library(fafbseg)
    choose_segmentation("flywire")
    dir.create("flywire_skeletons")

    # Points in FAFBv14 (nm) to flywire coordinates (also nm)
    points = matrix(c(723086,200499,90960),ncol=3)
    flywire.fafb = xform_brain(points, sample='FAFB14', ref="FlyWire")

    # Get mesh
    id = flywire_xyz2id(flywire.fafb)
    neuron_mesh = read_cloudvolume_meshes(id)

    # Skeletonise mesh
    meshparty_skeletonize(id, savedir = "flywire_skeletons")
    neuron_skel <- read.neuron.swc(sprintf("flywire_skeletons/%s.swc",id))

    nopen3d()
    plot3d(FAFB, alpha = 0.1)
    plot3d(neuron_skel*1000, WithNodes = F, col = "red", lwd = 2)
    wire3d(neuron_mesh[[1]], col = "cyan", alpha = 0.2)

### Skeletor

You may also want to install
[skeletor](https://github.com/schlegelp/skeletor), a package from
Philipp Schlegel that helps you thin and skeletonise meshes yourself.
This can produce more accurate results than you get using
[TEASAR](https://doi.org/10.1109/PCCGA.2000.883951) through meshparty,
and does not depend on either. To install, do:

    $ source activate flywire_env
    $ conda install nomkl
    $ pip3 install git+git://github.com/schlegelp/skeletor@master
    $ pip3 install fastremap
    $ pip3 install ncollpyde

You could test that skeletor works for flywire neurons in python using:

    python
    from cloudvolume import CloudVolume
    import trimesh as tm
    import skeletor as sk
    id=720575940614134045
    id=720575940634367221
    vol = CloudVolume('graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31', use_https=True)
    m = vol.mesh.get(id, deduplicate_chunk_boundaries=False)[id]
    m = tm.Trimesh(m.vertices, m.faces)
    simp = sk.pre.simplify(m, ratio=.1)
    cntr = sk.pre.contract(simp, SL=10, WH0=2, iter_lim=4, epsilon=0.05, precision=1e-6, progress=False, operator='umbrella')
    swc = sk.skeletonize.by_vertex_clusters(cntr, sampling_dist=500, progress=False)
    swc = sk.post.clean_up(s = swc, mesh = simp, theta = 0.01)
    sk.post.radii(s = swc, mesh = simp, method='knn', n=5, aggregate='mean')
    print(swc)

    # Faster, wavefront skeletonisation
    m = sk.pre.fix_mesh(m, remove_disconnected=10)
    swc2 = sk.skeletonize.by_wavefront(m, waves=2s)
    print(swc2)

Once you have installed skeletor and specified your python environment
as described above, in R you should be able to do:

    # Skeletonise
    choose_segmentation("flywire")
    nx=xform_brain(elmr::dense_core_neurons, ref="FlyWire", sample="FAFB14")
    xyz =xyzmatrix(nx)
    ids = unique(flywire_xyz2id(xyz[sample(1:nrow(xyz),100),]))
    neurons = skeletor(ids)

    # Plot in 3D
    nopen3d()
    plot3d(neurons) # note, in flywire space, slightly different to FAFBv14 space
    plot3d(nx, col="black", lwd  =2) # note, in flywire space

### Issues

If you run into issues, you can try wiping and re-initiating your conda
environment. You can try without igneous and PyChunkedGraph. ~~You may
also need to specify installing cloudvolume at version 1.20.1.~~ GJ:
Don’t do this as there have been important bug fixes since cloudvolume v
1.20.1 and the format of some URLs has changed

For example:

    $ conda remove -n flywire_env --all
    $ conda create --name flywire_env python=3.7
    $ conda activate flywire_env
    $ conda install nomkl
    $ conda install -c conda-forge pykdtree
    $ pip install --upgrade --prefer-binary meshparty cloud-volume
    $ pip3 install git+git://github.com/schlegelp/skeletor@master
    $ pip3 install fastremap
    $ pip3 install ncollpyde

If you need to install a specific version of cloudvolume (occasionally
the latest version breaks things), then try e.g.:

    pip install --upgrade --prefer-binary cloud-volume~=3.8.0
