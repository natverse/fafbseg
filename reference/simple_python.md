# Simple Python installation for use with R/fafbseg/FlyWire

Installs Python via an isolated miniconda environment as well as
recommended packages for fafbseg. If you absolutely do not want to use
miniconda (it is much simpler to get started) please read the Details
section.

## Usage

``` r
simple_python(
  pyinstall = c("basic", "full", "extra", "cleanenv", "blast", "none"),
  pkgs = NULL,
  miniconda = TRUE
)
```

## Arguments

- pyinstall:

  Whether to do a `"basic"` install (enough for most functionality) a
  `"full"` install, which includes tools for accessing fast/simple
  skeletons and "dotprops" for NBLAST via the `fafbseg-py` package;
  `"extra"` installs neuron packages that enable high resolution
  skeletonisation that are really not necessary for most users and
  frankly a pain to install. `"cleanenv"` will show you how to clean up
  your Python environment removing all packages. `"blast"` will show you
  how to completely remove your dedicated miniconda installation.
  Choosing what="none" skips update/install of Python and recommended
  packages only installing extras defined by `pkgs`.

- pkgs:

  Additional python packages to install.

- miniconda:

  Whether to use the reticulate package's default approach of a
  dedicated python for R based on miniconda (recommended, the default)
  or to allow the specification of a different system installed Python
  via the `RETICULATE_PYTHON` environment variable.

## Details

The recommended Python install procedure installs a miniconda Python
distribution. This will not be added to your system `PATH` by default
and can be used exclusively by R. If you do not want to use miniconda,
then you should at least a) make a Python virtual environment using
virtualenv (or conda if you are managing your own conda install) and b)
specify which Python you want to use with the `RETICULATE_PYTHON`
environment variable. You can set `RETICULATE_PYTHON` with
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html).

If you decided to stick with miniconda as recommended, some
customisation is still possible. As a halfway house, you can set
`options('fafbseg.condaenv')` to specify a non-standard miniconda
virtual environment as an alternative to the default `"r-reticulate"`.
Furthermore you can set an environment variable
`RETICULATE_MINICONDA_PYTHON_VERSION=3.10` to use a newer version of
Python than the reticulate package recommends.

If this sounds complicated, we strongly suggest sticking to the default
`miniconda=TRUE` approach.

Note that that after installing miniconda Python for the first time or
updating your miniconda install, you will likely be asked to restart R.
This is because you cannot restart the Python interpreter linked to an R
session. Therefore if Python was already running in this session, you
must restart R to use your new Python install.

## Examples

``` r
if (FALSE) { # \dontrun{
# just the basics
simple_python("basic")
# if you want to skeletonise meshes
simple_python("full")

# To install a special package using the recommended approach
simple_python(pkgs="PyChunkedGraph")
# the same but without touching Python itself or the recommended packages
simple_python('none', pkgs='PyChunkedGraph')

# install a specific version of cloud-volume package
simple_python('none', pkgs='cloud-volume~=3.8.0')
# if you really need to insist (e.g. because a newer version is already installed)
reticulate::py_install('cloud-volume==8.15.0', pip = TRUE)

# install the latest version of a package from github
simple_python('none', pkgs="git+git://github.com/schlegelp/skeletor@master")

# install a specific earlier version of a package
simple_python('none', pkgs="git+git://github.com/seung-lab/DracoPy@v0.0.15")

# install all recommended packages but use your existing Python
# only do this if you know what you are doing ...
simple_python("full", miniconda=FALSE)
} # }
```
