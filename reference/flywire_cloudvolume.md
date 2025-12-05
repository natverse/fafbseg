# Low level access to FlyWire data via Python cloudvolume module

Low level access to FlyWire data via Python cloudvolume module

## Usage

``` r
flywire_cloudvolume(
  cloudvolume.url = NULL,
  cached = TRUE,
  min_version = NULL,
  ...
)
```

## Arguments

- cloudvolume.url:

  URL for CloudVolume to fetch segmentation image data. The default
  value of NULL chooses the flywire production segmentation dataset.

- cached:

  When `TRUE` (the default) reuses a cached CloudVolume object from the
  current Python session. See details.

- min_version:

  A minimum version for the cloudvolume Python module e.g. `"3.12"`. The
  default `NULL` implies any version is acceptable.

- ...:

  Additional arguments passed to the CloudVolume constructor

## Details

this is the equivalent of doing (in Python):

`from cloudvolume import CloudVolume vol = CloudVolume('graphene://https://prodv1.flywire-daf.com/segmentation/table/fly_v31', use_https=True)`

The cache tries to be intelligent by

- 1\. generating a new object for every input parameter combination
  (which of course you would need to do in Python)

- 2\. avoiding stale references by checking that Python is currently
  running and that the returned CloudVolume object is non-null. It also
  regenerates the object every hour.

Note that reticulate the package which allows R/Python interaction binds
to one Python session. Furthermore Python cannot be restarted without
also restarting R.

## See also

[`simple_python`](https://natverse.org/fafbseg/reference/simple_python.md)
for installation of the necessary Python packages.

## Examples

``` r
if (FALSE) { # \dontrun{
cv=flywire_cloudvolume()

# detailed info about the image volume
cv$info
# bounding box (Python format in raw voxels)
cv$bounds
# in nm
boundingbox(cv)

# get help for a function
reticulate::py_help(cv$get_roots)
} # }
```
