# Choose or (temporarily) use a FAFB autosegmentation

`with_segmentation` allows a specific segmentation to be temporarily
selected.

## Usage

``` r
choose_segmentation(
  release = c("flywire31", "public-flywire31", "20200412", "20190805", "20190521",
    "sandbox-flywire31"),
  set = TRUE,
  moreoptions = list()
)

with_segmentation(release, expr)
```

## Arguments

- release:

  character vector specifying a released segmentation via a known short
  name or a sample neuroglancer URL.

- set:

  Whether or not to set the selected options for the selected `release`.

- moreoptions:

  Any further options that you might wish to set (optional, expert use
  only, principally intended for use by `fancr` package).

- expr:

  The expression to execute with the temporary options set

## Value

If `set=TRUE` a list containing the previous values of the relevant
global options (in the style of
[`options`](https://rdrr.io/r/base/options.html). If `set=FALSE` a named
list containing the option values.

The result of evaluating `expr`

## Details

Each released segmentation implies a number of global options. This
package comes with multiple different default scene urls specified via
`choose_segmentation` or `with_segmentation`. This is the easiest way to
choose a particular segmentation. You can also pass a sample URL to the
`release` argument.

As of Nov 2020, the `"flywire31"` is the default segmentation when the
package loads. This specifies the production (i.e. in progress) version
of the flywire dataset. You will need to register with the flywire team
and generate a flywire token linked to the email used during your
registration to access this. Alternatively you can also use the
`"public-flywire31"` data release accompanying the Dorkenwald et al 2023
and Schlegel et al 2023 publications; you still need a token for this
(see
[`flywire_set_token`](https://natverse.org/fafbseg/reference/flywire_set_token.md)),
but no registration is required (and there are no limitations on use of
these data for new projects besides citing those two preprints).

You can also specify a different sample URL via the `sampleurl` argument
of some functions; it will be remembered for the rest of the R session.
If you regularly use a particular kind of scene URL, you can set
`options(fafbseg.sampleurl)` in your
[`Rprofile`](https://rdrr.io/r/base/Startup.html) file.

If you need to use both built-in and custom segmentation URLs, we
recommend specifying the custom URL in your
[`Rprofile`](https://rdrr.io/r/base/Startup.html) file and using the
`with_segmentation` to run code that uses one of the built-in
segmentations.

## Examples

``` r
# \donttest{
choose_segmentation('20190805', set=FALSE)
#> $fafbseg.sampleurl
#> [1] "https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://neuroglancer-fafb-data/fafb_v14/fafb_v14_clahe_sharded%22%2C%22type%22:%22image%22%2C%22name%22:%22fafb_v14_clahe%22%7D%2C%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805%22%2C%22type%22:%22segmentation%22%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190805%22%7D%2C%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805-skeletons32nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190805-skeletons32nm%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B122437.5625%2C36391.234375%2C2057.085205078125%5D%7D%7D%2C%22zoomFactor%22:8%7D%2C%22perspectiveOrientation%22:%5B-0.6771116256713867%2C0.6536111831665039%2C-0.1610027700662613%2C0.2973051071166992%5D%2C%22perspectiveZoom%22:5184.860830857428%2C%22showSlices%22:false%2C%22layout%22:%224panel%22%7D"
#> 
#> $fafbseg.skeletonuri
#> [1] "brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805-skeletons32nm"
#> 
#> $fafbseg.brainmaps.volume
#> [1] "772153499790:fafb_v14:fafb-ffn1-20190805"
#> 
#> $fafbseg.brainmaps.meshName
#> [1] "mcws_quad1e6"
#> 
#> $fafbseg.catmaid
#> [1] "https://garden.catmaid.org/tracing/fafb/v14-seg-li-190805.0/"
#> 
#> $fafbseg.cloudvolume.url
#> [1] "brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805"
#> 
# }
if (FALSE) { # \dontrun{
# temporarily change default segmentation to run a command
# but restore original default when finished
# Choose the FlyWire segmentation
with_segmentation('flywire31', {open_fafb_ngl(c(460792, 221812, 61480))})

# similarly for one Google (Li and Jain) segmentation
with_segmentation('20190805', {open_fafb_ngl(c(460792, 221812, 61480))})
} # }
if (FALSE) { # \dontrun{
n <- with_segmentation("20190521",
  read.neuron.brainmaps(22427007374))

# open location in flywire
with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200)))
} # }
```
