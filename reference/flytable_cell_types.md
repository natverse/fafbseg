# Fetch (memoised) flywire cell type information from flytable

Fetch (memoised) flywire cell type information from flytable

## Usage

``` r
flytable_cell_types(
  pattern = NULL,
  version = NULL,
  timestamp = NULL,
  target = c("type", "cell_type", "hemibrain_type", "cell_class", "super_class",
    "cell_sub_class", "ito_lee_hemilineage", "malecns_type", "all"),
  table = c("info", "optic", "both"),
  transfer_hemibrain_type = c("extra", "none", "all"),
  cache = TRUE,
  use_static = NA
)
```

## Arguments

- pattern:

  Optional character vector specifying a pattern that cell types must
  match in a SQL `LIKE` statement executed by
  [`flytable_query`](https://natverse.org/fafbseg/reference/flytable-queries.md).
  The suffix `_L` or `_R` can be used to restricted to neurons annotated
  to the L or R hemisphere. See examples.

- version:

  An optional CAVE materialisation version number. See
  [`flywire_cave_query`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
  for more details. Note also that the special signalling value of
  `TRUE` implies the latest locally available connectome dump.

- timestamp:

  An optional timestamp as a string or POSIXct, interpreted as UTC when
  no timezone is specified.

- target:

  A character vector specifying which flytable columns `pattern` should
  match. The special value of `type` means either `cell_type` *or*
  `hemibrain_type` should match. The special value of `all` means to
  match against any of `cell_type, hemibrain_type, cell_class`.

- table:

  Which cell type information tables to use (`info` for brain, `optic`
  for optic lobes or `both`).

- transfer_hemibrain_type:

  Whether to transfer the `hemibrain_type` column into the `cell_type`
  (default TRUE, see details)

- cache:

  Whether to cache the results for 5m (default `TRUE` since the flytable
  query is is a little expensive)

- use_static:

  Whether to use static cell type information (from Schlegel et al)

## Value

The original data.frame left joined to appropriate rows from flytable.

## Details

when `transfer_hemibrain_type=TRUE`, `hemibrain_type` values will be
transferred into the `cell_type` column if `cell_type` is empty.

It seems that SQL LIKE searches (e.g. containing the `%` symbol) do not
work for the `ito_lee_hemilineage` column. You can still search for
exact matches or use full regular expression queries (which operate by
downloading all rows and then filtering on your machine).

Static cell type information is provided by Schlegel et 2023. See
[flywire_annotations](https://github.com/flyconnectome/flywire_annotations)
github repository. It will be used by default when connection to the
pre-release Cambridge flytable is not available or when specified by
`options(fafbseg.use_static_celltypes=TRUE)`. Note that presently only
one materialisation version (630) is supported for static data.

## See also

[`add_celltype_info`](https://natverse.org/fafbseg/reference/add_celltype_info.md)

## Examples

``` r
# \donttest{
flytable_cell_types("MBON%")
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940631916281 79448854932107696  left intrinsic     central       MBON
#> 2  720575940623472716 80011942324010318 right intrinsic     central       MBON
#> 3  720575940638526278 80715286101144676 right intrinsic     central       MBON
#> 4  720575940629585602 81137979670302132 right intrinsic     central       MBON
#> 5  720575940637934308 81349085835581377 right intrinsic     central       MBON
#> 6  720575940632118343 78744342923662254  left intrinsic     central       MBON
#> 7  720575940609959637 78603948898007978  left intrinsic     central       MBON
#> 8  720575940638163428 78111986298788517  left intrinsic     central       MBON
#> 9  720575940630496374 81349223341739625 right intrinsic     central       MBON
#> 10 720575940642142861 78393323903110735  left intrinsic     central       MBON
#> 11 720575940611344078 80223254782485510 right intrinsic     central       MBON
#> 12 720575940634822751 81277549061520124 right intrinsic     central       MBON
#> 13 720575940617552340 79588561627810154 right intrinsic     central       MBON
#> 14 720575940624280328 81489685951698728 right intrinsic     central       MBON
#> 15 720575940617567206 78180912067538056  left intrinsic     central       MBON
#> 16 720575940643700640 80152611093049624 right intrinsic     central       MBON
#> 17 720575940611984760 81137979670297348 right intrinsic     central       MBON
#> 18 720575940614026193 80154466519131949 right intrinsic     central       MBON
#> 19 720575940633209647 78463486488950059  left intrinsic     central       MBON
#> 20 720575940623001480 80154535238527400 right intrinsic     central       MBON
#> 21 720575940613070485 81066305390179956 right intrinsic     central       MBON
#> 22 720575940614595218 78040449457502610  left intrinsic     central       MBON
#> 23 720575940636992368 80785654845309410 right intrinsic     central       MBON
#> 24 720575940621828443 79166830266640003  left intrinsic     central       MBON
#> 25 720575940637902938 78181599128215629  left intrinsic     central       MBON
#> 26 720575940638774606 78040449457461921  left intrinsic     central       MBON
#> 27 720575940639697827 80224766543977933 right intrinsic     central       MBON
#> 28 720575940626833021 78251349531485275  left intrinsic     central       MBON
#> 29 720575940618008859 77901086224032815  left intrinsic     central       MBON
#> 30 720575940635841143 78110818201436707  left intrinsic     central       MBON
#> 31 720575940618249797 80082723452177136 right intrinsic     central       MBON
#> 32 720575940629856515 77408917331762608  left intrinsic     central       MBON
#> 33 720575940623905719 80224972702135904 right intrinsic     central       MBON
#> 34 720575940629529722 78533992672102680  left intrinsic     central       MBON
#> 35 720575940624659943 80011255129536552  left intrinsic     central       MBON
#> 36 720575940624590316 79167173863853528 right intrinsic     central       MBON
#> 37 720575940645304430 79380479052758063  left intrinsic     central       MBON
#> 38 720575940612196850 81136880426746412 right intrinsic     central       MBON
#> 39 720575940630767959 78251486970442209  left intrinsic     central       MBON
#> 40 720575940640963747 76985537105536911  left intrinsic     central       MBON
#> 41 720575940628757547 78040449457463315  left intrinsic     central       MBON
#> 42 720575940627144069 79166692827138268 right intrinsic     central       MBON
#> 43 720575940610647416 81137979602919932 right intrinsic     central       MBON
#> 44 720575940622093436 81136880426744139 right intrinsic     central       MBON
#> 45 720575940624696810 78110818201651448  left intrinsic     central       MBON
#> 46 720575940629422086 77760348668670642  left intrinsic     central       MBON
#> 47 720575940615435217 81137979670303492 right intrinsic     central       MBON
#> 48 720575940650386553 78110818201681795  left intrinsic     central       MBON
#> 49 720575940639556467 81067610926141558 right intrinsic     central       MBON
#> 50 720575940636120986 81700242831237167 right intrinsic     central       MBON
#> 51 720575940617749538 80786135881765925 right intrinsic     central       MBON
#> 52 720575940617760257 81277549061455173 right intrinsic     central       MBON
#> 53 720575940619810389 79518055445188833 right intrinsic     central       MBON
#> 54 720575940634482783 81136605414678093 right intrinsic     central       MBON
#> 55 720575940613906454 78182286188838645  left intrinsic     central       MBON
#> 56 720575940626109315 81066305390093306 right intrinsic     central       MBON
#> 57 720575940623377802 78110818201621392  left intrinsic     central       MBON
#> 58 720575940626744921 81277549061481189 right intrinsic     central       MBON
#> 59 720575940623464316 81136880426764424 right intrinsic     central       MBON
#> 60 720575940622979277 79027192356412856  left intrinsic     central       MBON
#> 61 720575940614892182 81277617781167541 right intrinsic     central       MBON
#> 62 720575940615756690 78111917579300796  left intrinsic     central       MBON
#> 63 720575940613090673 78324741798855874  left intrinsic     central       MBON
#> 64 720575940621777391 81137979736901319 right intrinsic     central       MBON
#> 65 720575940606953858 81136880426735755 right intrinsic     central       MBON
#> 66 720575940630075703 79309972869657633  left intrinsic     central       MBON
#> 67 720575940652390134 78603949099622269  left intrinsic     central       MBON
#> 68 720575940632535756 79380204241413766  left intrinsic     central       MBON
#> 69 720575940643696288 79309904216692475  left intrinsic     central       MBON
#> 70 720575940620162718 78323023677138555  left intrinsic     central       MBON
#> 71 720575940635063135 80082242348953693 right intrinsic     central       MBON
#> 72 720575940616463477 77830305364995511  left intrinsic     central       MBON
#> 73 720575940629981440 78885355625276424  left intrinsic     central       MBON
#> 74 720575940623182847 78463211409646517  left intrinsic     central       MBON
#> 75 720575940626315010 79943222847625326 right intrinsic     central       MBON
#> 76 720575940632943277 80925980016963911 right intrinsic     central       MBON
#> 77 720575940624539284 78040449457553649  left intrinsic     central       MBON
#> 78 720575940626180878 80011530007496199  left intrinsic     central       MBON
#> 79 720575940628783363 80434292093847745 right intrinsic     central       MBON
#> 80 720575940630390939 79870998677653042  left intrinsic     central       MBON
#> 81 720575940614392999 79237542540872430  left intrinsic     central       MBON
#> 82 720575940617302365 78604086538417636  left intrinsic     central       MBON
#> 83 720575940635651680 81277549061591572 right intrinsic     central       MBON
#> 84 720575940624694503 79941230049538531  left intrinsic     central       MBON
#> 85 720575940628734376 79869899232522382 right intrinsic     central       MBON
#> 86 720575940610964946 81630904409213619 right intrinsic     central       MBON
#> 87 720575940607155890 80433124332533007 right intrinsic     central       MBON
#> 88 720575940623381956 80644779918075196 right intrinsic     central       MBON
#> 89 720575940628487813 81560466945525625 right intrinsic     central       MBON
#> 90 720575940638028607 81419592152467685 right intrinsic     central       MBON
#> 91 720575940621164720 80222636373797362  left intrinsic     central       MBON
#> 92 720575940623841463 78251349531447591  left intrinsic     central       MBON
#> 93 720575940631177803 78814711734490195  left intrinsic     central       MBON
#> 94 720575940622306625 79166761480079751 right intrinsic     central       MBON
#> 95 720575940616398303 79729161744437991  left intrinsic     central       MBON
#> 96 720575940613815210 79729161744426207  left intrinsic     central       MBON
#>    cell_sub_class     cell_type        top_nt ito_lee_hemilineage
#> 1            <NA>        MBON02     glutamate    putative_primary
#> 2            <NA>        MBON01     glutamate    putative_primary
#> 3            <NA>        MBON32          gaba    putative_primary
#> 4            <NA>        MBON10          gaba                EBa1
#> 5            <NA>        MBON30     glutamate    putative_primary
#> 6            <NA>        MBON10          gaba                EBa1
#> 7            <NA>        MBON32          gaba    putative_primary
#> 8            <NA>        MBON06     glutamate    putative_primary
#> 9            <NA>        MBON05     glutamate    putative_primary
#> 10           <NA>        MBON10     glutamate                EBa1
#> 11           <NA>        MBON20          gaba    putative_primary
#> 12           <NA>   MBON17-like acetylcholine          DL1_dorsal
#> 13           <NA>        MBON02          gaba    putative_primary
#> 14           <NA>        MBON33 acetylcholine    putative_primary
#> 15           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 16           <NA>        MBON29 acetylcholine         DL2_ventral
#> 17           <NA>        MBON10          gaba                EBa1
#> 18           <NA>        MBON14 acetylcholine               FLAa2
#> 19           <NA>        MBON10     glutamate                EBa1
#> 20           <NA>        MBON12 acetylcholine               FLAa2
#> 21           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 22           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 23           <NA>        MBON31          gaba    putative_primary
#> 24           <NA>        MBON12 acetylcholine               FLAa2
#> 25           <NA>        MBON35 acetylcholine    putative_primary
#> 26           <NA>        MBON17 acetylcholine          DL1_dorsal
#> 27           <NA>        MBON14 acetylcholine               FLAa2
#> 28           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 29           <NA>        MBON30     glutamate    putative_primary
#> 30           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 31           <NA>        MBON27 acetylcholine    putative_primary
#> 32           <NA>        MBON09          gaba    putative_primary
#> 33           <NA>        MBON12 acetylcholine               FLAa2
#> 34           <NA>        MBON10     glutamate                EBa1
#> 35           <NA> MBON25,MBON34     glutamate              SMPad3
#> 36           <NA>        MBON03     glutamate    putative_primary
#> 37           <NA>        MBON13 acetylcholine               FLAa2
#> 38           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 39           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 40           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 41           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 42           <NA> MBON25,MBON34     glutamate              SMPad3
#> 43           <NA>        MBON10          gaba                EBa1
#> 44           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 45           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 46           <NA>        MBON09          gaba    putative_primary
#> 47           <NA>        MBON10          gaba                EBa1
#> 48           <NA>   MBON17-like acetylcholine          DL1_dorsal
#> 49           <NA>        MBON10          gaba                EBa1
#> 50           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 51           <NA>        MBON11          gaba    putative_primary
#> 52           <NA>        MBON17 acetylcholine          DL1_dorsal
#> 53           <NA> MBON25,MBON34     glutamate              SMPad3
#> 54           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 55           <NA>        MBON31          gaba    putative_primary
#> 56           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 57           <NA>        MBON16 acetylcholine          DL1_dorsal
#> 58           <NA>        MBON16 acetylcholine          DL1_dorsal
#> 59           <NA>        MBON24 acetylcholine    putative_primary
#> 60           <NA>        MBON27 acetylcholine    putative_primary
#> 61           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 62           <NA>        MBON21 acetylcholine    putative_primary
#> 63           <NA>        MBON20          gaba    putative_primary
#> 64           <NA>        MBON21 acetylcholine    putative_primary
#> 65           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 66           <NA>        MBON12 acetylcholine               FLAa2
#> 67           <NA>        MBON07     glutamate    putative_primary
#> 68           <NA>        MBON14 acetylcholine               FLAa2
#> 69           <NA>        MBON14 acetylcholine               FLAa2
#> 70           <NA>        MBON11          gaba    putative_primary
#> 71           <NA>        MBON22 acetylcholine    putative_primary
#> 72           <NA>        MBON22 acetylcholine    putative_primary
#> 73           <NA>        MBON26 acetylcholine    putative_primary
#> 74           <NA>        MBON33 acetylcholine    putative_primary
#> 75           <NA>        MBON13 acetylcholine               FLAa2
#> 76           <NA>        MBON35 acetylcholine    putative_primary
#> 77           <NA>        MBON18 acetylcholine          DL1_dorsal
#> 78           <NA>        MBON04     glutamate    putative_primary
#> 79           <NA>        MBON07     glutamate    putative_primary
#> 80           <NA>        MBON01     glutamate    putative_primary
#> 81           <NA>        MBON29 acetylcholine         DL2_ventral
#> 82           <NA>        MBON07     glutamate    putative_primary
#> 83           <NA>        MBON18 acetylcholine          DL1_dorsal
#> 84           <NA>        MBON03     glutamate    putative_primary
#> 85           <NA>        MBON04     glutamate    putative_primary
#> 86           <NA>        MBON09          gaba    putative_primary
#> 87           <NA>        MBON26 acetylcholine    putative_primary
#> 88           <NA>        MBON07     glutamate    putative_primary
#> 89           <NA>        MBON09          gaba    putative_primary
#> 90           <NA>        MBON06     glutamate    putative_primary
#> 91           <NA>        MBON05 acetylcholine    putative_primary
#> 92           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 93           <NA>        MBON24 acetylcholine    putative_primary
#> 94           <NA> MBON25,MBON34     glutamate              SMPad3
#> 95           <NA> MBON25,MBON34     glutamate              SMPad3
#> 96           <NA> MBON25,MBON34     glutamate              SMPad3
#>    hemibrain_type malecns_type       fbbt_id
#> 1          MBON02         <NA> FBbt_00111012
#> 2          MBON01         <NA> FBbt_00100234
#> 3          MBON32         <NA> FBbt_00049852
#> 4          MBON10         <NA> FBbt_00111008
#> 5          MBON30         <NA> FBbt_00049850
#> 6          MBON10         <NA> FBbt_00111008
#> 7          MBON32         <NA> FBbt_00049852
#> 8          MBON06         <NA> FBbt_00100242
#> 9          MBON05         <NA> FBbt_00111004
#> 10         MBON10         <NA> FBbt_00111008
#> 11         MBON20         <NA> FBbt_00111013
#> 12    MBON17-like         <NA>          <NA>
#> 13         MBON02         <NA> FBbt_00111012
#> 14         MBON33         <NA> FBbt_00049853
#> 15         MBON23         <NA> FBbt_00049113
#> 16         MBON29         <NA> FBbt_00049849
#> 17         MBON10         <NA> FBbt_00111008
#> 18         MBON14         <NA> FBbt_00100238
#> 19         MBON10         <NA> FBbt_00111008
#> 20         MBON12         <NA> FBbt_00111009
#> 21    MBON15-like         <NA>          <NA>
#> 22         MBON28         <NA> FBbt_00049848
#> 23         MBON31         <NA> FBbt_00049851
#> 24         MBON12         <NA> FBbt_00111009
#> 25         MBON35         <NA> FBbt_00049855
#> 26         MBON17         <NA> FBbt_00111064
#> 27         MBON14         <NA> FBbt_00100238
#> 28         MBON19         <NA> FBbt_00111011
#> 29         MBON30         <NA> FBbt_00049850
#> 30         MBON15         <NA> FBbt_00111010
#> 31         MBON27         <NA> FBbt_00049847
#> 32         MBON09         <NA> FBbt_00111007
#> 33         MBON12         <NA> FBbt_00111009
#> 34         MBON10         <NA> FBbt_00111008
#> 35  MBON25,MBON34         <NA>          <NA>
#> 36         MBON03         <NA> FBbt_00100232
#> 37         MBON13         <NA> FBbt_00100239
#> 38    MBON15-like         <NA>          <NA>
#> 39         MBON15         <NA> FBbt_00111010
#> 40    MBON15-like         <NA>          <NA>
#> 41    MBON15-like         <NA>          <NA>
#> 42  MBON25,MBON34         <NA>          <NA>
#> 43         MBON10         <NA> FBbt_00111008
#> 44         MBON15         <NA> FBbt_00111010
#> 45    MBON15-like         <NA>          <NA>
#> 46         MBON09         <NA> FBbt_00111007
#> 47         MBON10         <NA> FBbt_00111008
#> 48    MBON17-like         <NA>          <NA>
#> 49         MBON10         <NA> FBbt_00111008
#> 50         MBON15         <NA> FBbt_00111010
#> 51         MBON11         <NA> FBbt_00100246
#> 52         MBON17         <NA> FBbt_00111064
#> 53  MBON25,MBON34         <NA>          <NA>
#> 54         MBON19         <NA> FBbt_00111011
#> 55         MBON31         <NA> FBbt_00049851
#> 56         MBON19         <NA> FBbt_00111011
#> 57         MBON16         <NA> FBbt_00111063
#> 58         MBON16         <NA> FBbt_00111063
#> 59         MBON24         <NA> FBbt_00049844
#> 60         MBON27         <NA> FBbt_00049847
#> 61         MBON28         <NA> FBbt_00049848
#> 62         MBON21         <NA> FBbt_00111046
#> 63         MBON20         <NA> FBbt_00111013
#> 64         MBON21         <NA> FBbt_00111046
#> 65         MBON23         <NA> FBbt_00049113
#> 66         MBON12         <NA> FBbt_00111009
#> 67         MBON07         <NA> FBbt_00111005
#> 68         MBON14         <NA> FBbt_00100238
#> 69         MBON14         <NA> FBbt_00100238
#> 70         MBON11         <NA> FBbt_00100246
#> 71         MBON22         <NA> FBbt_00100240
#> 72         MBON22         <NA> FBbt_00100240
#> 73         MBON26         <NA> FBbt_00049846
#> 74         MBON33         <NA> FBbt_00049853
#> 75         MBON13         <NA> FBbt_00100239
#> 76         MBON35         <NA> FBbt_00049855
#> 77         MBON18         <NA> FBbt_00110101
#> 78         MBON04         <NA> FBbt_00111014
#> 79         MBON07         <NA> FBbt_00111005
#> 80         MBON01         <NA> FBbt_00100234
#> 81         MBON29         <NA> FBbt_00049849
#> 82         MBON07         <NA> FBbt_00111005
#> 83         MBON18         <NA> FBbt_00110101
#> 84         MBON03         <NA> FBbt_00100232
#> 85         MBON04         <NA> FBbt_00111014
#> 86         MBON09         <NA> FBbt_00111007
#> 87         MBON26         <NA> FBbt_00049846
#> 88         MBON07         <NA> FBbt_00111005
#> 89         MBON09         <NA> FBbt_00111007
#> 90         MBON06         <NA> FBbt_00100242
#> 91         MBON05         <NA> FBbt_00111004
#> 92         MBON19         <NA> FBbt_00111011
#> 93         MBON24         <NA> FBbt_00049844
#> 94  MBON25,MBON34         <NA>          <NA>
#> 95  MBON25,MBON34         <NA>          <NA>
#> 96  MBON25,MBON34         <NA>          <NA>
flytable_cell_types("MBON%", version=450)
#> Updating 92 ids
#> flywire_rootid_cached: Looking up 92 missing keys
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940620475757 79448854932107696  left intrinsic     central       MBON
#> 2  720575940635988581 80011942324010318 right intrinsic     central       MBON
#> 3  720575940616047221 80715286101144676 right intrinsic     central       MBON
#> 4  720575940638780494 81137979670302132 right intrinsic     central       MBON
#> 5  720575940610014510 81349085835581377 right intrinsic     central       MBON
#> 6  720575940608009737 78744342923662254  left intrinsic     central       MBON
#> 7  720575940620520769 78603948898007978  left intrinsic     central       MBON
#> 8  720575940621245680 78111986298788517  left intrinsic     central       MBON
#> 9  720575940631910308 81349223341739625 right intrinsic     central       MBON
#> 10 720575940612772579 78393323903110735  left intrinsic     central       MBON
#> 11 720575940616240011 80223254782485510 right intrinsic     central       MBON
#> 12 720575940624260839 81277549061520124 right intrinsic     central       MBON
#> 13 720575940625496010 79588561627810154 right intrinsic     central       MBON
#> 14 720575940621704361 81489685951698728 right intrinsic     central       MBON
#> 15 720575940616298635 78180912067538056  left intrinsic     central       MBON
#> 16 720575940621187056 80152611093049624 right intrinsic     central       MBON
#> 17 720575940614206866 81137979670297348 right intrinsic     central       MBON
#> 18 720575940621208820 80154466519131949 right intrinsic     central       MBON
#> 19 720575940615431692 78463486488950059  left intrinsic     central       MBON
#> 20 720575940639603584 80154535238527400 right intrinsic     central       MBON
#> 21 720575940634657899 81066305390179956 right intrinsic     central       MBON
#> 22 720575940630796363 78040449457502610  left intrinsic     central       MBON
#> 23 720575940621196784 80785654845309410 right intrinsic     central       MBON
#> 24 720575940617807429 79166830266640003  left intrinsic     central       MBON
#> 25 720575940623046035 78181599128215629  left intrinsic     central       MBON
#> 26 720575940638774606 78040449457461921  left intrinsic     central       MBON
#> 27 720575940643518408 80224766543977933 right intrinsic     central       MBON
#> 28 720575940625168528 78251349531485275  left intrinsic     central       MBON
#> 29 720575940615443468 77901086224032815  left intrinsic     central       MBON
#> 30 720575940635841143 78110818201436707  left intrinsic     central       MBON
#> 31 720575940613049325 80082723452177136 right intrinsic     central       MBON
#> 32 720575940627152712 77408917331762608  left intrinsic     central       MBON
#> 33 720575940623615561 80224972702135904 right intrinsic     central       MBON
#> 34 720575940624260583 78533992672102680  left intrinsic     central       MBON
#> 35 720575940623189431 80011255129536552  left intrinsic     central       MBON
#> 36 720575940628356611 79167173863853528 right intrinsic     central       MBON
#> 37 720575940629103486 79380479052758063  left intrinsic     central       MBON
#> 38 720575940626510729 81136880426746412 right intrinsic     central       MBON
#> 39 720575940631573165 78251486970442209  left intrinsic     central       MBON
#> 40 720575940627235215 76985537105536911  left intrinsic     central       MBON
#> 41 720575940625623165 78040449457463315  left intrinsic     central       MBON
#> 42 720575940641532557 79166692827138268 right intrinsic     central       MBON
#> 43 720575940604965809 81137979602919932 right intrinsic     central       MBON
#> 44 720575940604592300 81136880426744139 right intrinsic     central       MBON
#> 45 720575940613666001 78110818201651448  left intrinsic     central       MBON
#> 46 720575940620015275 77760348668670642  left intrinsic     central       MBON
#> 47 720575940619587975 81137979670303492 right intrinsic     central       MBON
#> 48 720575940636538480 78110818201681795  left intrinsic     central       MBON
#> 49 720575940629528064 81067610926141558 right intrinsic     central       MBON
#> 50 720575940630182467 81700242831237167 right intrinsic     central       MBON
#> 51 720575940611534193 80786135881765925 right intrinsic     central       MBON
#> 52 720575940624149908 81277549061455173 right intrinsic     central       MBON
#> 53 720575940635705719 79518055445188833 right intrinsic     central       MBON
#> 54 720575940634482783 81136605414678093 right intrinsic     central       MBON
#> 55 720575940628680039 78182286188838645  left intrinsic     central       MBON
#> 56 720575940626109315 81066305390093306 right intrinsic     central       MBON
#> 57 720575940644139812 78110818201621392  left intrinsic     central       MBON
#> 58 720575940654302881 81277549061481189 right intrinsic     central       MBON
#> 59 720575940626786949 81136880426764424 right intrinsic     central       MBON
#> 60 720575940611536241 79027192356412856  left intrinsic     central       MBON
#> 61 720575940623018730 81277617781167541 right intrinsic     central       MBON
#> 62 720575940635488623 78111917579300796  left intrinsic     central       MBON
#> 63 720575940616286603 78324741798855874  left intrinsic     central       MBON
#> 64 720575940621503265 81137979736901319 right intrinsic     central       MBON
#> 65 720575940631189304 81136880426735755 right intrinsic     central       MBON
#> 66 720575940617330945 79309972869657633  left intrinsic     central       MBON
#> 67 720575940628523419 78603949099622269  left intrinsic     central       MBON
#> 68 720575940610543826 79380204241413766  left intrinsic     central       MBON
#> 69 720575940616968633 79309904216692475  left intrinsic     central       MBON
#> 70 720575940623762923 78323023677138555  left intrinsic     central       MBON
#> 71 720575940626901637 80082242348953693 right intrinsic     central       MBON
#> 72 720575940623211498 77830305364995511  left intrinsic     central       MBON
#> 73 720575940625816420 78885355625276424  left intrinsic     central       MBON
#> 74 720575940619919531 78463211409646517  left intrinsic     central       MBON
#> 75 720575940614532970 79943222847625326 right intrinsic     central       MBON
#> 76 720575940643356064 80925980016963911 right intrinsic     central       MBON
#> 77 720575940649242233 78040449457553649  left intrinsic     central       MBON
#> 78 720575940619620487 80011530007496199  left intrinsic     central       MBON
#> 79 720575940624835661 80434292093847745 right intrinsic     central       MBON
#> 80 720575940625538110 79870998677653042  left intrinsic     central       MBON
#> 81 720575940626706825 79237542540872430  left intrinsic     central       MBON
#> 82 720575940605195878 78604086538417636  left intrinsic     central       MBON
#> 83 720575940631910052 81277549061591572 right intrinsic     central       MBON
#> 84 720575940608426197 79941230049538531  left intrinsic     central       MBON
#> 85 720575940625222288 79869899232522382 right intrinsic     central       MBON
#> 86 720575940620070278 81630904409213619 right intrinsic     central       MBON
#> 87 720575940633688220 80433124332533007 right intrinsic     central       MBON
#> 88 720575940613357096 80644779918075196 right intrinsic     central       MBON
#> 89 720575940608355029 81560466945525625 right intrinsic     central       MBON
#> 90 720575940624105196 81419592152467685 right intrinsic     central       MBON
#> 91 720575940613666769 80222636373797362  left intrinsic     central       MBON
#> 92 720575940605182822 78251349531447591  left intrinsic     central       MBON
#> 93 720575940635161268 78814711734490195  left intrinsic     central       MBON
#> 94 720575940631601292 79166761480079751 right intrinsic     central       MBON
#> 95 720575940609440324 79729161744437991  left intrinsic     central       MBON
#> 96 720575940604158124 79729161744426207  left intrinsic     central       MBON
#>    cell_sub_class     cell_type        top_nt ito_lee_hemilineage
#> 1            <NA>        MBON02     glutamate    putative_primary
#> 2            <NA>        MBON01     glutamate    putative_primary
#> 3            <NA>        MBON32          gaba    putative_primary
#> 4            <NA>        MBON10          gaba                EBa1
#> 5            <NA>        MBON30     glutamate    putative_primary
#> 6            <NA>        MBON10          gaba                EBa1
#> 7            <NA>        MBON32          gaba    putative_primary
#> 8            <NA>        MBON06     glutamate    putative_primary
#> 9            <NA>        MBON05     glutamate    putative_primary
#> 10           <NA>        MBON10     glutamate                EBa1
#> 11           <NA>        MBON20          gaba    putative_primary
#> 12           <NA>   MBON17-like acetylcholine          DL1_dorsal
#> 13           <NA>        MBON02          gaba    putative_primary
#> 14           <NA>        MBON33 acetylcholine    putative_primary
#> 15           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 16           <NA>        MBON29 acetylcholine         DL2_ventral
#> 17           <NA>        MBON10          gaba                EBa1
#> 18           <NA>        MBON14 acetylcholine               FLAa2
#> 19           <NA>        MBON10     glutamate                EBa1
#> 20           <NA>        MBON12 acetylcholine               FLAa2
#> 21           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 22           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 23           <NA>        MBON31          gaba    putative_primary
#> 24           <NA>        MBON12 acetylcholine               FLAa2
#> 25           <NA>        MBON35 acetylcholine    putative_primary
#> 26           <NA>        MBON17 acetylcholine          DL1_dorsal
#> 27           <NA>        MBON14 acetylcholine               FLAa2
#> 28           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 29           <NA>        MBON30     glutamate    putative_primary
#> 30           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 31           <NA>        MBON27 acetylcholine    putative_primary
#> 32           <NA>        MBON09          gaba    putative_primary
#> 33           <NA>        MBON12 acetylcholine               FLAa2
#> 34           <NA>        MBON10     glutamate                EBa1
#> 35           <NA> MBON25,MBON34     glutamate              SMPad3
#> 36           <NA>        MBON03     glutamate    putative_primary
#> 37           <NA>        MBON13 acetylcholine               FLAa2
#> 38           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 39           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 40           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 41           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 42           <NA> MBON25,MBON34     glutamate              SMPad3
#> 43           <NA>        MBON10          gaba                EBa1
#> 44           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 45           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 46           <NA>        MBON09          gaba    putative_primary
#> 47           <NA>        MBON10          gaba                EBa1
#> 48           <NA>   MBON17-like acetylcholine          DL1_dorsal
#> 49           <NA>        MBON10          gaba                EBa1
#> 50           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 51           <NA>        MBON11          gaba    putative_primary
#> 52           <NA>        MBON17 acetylcholine          DL1_dorsal
#> 53           <NA> MBON25,MBON34     glutamate              SMPad3
#> 54           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 55           <NA>        MBON31          gaba    putative_primary
#> 56           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 57           <NA>        MBON16 acetylcholine          DL1_dorsal
#> 58           <NA>        MBON16 acetylcholine          DL1_dorsal
#> 59           <NA>        MBON24 acetylcholine    putative_primary
#> 60           <NA>        MBON27 acetylcholine    putative_primary
#> 61           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 62           <NA>        MBON21 acetylcholine    putative_primary
#> 63           <NA>        MBON20          gaba    putative_primary
#> 64           <NA>        MBON21 acetylcholine    putative_primary
#> 65           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 66           <NA>        MBON12 acetylcholine               FLAa2
#> 67           <NA>        MBON07     glutamate    putative_primary
#> 68           <NA>        MBON14 acetylcholine               FLAa2
#> 69           <NA>        MBON14 acetylcholine               FLAa2
#> 70           <NA>        MBON11          gaba    putative_primary
#> 71           <NA>        MBON22 acetylcholine    putative_primary
#> 72           <NA>        MBON22 acetylcholine    putative_primary
#> 73           <NA>        MBON26 acetylcholine    putative_primary
#> 74           <NA>        MBON33 acetylcholine    putative_primary
#> 75           <NA>        MBON13 acetylcholine               FLAa2
#> 76           <NA>        MBON35 acetylcholine    putative_primary
#> 77           <NA>        MBON18 acetylcholine          DL1_dorsal
#> 78           <NA>        MBON04     glutamate    putative_primary
#> 79           <NA>        MBON07     glutamate    putative_primary
#> 80           <NA>        MBON01     glutamate    putative_primary
#> 81           <NA>        MBON29 acetylcholine         DL2_ventral
#> 82           <NA>        MBON07     glutamate    putative_primary
#> 83           <NA>        MBON18 acetylcholine          DL1_dorsal
#> 84           <NA>        MBON03     glutamate    putative_primary
#> 85           <NA>        MBON04     glutamate    putative_primary
#> 86           <NA>        MBON09          gaba    putative_primary
#> 87           <NA>        MBON26 acetylcholine    putative_primary
#> 88           <NA>        MBON07     glutamate    putative_primary
#> 89           <NA>        MBON09          gaba    putative_primary
#> 90           <NA>        MBON06     glutamate    putative_primary
#> 91           <NA>        MBON05 acetylcholine    putative_primary
#> 92           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 93           <NA>        MBON24 acetylcholine    putative_primary
#> 94           <NA> MBON25,MBON34     glutamate              SMPad3
#> 95           <NA> MBON25,MBON34     glutamate              SMPad3
#> 96           <NA> MBON25,MBON34     glutamate              SMPad3
#>    hemibrain_type malecns_type       fbbt_id
#> 1          MBON02         <NA> FBbt_00111012
#> 2          MBON01         <NA> FBbt_00100234
#> 3          MBON32         <NA> FBbt_00049852
#> 4          MBON10         <NA> FBbt_00111008
#> 5          MBON30         <NA> FBbt_00049850
#> 6          MBON10         <NA> FBbt_00111008
#> 7          MBON32         <NA> FBbt_00049852
#> 8          MBON06         <NA> FBbt_00100242
#> 9          MBON05         <NA> FBbt_00111004
#> 10         MBON10         <NA> FBbt_00111008
#> 11         MBON20         <NA> FBbt_00111013
#> 12    MBON17-like         <NA>          <NA>
#> 13         MBON02         <NA> FBbt_00111012
#> 14         MBON33         <NA> FBbt_00049853
#> 15         MBON23         <NA> FBbt_00049113
#> 16         MBON29         <NA> FBbt_00049849
#> 17         MBON10         <NA> FBbt_00111008
#> 18         MBON14         <NA> FBbt_00100238
#> 19         MBON10         <NA> FBbt_00111008
#> 20         MBON12         <NA> FBbt_00111009
#> 21    MBON15-like         <NA>          <NA>
#> 22         MBON28         <NA> FBbt_00049848
#> 23         MBON31         <NA> FBbt_00049851
#> 24         MBON12         <NA> FBbt_00111009
#> 25         MBON35         <NA> FBbt_00049855
#> 26         MBON17         <NA> FBbt_00111064
#> 27         MBON14         <NA> FBbt_00100238
#> 28         MBON19         <NA> FBbt_00111011
#> 29         MBON30         <NA> FBbt_00049850
#> 30         MBON15         <NA> FBbt_00111010
#> 31         MBON27         <NA> FBbt_00049847
#> 32         MBON09         <NA> FBbt_00111007
#> 33         MBON12         <NA> FBbt_00111009
#> 34         MBON10         <NA> FBbt_00111008
#> 35  MBON25,MBON34         <NA>          <NA>
#> 36         MBON03         <NA> FBbt_00100232
#> 37         MBON13         <NA> FBbt_00100239
#> 38    MBON15-like         <NA>          <NA>
#> 39         MBON15         <NA> FBbt_00111010
#> 40    MBON15-like         <NA>          <NA>
#> 41    MBON15-like         <NA>          <NA>
#> 42  MBON25,MBON34         <NA>          <NA>
#> 43         MBON10         <NA> FBbt_00111008
#> 44         MBON15         <NA> FBbt_00111010
#> 45    MBON15-like         <NA>          <NA>
#> 46         MBON09         <NA> FBbt_00111007
#> 47         MBON10         <NA> FBbt_00111008
#> 48    MBON17-like         <NA>          <NA>
#> 49         MBON10         <NA> FBbt_00111008
#> 50         MBON15         <NA> FBbt_00111010
#> 51         MBON11         <NA> FBbt_00100246
#> 52         MBON17         <NA> FBbt_00111064
#> 53  MBON25,MBON34         <NA>          <NA>
#> 54         MBON19         <NA> FBbt_00111011
#> 55         MBON31         <NA> FBbt_00049851
#> 56         MBON19         <NA> FBbt_00111011
#> 57         MBON16         <NA> FBbt_00111063
#> 58         MBON16         <NA> FBbt_00111063
#> 59         MBON24         <NA> FBbt_00049844
#> 60         MBON27         <NA> FBbt_00049847
#> 61         MBON28         <NA> FBbt_00049848
#> 62         MBON21         <NA> FBbt_00111046
#> 63         MBON20         <NA> FBbt_00111013
#> 64         MBON21         <NA> FBbt_00111046
#> 65         MBON23         <NA> FBbt_00049113
#> 66         MBON12         <NA> FBbt_00111009
#> 67         MBON07         <NA> FBbt_00111005
#> 68         MBON14         <NA> FBbt_00100238
#> 69         MBON14         <NA> FBbt_00100238
#> 70         MBON11         <NA> FBbt_00100246
#> 71         MBON22         <NA> FBbt_00100240
#> 72         MBON22         <NA> FBbt_00100240
#> 73         MBON26         <NA> FBbt_00049846
#> 74         MBON33         <NA> FBbt_00049853
#> 75         MBON13         <NA> FBbt_00100239
#> 76         MBON35         <NA> FBbt_00049855
#> 77         MBON18         <NA> FBbt_00110101
#> 78         MBON04         <NA> FBbt_00111014
#> 79         MBON07         <NA> FBbt_00111005
#> 80         MBON01         <NA> FBbt_00100234
#> 81         MBON29         <NA> FBbt_00049849
#> 82         MBON07         <NA> FBbt_00111005
#> 83         MBON18         <NA> FBbt_00110101
#> 84         MBON03         <NA> FBbt_00100232
#> 85         MBON04         <NA> FBbt_00111014
#> 86         MBON09         <NA> FBbt_00111007
#> 87         MBON26         <NA> FBbt_00049846
#> 88         MBON07         <NA> FBbt_00111005
#> 89         MBON09         <NA> FBbt_00111007
#> 90         MBON06         <NA> FBbt_00100242
#> 91         MBON05         <NA> FBbt_00111004
#> 92         MBON19         <NA> FBbt_00111011
#> 93         MBON24         <NA> FBbt_00049844
#> 94  MBON25,MBON34         <NA>          <NA>
#> 95  MBON25,MBON34         <NA>          <NA>
#> 96  MBON25,MBON34         <NA>          <NA>
# the latest connectome dump, see flywire_connectome_data_version()
if (FALSE) { # \dontrun{
flytable_cell_types("MBON%", version=TRUE)
} # }

# two characters
flytable_cell_types("MBON__")
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940631916281 79448854932107696  left intrinsic     central       MBON
#> 2  720575940623472716 80011942324010318 right intrinsic     central       MBON
#> 3  720575940638526278 80715286101144676 right intrinsic     central       MBON
#> 4  720575940629585602 81137979670302132 right intrinsic     central       MBON
#> 5  720575940637934308 81349085835581377 right intrinsic     central       MBON
#> 6  720575940632118343 78744342923662254  left intrinsic     central       MBON
#> 7  720575940609959637 78603948898007978  left intrinsic     central       MBON
#> 8  720575940638163428 78111986298788517  left intrinsic     central       MBON
#> 9  720575940630496374 81349223341739625 right intrinsic     central       MBON
#> 10 720575940642142861 78393323903110735  left intrinsic     central       MBON
#> 11 720575940611344078 80223254782485510 right intrinsic     central       MBON
#> 12 720575940617552340 79588561627810154 right intrinsic     central       MBON
#> 13 720575940624280328 81489685951698728 right intrinsic     central       MBON
#> 14 720575940617567206 78180912067538056  left intrinsic     central       MBON
#> 15 720575940643700640 80152611093049624 right intrinsic     central       MBON
#> 16 720575940611984760 81137979670297348 right intrinsic     central       MBON
#> 17 720575940614026193 80154466519131949 right intrinsic     central       MBON
#> 18 720575940633209647 78463486488950059  left intrinsic     central       MBON
#> 19 720575940623001480 80154535238527400 right intrinsic     central       MBON
#> 20 720575940614595218 78040449457502610  left intrinsic     central       MBON
#> 21 720575940636992368 80785654845309410 right intrinsic     central       MBON
#> 22 720575940621828443 79166830266640003  left intrinsic     central       MBON
#> 23 720575940637902938 78181599128215629  left intrinsic     central       MBON
#> 24 720575940638774606 78040449457461921  left intrinsic     central       MBON
#> 25 720575940639697827 80224766543977933 right intrinsic     central       MBON
#> 26 720575940626833021 78251349531485275  left intrinsic     central       MBON
#> 27 720575940618008859 77901086224032815  left intrinsic     central       MBON
#> 28 720575940635841143 78110818201436707  left intrinsic     central       MBON
#> 29 720575940618249797 80082723452177136 right intrinsic     central       MBON
#> 30 720575940629856515 77408917331762608  left intrinsic     central       MBON
#> 31 720575940623905719 80224972702135904 right intrinsic     central       MBON
#> 32 720575940629529722 78533992672102680  left intrinsic     central       MBON
#> 33 720575940624590316 79167173863853528 right intrinsic     central       MBON
#> 34 720575940645304430 79380479052758063  left intrinsic     central       MBON
#> 35 720575940630767959 78251486970442209  left intrinsic     central       MBON
#> 36 720575940610647416 81137979602919932 right intrinsic     central       MBON
#> 37 720575940622093436 81136880426744139 right intrinsic     central       MBON
#> 38 720575940629422086 77760348668670642  left intrinsic     central       MBON
#> 39 720575940615435217 81137979670303492 right intrinsic     central       MBON
#> 40 720575940639556467 81067610926141558 right intrinsic     central       MBON
#> 41 720575940636120986 81700242831237167 right intrinsic     central       MBON
#> 42 720575940617749538 80786135881765925 right intrinsic     central       MBON
#> 43 720575940617760257 81277549061455173 right intrinsic     central       MBON
#> 44 720575940634482783 81136605414678093 right intrinsic     central       MBON
#> 45 720575940613906454 78182286188838645  left intrinsic     central       MBON
#> 46 720575940626109315 81066305390093306 right intrinsic     central       MBON
#> 47 720575940623377802 78110818201621392  left intrinsic     central       MBON
#> 48 720575940626744921 81277549061481189 right intrinsic     central       MBON
#> 49 720575940623464316 81136880426764424 right intrinsic     central       MBON
#> 50 720575940622979277 79027192356412856  left intrinsic     central       MBON
#> 51 720575940614892182 81277617781167541 right intrinsic     central       MBON
#> 52 720575940615756690 78111917579300796  left intrinsic     central       MBON
#> 53 720575940613090673 78324741798855874  left intrinsic     central       MBON
#> 54 720575940621777391 81137979736901319 right intrinsic     central       MBON
#> 55 720575940606953858 81136880426735755 right intrinsic     central       MBON
#> 56 720575940630075703 79309972869657633  left intrinsic     central       MBON
#> 57 720575940652390134 78603949099622269  left intrinsic     central       MBON
#> 58 720575940632535756 79380204241413766  left intrinsic     central       MBON
#> 59 720575940643696288 79309904216692475  left intrinsic     central       MBON
#> 60 720575940620162718 78323023677138555  left intrinsic     central       MBON
#> 61 720575940635063135 80082242348953693 right intrinsic     central       MBON
#> 62 720575940616463477 77830305364995511  left intrinsic     central       MBON
#> 63 720575940629981440 78885355625276424  left intrinsic     central       MBON
#> 64 720575940623182847 78463211409646517  left intrinsic     central       MBON
#> 65 720575940626315010 79943222847625326 right intrinsic     central       MBON
#> 66 720575940632943277 80925980016963911 right intrinsic     central       MBON
#> 67 720575940624539284 78040449457553649  left intrinsic     central       MBON
#> 68 720575940626180878 80011530007496199  left intrinsic     central       MBON
#> 69 720575940628783363 80434292093847745 right intrinsic     central       MBON
#> 70 720575940630390939 79870998677653042  left intrinsic     central       MBON
#> 71 720575940614392999 79237542540872430  left intrinsic     central       MBON
#> 72 720575940617302365 78604086538417636  left intrinsic     central       MBON
#> 73 720575940635651680 81277549061591572 right intrinsic     central       MBON
#> 74 720575940624694503 79941230049538531  left intrinsic     central       MBON
#> 75 720575940628734376 79869899232522382 right intrinsic     central       MBON
#> 76 720575940610964946 81630904409213619 right intrinsic     central       MBON
#> 77 720575940607155890 80433124332533007 right intrinsic     central       MBON
#> 78 720575940623381956 80644779918075196 right intrinsic     central       MBON
#> 79 720575940628487813 81560466945525625 right intrinsic     central       MBON
#> 80 720575940638028607 81419592152467685 right intrinsic     central       MBON
#> 81 720575940621164720 80222636373797362  left intrinsic     central       MBON
#> 82 720575940623841463 78251349531447591  left intrinsic     central       MBON
#> 83 720575940631177803 78814711734490195  left intrinsic     central       MBON
#>    cell_sub_class cell_type        top_nt ito_lee_hemilineage hemibrain_type
#> 1            <NA>    MBON02     glutamate    putative_primary         MBON02
#> 2            <NA>    MBON01     glutamate    putative_primary         MBON01
#> 3            <NA>    MBON32          gaba    putative_primary         MBON32
#> 4            <NA>    MBON10          gaba                EBa1         MBON10
#> 5            <NA>    MBON30     glutamate    putative_primary         MBON30
#> 6            <NA>    MBON10          gaba                EBa1         MBON10
#> 7            <NA>    MBON32          gaba    putative_primary         MBON32
#> 8            <NA>    MBON06     glutamate    putative_primary         MBON06
#> 9            <NA>    MBON05     glutamate    putative_primary         MBON05
#> 10           <NA>    MBON10     glutamate                EBa1         MBON10
#> 11           <NA>    MBON20          gaba    putative_primary         MBON20
#> 12           <NA>    MBON02          gaba    putative_primary         MBON02
#> 13           <NA>    MBON33 acetylcholine    putative_primary         MBON33
#> 14           <NA>    MBON23 acetylcholine          DL1_dorsal         MBON23
#> 15           <NA>    MBON29 acetylcholine         DL2_ventral         MBON29
#> 16           <NA>    MBON10          gaba                EBa1         MBON10
#> 17           <NA>    MBON14 acetylcholine               FLAa2         MBON14
#> 18           <NA>    MBON10     glutamate                EBa1         MBON10
#> 19           <NA>    MBON12 acetylcholine               FLAa2         MBON12
#> 20           <NA>    MBON28 acetylcholine          DL1_dorsal         MBON28
#> 21           <NA>    MBON31          gaba    putative_primary         MBON31
#> 22           <NA>    MBON12 acetylcholine               FLAa2         MBON12
#> 23           <NA>    MBON35 acetylcholine    putative_primary         MBON35
#> 24           <NA>    MBON17 acetylcholine          DL1_dorsal         MBON17
#> 25           <NA>    MBON14 acetylcholine               FLAa2         MBON14
#> 26           <NA>    MBON19 acetylcholine          DL1_dorsal         MBON19
#> 27           <NA>    MBON30     glutamate    putative_primary         MBON30
#> 28           <NA>    MBON15 acetylcholine          DL1_dorsal         MBON15
#> 29           <NA>    MBON27 acetylcholine    putative_primary         MBON27
#> 30           <NA>    MBON09          gaba    putative_primary         MBON09
#> 31           <NA>    MBON12 acetylcholine               FLAa2         MBON12
#> 32           <NA>    MBON10     glutamate                EBa1         MBON10
#> 33           <NA>    MBON03     glutamate    putative_primary         MBON03
#> 34           <NA>    MBON13 acetylcholine               FLAa2         MBON13
#> 35           <NA>    MBON15 acetylcholine          DL1_dorsal         MBON15
#> 36           <NA>    MBON10          gaba                EBa1         MBON10
#> 37           <NA>    MBON15 acetylcholine          DL1_dorsal         MBON15
#> 38           <NA>    MBON09          gaba    putative_primary         MBON09
#> 39           <NA>    MBON10          gaba                EBa1         MBON10
#> 40           <NA>    MBON10          gaba                EBa1         MBON10
#> 41           <NA>    MBON15 acetylcholine          DL1_dorsal         MBON15
#> 42           <NA>    MBON11          gaba    putative_primary         MBON11
#> 43           <NA>    MBON17 acetylcholine          DL1_dorsal         MBON17
#> 44           <NA>    MBON19 acetylcholine          DL1_dorsal         MBON19
#> 45           <NA>    MBON31          gaba    putative_primary         MBON31
#> 46           <NA>    MBON19 acetylcholine          DL1_dorsal         MBON19
#> 47           <NA>    MBON16 acetylcholine          DL1_dorsal         MBON16
#> 48           <NA>    MBON16 acetylcholine          DL1_dorsal         MBON16
#> 49           <NA>    MBON24 acetylcholine    putative_primary         MBON24
#> 50           <NA>    MBON27 acetylcholine    putative_primary         MBON27
#> 51           <NA>    MBON28 acetylcholine          DL1_dorsal         MBON28
#> 52           <NA>    MBON21 acetylcholine    putative_primary         MBON21
#> 53           <NA>    MBON20          gaba    putative_primary         MBON20
#> 54           <NA>    MBON21 acetylcholine    putative_primary         MBON21
#> 55           <NA>    MBON23 acetylcholine          DL1_dorsal         MBON23
#> 56           <NA>    MBON12 acetylcholine               FLAa2         MBON12
#> 57           <NA>    MBON07     glutamate    putative_primary         MBON07
#> 58           <NA>    MBON14 acetylcholine               FLAa2         MBON14
#> 59           <NA>    MBON14 acetylcholine               FLAa2         MBON14
#> 60           <NA>    MBON11          gaba    putative_primary         MBON11
#> 61           <NA>    MBON22 acetylcholine    putative_primary         MBON22
#> 62           <NA>    MBON22 acetylcholine    putative_primary         MBON22
#> 63           <NA>    MBON26 acetylcholine    putative_primary         MBON26
#> 64           <NA>    MBON33 acetylcholine    putative_primary         MBON33
#> 65           <NA>    MBON13 acetylcholine               FLAa2         MBON13
#> 66           <NA>    MBON35 acetylcholine    putative_primary         MBON35
#> 67           <NA>    MBON18 acetylcholine          DL1_dorsal         MBON18
#> 68           <NA>    MBON04     glutamate    putative_primary         MBON04
#> 69           <NA>    MBON07     glutamate    putative_primary         MBON07
#> 70           <NA>    MBON01     glutamate    putative_primary         MBON01
#> 71           <NA>    MBON29 acetylcholine         DL2_ventral         MBON29
#> 72           <NA>    MBON07     glutamate    putative_primary         MBON07
#> 73           <NA>    MBON18 acetylcholine          DL1_dorsal         MBON18
#> 74           <NA>    MBON03     glutamate    putative_primary         MBON03
#> 75           <NA>    MBON04     glutamate    putative_primary         MBON04
#> 76           <NA>    MBON09          gaba    putative_primary         MBON09
#> 77           <NA>    MBON26 acetylcholine    putative_primary         MBON26
#> 78           <NA>    MBON07     glutamate    putative_primary         MBON07
#> 79           <NA>    MBON09          gaba    putative_primary         MBON09
#> 80           <NA>    MBON06     glutamate    putative_primary         MBON06
#> 81           <NA>    MBON05 acetylcholine    putative_primary         MBON05
#> 82           <NA>    MBON19 acetylcholine          DL1_dorsal         MBON19
#> 83           <NA>    MBON24 acetylcholine    putative_primary         MBON24
#>    malecns_type       fbbt_id
#> 1          <NA> FBbt_00111012
#> 2          <NA> FBbt_00100234
#> 3          <NA> FBbt_00049852
#> 4          <NA> FBbt_00111008
#> 5          <NA> FBbt_00049850
#> 6          <NA> FBbt_00111008
#> 7          <NA> FBbt_00049852
#> 8          <NA> FBbt_00100242
#> 9          <NA> FBbt_00111004
#> 10         <NA> FBbt_00111008
#> 11         <NA> FBbt_00111013
#> 12         <NA> FBbt_00111012
#> 13         <NA> FBbt_00049853
#> 14         <NA> FBbt_00049113
#> 15         <NA> FBbt_00049849
#> 16         <NA> FBbt_00111008
#> 17         <NA> FBbt_00100238
#> 18         <NA> FBbt_00111008
#> 19         <NA> FBbt_00111009
#> 20         <NA> FBbt_00049848
#> 21         <NA> FBbt_00049851
#> 22         <NA> FBbt_00111009
#> 23         <NA> FBbt_00049855
#> 24         <NA> FBbt_00111064
#> 25         <NA> FBbt_00100238
#> 26         <NA> FBbt_00111011
#> 27         <NA> FBbt_00049850
#> 28         <NA> FBbt_00111010
#> 29         <NA> FBbt_00049847
#> 30         <NA> FBbt_00111007
#> 31         <NA> FBbt_00111009
#> 32         <NA> FBbt_00111008
#> 33         <NA> FBbt_00100232
#> 34         <NA> FBbt_00100239
#> 35         <NA> FBbt_00111010
#> 36         <NA> FBbt_00111008
#> 37         <NA> FBbt_00111010
#> 38         <NA> FBbt_00111007
#> 39         <NA> FBbt_00111008
#> 40         <NA> FBbt_00111008
#> 41         <NA> FBbt_00111010
#> 42         <NA> FBbt_00100246
#> 43         <NA> FBbt_00111064
#> 44         <NA> FBbt_00111011
#> 45         <NA> FBbt_00049851
#> 46         <NA> FBbt_00111011
#> 47         <NA> FBbt_00111063
#> 48         <NA> FBbt_00111063
#> 49         <NA> FBbt_00049844
#> 50         <NA> FBbt_00049847
#> 51         <NA> FBbt_00049848
#> 52         <NA> FBbt_00111046
#> 53         <NA> FBbt_00111013
#> 54         <NA> FBbt_00111046
#> 55         <NA> FBbt_00049113
#> 56         <NA> FBbt_00111009
#> 57         <NA> FBbt_00111005
#> 58         <NA> FBbt_00100238
#> 59         <NA> FBbt_00100238
#> 60         <NA> FBbt_00100246
#> 61         <NA> FBbt_00100240
#> 62         <NA> FBbt_00100240
#> 63         <NA> FBbt_00049846
#> 64         <NA> FBbt_00049853
#> 65         <NA> FBbt_00100239
#> 66         <NA> FBbt_00049855
#> 67         <NA> FBbt_00110101
#> 68         <NA> FBbt_00111014
#> 69         <NA> FBbt_00111005
#> 70         <NA> FBbt_00100234
#> 71         <NA> FBbt_00049849
#> 72         <NA> FBbt_00111005
#> 73         <NA> FBbt_00110101
#> 74         <NA> FBbt_00100232
#> 75         <NA> FBbt_00111014
#> 76         <NA> FBbt_00111007
#> 77         <NA> FBbt_00049846
#> 78         <NA> FBbt_00111005
#> 79         <NA> FBbt_00111007
#> 80         <NA> FBbt_00100242
#> 81         <NA> FBbt_00111004
#> 82         <NA> FBbt_00111011
#> 83         <NA> FBbt_00049844
# at least one character
flytable_cell_types("MBON_%")
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940631916281 79448854932107696  left intrinsic     central       MBON
#> 2  720575940623472716 80011942324010318 right intrinsic     central       MBON
#> 3  720575940638526278 80715286101144676 right intrinsic     central       MBON
#> 4  720575940629585602 81137979670302132 right intrinsic     central       MBON
#> 5  720575940637934308 81349085835581377 right intrinsic     central       MBON
#> 6  720575940632118343 78744342923662254  left intrinsic     central       MBON
#> 7  720575940609959637 78603948898007978  left intrinsic     central       MBON
#> 8  720575940638163428 78111986298788517  left intrinsic     central       MBON
#> 9  720575940630496374 81349223341739625 right intrinsic     central       MBON
#> 10 720575940642142861 78393323903110735  left intrinsic     central       MBON
#> 11 720575940611344078 80223254782485510 right intrinsic     central       MBON
#> 12 720575940634822751 81277549061520124 right intrinsic     central       MBON
#> 13 720575940617552340 79588561627810154 right intrinsic     central       MBON
#> 14 720575940624280328 81489685951698728 right intrinsic     central       MBON
#> 15 720575940617567206 78180912067538056  left intrinsic     central       MBON
#> 16 720575940643700640 80152611093049624 right intrinsic     central       MBON
#> 17 720575940611984760 81137979670297348 right intrinsic     central       MBON
#> 18 720575940614026193 80154466519131949 right intrinsic     central       MBON
#> 19 720575940633209647 78463486488950059  left intrinsic     central       MBON
#> 20 720575940623001480 80154535238527400 right intrinsic     central       MBON
#> 21 720575940613070485 81066305390179956 right intrinsic     central       MBON
#> 22 720575940614595218 78040449457502610  left intrinsic     central       MBON
#> 23 720575940636992368 80785654845309410 right intrinsic     central       MBON
#> 24 720575940621828443 79166830266640003  left intrinsic     central       MBON
#> 25 720575940637902938 78181599128215629  left intrinsic     central       MBON
#> 26 720575940638774606 78040449457461921  left intrinsic     central       MBON
#> 27 720575940639697827 80224766543977933 right intrinsic     central       MBON
#> 28 720575940626833021 78251349531485275  left intrinsic     central       MBON
#> 29 720575940618008859 77901086224032815  left intrinsic     central       MBON
#> 30 720575940635841143 78110818201436707  left intrinsic     central       MBON
#> 31 720575940618249797 80082723452177136 right intrinsic     central       MBON
#> 32 720575940629856515 77408917331762608  left intrinsic     central       MBON
#> 33 720575940623905719 80224972702135904 right intrinsic     central       MBON
#> 34 720575940629529722 78533992672102680  left intrinsic     central       MBON
#> 35 720575940624659943 80011255129536552  left intrinsic     central       MBON
#> 36 720575940624590316 79167173863853528 right intrinsic     central       MBON
#> 37 720575940645304430 79380479052758063  left intrinsic     central       MBON
#> 38 720575940612196850 81136880426746412 right intrinsic     central       MBON
#> 39 720575940630767959 78251486970442209  left intrinsic     central       MBON
#> 40 720575940640963747 76985537105536911  left intrinsic     central       MBON
#> 41 720575940628757547 78040449457463315  left intrinsic     central       MBON
#> 42 720575940627144069 79166692827138268 right intrinsic     central       MBON
#> 43 720575940610647416 81137979602919932 right intrinsic     central       MBON
#> 44 720575940622093436 81136880426744139 right intrinsic     central       MBON
#> 45 720575940624696810 78110818201651448  left intrinsic     central       MBON
#> 46 720575940629422086 77760348668670642  left intrinsic     central       MBON
#> 47 720575940615435217 81137979670303492 right intrinsic     central       MBON
#> 48 720575940650386553 78110818201681795  left intrinsic     central       MBON
#> 49 720575940639556467 81067610926141558 right intrinsic     central       MBON
#> 50 720575940636120986 81700242831237167 right intrinsic     central       MBON
#> 51 720575940617749538 80786135881765925 right intrinsic     central       MBON
#> 52 720575940617760257 81277549061455173 right intrinsic     central       MBON
#> 53 720575940619810389 79518055445188833 right intrinsic     central       MBON
#> 54 720575940634482783 81136605414678093 right intrinsic     central       MBON
#> 55 720575940613906454 78182286188838645  left intrinsic     central       MBON
#> 56 720575940626109315 81066305390093306 right intrinsic     central       MBON
#> 57 720575940623377802 78110818201621392  left intrinsic     central       MBON
#> 58 720575940626744921 81277549061481189 right intrinsic     central       MBON
#> 59 720575940623464316 81136880426764424 right intrinsic     central       MBON
#> 60 720575940622979277 79027192356412856  left intrinsic     central       MBON
#> 61 720575940614892182 81277617781167541 right intrinsic     central       MBON
#> 62 720575940615756690 78111917579300796  left intrinsic     central       MBON
#> 63 720575940613090673 78324741798855874  left intrinsic     central       MBON
#> 64 720575940621777391 81137979736901319 right intrinsic     central       MBON
#> 65 720575940606953858 81136880426735755 right intrinsic     central       MBON
#> 66 720575940630075703 79309972869657633  left intrinsic     central       MBON
#> 67 720575940652390134 78603949099622269  left intrinsic     central       MBON
#> 68 720575940632535756 79380204241413766  left intrinsic     central       MBON
#> 69 720575940643696288 79309904216692475  left intrinsic     central       MBON
#> 70 720575940620162718 78323023677138555  left intrinsic     central       MBON
#> 71 720575940635063135 80082242348953693 right intrinsic     central       MBON
#> 72 720575940616463477 77830305364995511  left intrinsic     central       MBON
#> 73 720575940629981440 78885355625276424  left intrinsic     central       MBON
#> 74 720575940623182847 78463211409646517  left intrinsic     central       MBON
#> 75 720575940626315010 79943222847625326 right intrinsic     central       MBON
#> 76 720575940632943277 80925980016963911 right intrinsic     central       MBON
#> 77 720575940624539284 78040449457553649  left intrinsic     central       MBON
#> 78 720575940626180878 80011530007496199  left intrinsic     central       MBON
#> 79 720575940628783363 80434292093847745 right intrinsic     central       MBON
#> 80 720575940630390939 79870998677653042  left intrinsic     central       MBON
#> 81 720575940614392999 79237542540872430  left intrinsic     central       MBON
#> 82 720575940617302365 78604086538417636  left intrinsic     central       MBON
#> 83 720575940635651680 81277549061591572 right intrinsic     central       MBON
#> 84 720575940624694503 79941230049538531  left intrinsic     central       MBON
#> 85 720575940628734376 79869899232522382 right intrinsic     central       MBON
#> 86 720575940610964946 81630904409213619 right intrinsic     central       MBON
#> 87 720575940607155890 80433124332533007 right intrinsic     central       MBON
#> 88 720575940623381956 80644779918075196 right intrinsic     central       MBON
#> 89 720575940628487813 81560466945525625 right intrinsic     central       MBON
#> 90 720575940638028607 81419592152467685 right intrinsic     central       MBON
#> 91 720575940621164720 80222636373797362  left intrinsic     central       MBON
#> 92 720575940623841463 78251349531447591  left intrinsic     central       MBON
#> 93 720575940631177803 78814711734490195  left intrinsic     central       MBON
#> 94 720575940622306625 79166761480079751 right intrinsic     central       MBON
#> 95 720575940616398303 79729161744437991  left intrinsic     central       MBON
#> 96 720575940613815210 79729161744426207  left intrinsic     central       MBON
#>    cell_sub_class     cell_type        top_nt ito_lee_hemilineage
#> 1            <NA>        MBON02     glutamate    putative_primary
#> 2            <NA>        MBON01     glutamate    putative_primary
#> 3            <NA>        MBON32          gaba    putative_primary
#> 4            <NA>        MBON10          gaba                EBa1
#> 5            <NA>        MBON30     glutamate    putative_primary
#> 6            <NA>        MBON10          gaba                EBa1
#> 7            <NA>        MBON32          gaba    putative_primary
#> 8            <NA>        MBON06     glutamate    putative_primary
#> 9            <NA>        MBON05     glutamate    putative_primary
#> 10           <NA>        MBON10     glutamate                EBa1
#> 11           <NA>        MBON20          gaba    putative_primary
#> 12           <NA>   MBON17-like acetylcholine          DL1_dorsal
#> 13           <NA>        MBON02          gaba    putative_primary
#> 14           <NA>        MBON33 acetylcholine    putative_primary
#> 15           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 16           <NA>        MBON29 acetylcholine         DL2_ventral
#> 17           <NA>        MBON10          gaba                EBa1
#> 18           <NA>        MBON14 acetylcholine               FLAa2
#> 19           <NA>        MBON10     glutamate                EBa1
#> 20           <NA>        MBON12 acetylcholine               FLAa2
#> 21           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 22           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 23           <NA>        MBON31          gaba    putative_primary
#> 24           <NA>        MBON12 acetylcholine               FLAa2
#> 25           <NA>        MBON35 acetylcholine    putative_primary
#> 26           <NA>        MBON17 acetylcholine          DL1_dorsal
#> 27           <NA>        MBON14 acetylcholine               FLAa2
#> 28           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 29           <NA>        MBON30     glutamate    putative_primary
#> 30           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 31           <NA>        MBON27 acetylcholine    putative_primary
#> 32           <NA>        MBON09          gaba    putative_primary
#> 33           <NA>        MBON12 acetylcholine               FLAa2
#> 34           <NA>        MBON10     glutamate                EBa1
#> 35           <NA> MBON25,MBON34     glutamate              SMPad3
#> 36           <NA>        MBON03     glutamate    putative_primary
#> 37           <NA>        MBON13 acetylcholine               FLAa2
#> 38           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 39           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 40           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 41           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 42           <NA> MBON25,MBON34     glutamate              SMPad3
#> 43           <NA>        MBON10          gaba                EBa1
#> 44           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 45           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 46           <NA>        MBON09          gaba    putative_primary
#> 47           <NA>        MBON10          gaba                EBa1
#> 48           <NA>   MBON17-like acetylcholine          DL1_dorsal
#> 49           <NA>        MBON10          gaba                EBa1
#> 50           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 51           <NA>        MBON11          gaba    putative_primary
#> 52           <NA>        MBON17 acetylcholine          DL1_dorsal
#> 53           <NA> MBON25,MBON34     glutamate              SMPad3
#> 54           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 55           <NA>        MBON31          gaba    putative_primary
#> 56           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 57           <NA>        MBON16 acetylcholine          DL1_dorsal
#> 58           <NA>        MBON16 acetylcholine          DL1_dorsal
#> 59           <NA>        MBON24 acetylcholine    putative_primary
#> 60           <NA>        MBON27 acetylcholine    putative_primary
#> 61           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 62           <NA>        MBON21 acetylcholine    putative_primary
#> 63           <NA>        MBON20          gaba    putative_primary
#> 64           <NA>        MBON21 acetylcholine    putative_primary
#> 65           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 66           <NA>        MBON12 acetylcholine               FLAa2
#> 67           <NA>        MBON07     glutamate    putative_primary
#> 68           <NA>        MBON14 acetylcholine               FLAa2
#> 69           <NA>        MBON14 acetylcholine               FLAa2
#> 70           <NA>        MBON11          gaba    putative_primary
#> 71           <NA>        MBON22 acetylcholine    putative_primary
#> 72           <NA>        MBON22 acetylcholine    putative_primary
#> 73           <NA>        MBON26 acetylcholine    putative_primary
#> 74           <NA>        MBON33 acetylcholine    putative_primary
#> 75           <NA>        MBON13 acetylcholine               FLAa2
#> 76           <NA>        MBON35 acetylcholine    putative_primary
#> 77           <NA>        MBON18 acetylcholine          DL1_dorsal
#> 78           <NA>        MBON04     glutamate    putative_primary
#> 79           <NA>        MBON07     glutamate    putative_primary
#> 80           <NA>        MBON01     glutamate    putative_primary
#> 81           <NA>        MBON29 acetylcholine         DL2_ventral
#> 82           <NA>        MBON07     glutamate    putative_primary
#> 83           <NA>        MBON18 acetylcholine          DL1_dorsal
#> 84           <NA>        MBON03     glutamate    putative_primary
#> 85           <NA>        MBON04     glutamate    putative_primary
#> 86           <NA>        MBON09          gaba    putative_primary
#> 87           <NA>        MBON26 acetylcholine    putative_primary
#> 88           <NA>        MBON07     glutamate    putative_primary
#> 89           <NA>        MBON09          gaba    putative_primary
#> 90           <NA>        MBON06     glutamate    putative_primary
#> 91           <NA>        MBON05 acetylcholine    putative_primary
#> 92           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 93           <NA>        MBON24 acetylcholine    putative_primary
#> 94           <NA> MBON25,MBON34     glutamate              SMPad3
#> 95           <NA> MBON25,MBON34     glutamate              SMPad3
#> 96           <NA> MBON25,MBON34     glutamate              SMPad3
#>    hemibrain_type malecns_type       fbbt_id
#> 1          MBON02         <NA> FBbt_00111012
#> 2          MBON01         <NA> FBbt_00100234
#> 3          MBON32         <NA> FBbt_00049852
#> 4          MBON10         <NA> FBbt_00111008
#> 5          MBON30         <NA> FBbt_00049850
#> 6          MBON10         <NA> FBbt_00111008
#> 7          MBON32         <NA> FBbt_00049852
#> 8          MBON06         <NA> FBbt_00100242
#> 9          MBON05         <NA> FBbt_00111004
#> 10         MBON10         <NA> FBbt_00111008
#> 11         MBON20         <NA> FBbt_00111013
#> 12    MBON17-like         <NA>          <NA>
#> 13         MBON02         <NA> FBbt_00111012
#> 14         MBON33         <NA> FBbt_00049853
#> 15         MBON23         <NA> FBbt_00049113
#> 16         MBON29         <NA> FBbt_00049849
#> 17         MBON10         <NA> FBbt_00111008
#> 18         MBON14         <NA> FBbt_00100238
#> 19         MBON10         <NA> FBbt_00111008
#> 20         MBON12         <NA> FBbt_00111009
#> 21    MBON15-like         <NA>          <NA>
#> 22         MBON28         <NA> FBbt_00049848
#> 23         MBON31         <NA> FBbt_00049851
#> 24         MBON12         <NA> FBbt_00111009
#> 25         MBON35         <NA> FBbt_00049855
#> 26         MBON17         <NA> FBbt_00111064
#> 27         MBON14         <NA> FBbt_00100238
#> 28         MBON19         <NA> FBbt_00111011
#> 29         MBON30         <NA> FBbt_00049850
#> 30         MBON15         <NA> FBbt_00111010
#> 31         MBON27         <NA> FBbt_00049847
#> 32         MBON09         <NA> FBbt_00111007
#> 33         MBON12         <NA> FBbt_00111009
#> 34         MBON10         <NA> FBbt_00111008
#> 35  MBON25,MBON34         <NA>          <NA>
#> 36         MBON03         <NA> FBbt_00100232
#> 37         MBON13         <NA> FBbt_00100239
#> 38    MBON15-like         <NA>          <NA>
#> 39         MBON15         <NA> FBbt_00111010
#> 40    MBON15-like         <NA>          <NA>
#> 41    MBON15-like         <NA>          <NA>
#> 42  MBON25,MBON34         <NA>          <NA>
#> 43         MBON10         <NA> FBbt_00111008
#> 44         MBON15         <NA> FBbt_00111010
#> 45    MBON15-like         <NA>          <NA>
#> 46         MBON09         <NA> FBbt_00111007
#> 47         MBON10         <NA> FBbt_00111008
#> 48    MBON17-like         <NA>          <NA>
#> 49         MBON10         <NA> FBbt_00111008
#> 50         MBON15         <NA> FBbt_00111010
#> 51         MBON11         <NA> FBbt_00100246
#> 52         MBON17         <NA> FBbt_00111064
#> 53  MBON25,MBON34         <NA>          <NA>
#> 54         MBON19         <NA> FBbt_00111011
#> 55         MBON31         <NA> FBbt_00049851
#> 56         MBON19         <NA> FBbt_00111011
#> 57         MBON16         <NA> FBbt_00111063
#> 58         MBON16         <NA> FBbt_00111063
#> 59         MBON24         <NA> FBbt_00049844
#> 60         MBON27         <NA> FBbt_00049847
#> 61         MBON28         <NA> FBbt_00049848
#> 62         MBON21         <NA> FBbt_00111046
#> 63         MBON20         <NA> FBbt_00111013
#> 64         MBON21         <NA> FBbt_00111046
#> 65         MBON23         <NA> FBbt_00049113
#> 66         MBON12         <NA> FBbt_00111009
#> 67         MBON07         <NA> FBbt_00111005
#> 68         MBON14         <NA> FBbt_00100238
#> 69         MBON14         <NA> FBbt_00100238
#> 70         MBON11         <NA> FBbt_00100246
#> 71         MBON22         <NA> FBbt_00100240
#> 72         MBON22         <NA> FBbt_00100240
#> 73         MBON26         <NA> FBbt_00049846
#> 74         MBON33         <NA> FBbt_00049853
#> 75         MBON13         <NA> FBbt_00100239
#> 76         MBON35         <NA> FBbt_00049855
#> 77         MBON18         <NA> FBbt_00110101
#> 78         MBON04         <NA> FBbt_00111014
#> 79         MBON07         <NA> FBbt_00111005
#> 80         MBON01         <NA> FBbt_00100234
#> 81         MBON29         <NA> FBbt_00049849
#> 82         MBON07         <NA> FBbt_00111005
#> 83         MBON18         <NA> FBbt_00110101
#> 84         MBON03         <NA> FBbt_00100232
#> 85         MBON04         <NA> FBbt_00111014
#> 86         MBON09         <NA> FBbt_00111007
#> 87         MBON26         <NA> FBbt_00049846
#> 88         MBON07         <NA> FBbt_00111005
#> 89         MBON09         <NA> FBbt_00111007
#> 90         MBON06         <NA> FBbt_00100242
#> 91         MBON05         <NA> FBbt_00111004
#> 92         MBON19         <NA> FBbt_00111011
#> 93         MBON24         <NA> FBbt_00049844
#> 94  MBON25,MBON34         <NA>          <NA>
#> 95  MBON25,MBON34         <NA>          <NA>
#> 96  MBON25,MBON34         <NA>          <NA>
# range
flytable_cell_types("MBON2[0-5]")
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940611344078 80223254782485510 right intrinsic     central       MBON
#> 2  720575940617567206 78180912067538056  left intrinsic     central       MBON
#> 3  720575940623464316 81136880426764424 right intrinsic     central       MBON
#> 4  720575940615756690 78111917579300796  left intrinsic     central       MBON
#> 5  720575940613090673 78324741798855874  left intrinsic     central       MBON
#> 6  720575940621777391 81137979736901319 right intrinsic     central       MBON
#> 7  720575940606953858 81136880426735755 right intrinsic     central       MBON
#> 8  720575940635063135 80082242348953693 right intrinsic     central       MBON
#> 9  720575940616463477 77830305364995511  left intrinsic     central       MBON
#> 10 720575940631177803 78814711734490195  left intrinsic     central       MBON
#>    cell_sub_class cell_type        top_nt ito_lee_hemilineage hemibrain_type
#> 1            <NA>    MBON20          gaba    putative_primary         MBON20
#> 2            <NA>    MBON23 acetylcholine          DL1_dorsal         MBON23
#> 3            <NA>    MBON24 acetylcholine    putative_primary         MBON24
#> 4            <NA>    MBON21 acetylcholine    putative_primary         MBON21
#> 5            <NA>    MBON20          gaba    putative_primary         MBON20
#> 6            <NA>    MBON21 acetylcholine    putative_primary         MBON21
#> 7            <NA>    MBON23 acetylcholine          DL1_dorsal         MBON23
#> 8            <NA>    MBON22 acetylcholine    putative_primary         MBON22
#> 9            <NA>    MBON22 acetylcholine    putative_primary         MBON22
#> 10           <NA>    MBON24 acetylcholine    putative_primary         MBON24
#>    malecns_type       fbbt_id
#> 1          <NA> FBbt_00111013
#> 2          <NA> FBbt_00049113
#> 3          <NA> FBbt_00049844
#> 4          <NA> FBbt_00111046
#> 5          <NA> FBbt_00111013
#> 6          <NA> FBbt_00111046
#> 7          <NA> FBbt_00049113
#> 8          <NA> FBbt_00100240
#> 9          <NA> FBbt_00100240
#> 10         <NA> FBbt_00049844

# include side specification
flytable_cell_types("DA2_lPN_R")
#>              root_id     supervoxel_id  side      flow super_class cell_class
#> 1 720575940639337461 80646154374848875 right intrinsic     central       ALPN
#> 2 720575940628259407 81349910469210197 right intrinsic     central       ALPN
#> 3 720575940627372837 80575785630612215 right intrinsic     central       ALPN
#> 4 720575940622762995 80575785630553529 right intrinsic     central       ALPN
#> 5 720575940622311704 80646154374760379 right intrinsic     central       ALPN
#> 6 720575940610052266 80646154374867885 right intrinsic     central       ALPN
#>   cell_sub_class cell_type        top_nt ito_lee_hemilineage hemibrain_type
#> 1  uniglomerular   DA2_lPN acetylcholine        ALl1_ventral        DA2_lPN
#> 2  uniglomerular   DA2_lPN acetylcholine        ALl1_ventral        DA2_lPN
#> 3  uniglomerular   DA2_lPN acetylcholine        ALl1_ventral        DA2_lPN
#> 4  uniglomerular   DA2_lPN acetylcholine        ALl1_ventral        DA2_lPN
#> 5  uniglomerular   DA2_lPN acetylcholine        ALl1_ventral        DA2_lPN
#> 6  uniglomerular   DA2_lPN acetylcholine        ALl1_ventral        DA2_lPN
#>   malecns_type       fbbt_id
#> 1         <NA> FBbt_00110882
#> 2         <NA> FBbt_00110882
#> 3         <NA> FBbt_00110882
#> 4         <NA> FBbt_00110882
#> 5         <NA> FBbt_00110882
#> 6         <NA> FBbt_00110882
# only the RHS MBON20
flytable_cell_types("MBON20_R")
#>              root_id     supervoxel_id  side      flow super_class cell_class
#> 1 720575940611344078 80223254782485510 right intrinsic     central       MBON
#>   cell_sub_class cell_type top_nt ito_lee_hemilineage hemibrain_type
#> 1           <NA>    MBON20   gaba    putative_primary         MBON20
#>   malecns_type       fbbt_id
#> 1         <NA> FBbt_00111013
# all RHS cells with class MBON
flytable_cell_types("MBON_R", target="cell_class")
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940623472716 80011942324010318 right intrinsic     central       MBON
#> 2  720575940638526278 80715286101144676 right intrinsic     central       MBON
#> 3  720575940629585602 81137979670302132 right intrinsic     central       MBON
#> 4  720575940637934308 81349085835581377 right intrinsic     central       MBON
#> 5  720575940630496374 81349223341739625 right intrinsic     central       MBON
#> 6  720575940611344078 80223254782485510 right intrinsic     central       MBON
#> 7  720575940634822751 81277549061520124 right intrinsic     central       MBON
#> 8  720575940617552340 79588561627810154 right intrinsic     central       MBON
#> 9  720575940624280328 81489685951698728 right intrinsic     central       MBON
#> 10 720575940643700640 80152611093049624 right intrinsic     central       MBON
#> 11 720575940611984760 81137979670297348 right intrinsic     central       MBON
#> 12 720575940614026193 80154466519131949 right intrinsic     central       MBON
#> 13 720575940623001480 80154535238527400 right intrinsic     central       MBON
#> 14 720575940613070485 81066305390179956 right intrinsic     central       MBON
#> 15 720575940636992368 80785654845309410 right intrinsic     central       MBON
#> 16 720575940639697827 80224766543977933 right intrinsic     central       MBON
#> 17 720575940618249797 80082723452177136 right intrinsic     central       MBON
#> 18 720575940623905719 80224972702135904 right intrinsic     central       MBON
#> 19 720575940624590316 79167173863853528 right intrinsic     central       MBON
#> 20 720575940612196850 81136880426746412 right intrinsic     central       MBON
#> 21 720575940627144069 79166692827138268 right intrinsic     central       MBON
#> 22 720575940610647416 81137979602919932 right intrinsic     central       MBON
#> 23 720575940622093436 81136880426744139 right intrinsic     central       MBON
#> 24 720575940615435217 81137979670303492 right intrinsic     central       MBON
#> 25 720575940639556467 81067610926141558 right intrinsic     central       MBON
#> 26 720575940636120986 81700242831237167 right intrinsic     central       MBON
#> 27 720575940617749538 80786135881765925 right intrinsic     central       MBON
#> 28 720575940617760257 81277549061455173 right intrinsic     central       MBON
#> 29 720575940619810389 79518055445188833 right intrinsic     central       MBON
#> 30 720575940634482783 81136605414678093 right intrinsic     central       MBON
#> 31 720575940626109315 81066305390093306 right intrinsic     central       MBON
#> 32 720575940626744921 81277549061481189 right intrinsic     central       MBON
#> 33 720575940623464316 81136880426764424 right intrinsic     central       MBON
#> 34 720575940614892182 81277617781167541 right intrinsic     central       MBON
#> 35 720575940621777391 81137979736901319 right intrinsic     central       MBON
#> 36 720575940606953858 81136880426735755 right intrinsic     central       MBON
#> 37 720575940635063135 80082242348953693 right intrinsic     central       MBON
#> 38 720575940626315010 79943222847625326 right intrinsic     central       MBON
#> 39 720575940632943277 80925980016963911 right intrinsic     central       MBON
#> 40 720575940628783363 80434292093847745 right intrinsic     central       MBON
#> 41 720575940635651680 81277549061591572 right intrinsic     central       MBON
#> 42 720575940628734376 79869899232522382 right intrinsic     central       MBON
#> 43 720575940610964946 81630904409213619 right intrinsic     central       MBON
#> 44 720575940607155890 80433124332533007 right intrinsic     central       MBON
#> 45 720575940623381956 80644779918075196 right intrinsic     central       MBON
#> 46 720575940628487813 81560466945525625 right intrinsic     central       MBON
#> 47 720575940638028607 81419592152467685 right intrinsic     central       MBON
#> 48 720575940622306625 79166761480079751 right intrinsic     central       MBON
#>    cell_sub_class     cell_type        top_nt ito_lee_hemilineage
#> 1            <NA>        MBON01     glutamate    putative_primary
#> 2            <NA>        MBON32          gaba    putative_primary
#> 3            <NA>        MBON10          gaba                EBa1
#> 4            <NA>        MBON30     glutamate    putative_primary
#> 5            <NA>        MBON05     glutamate    putative_primary
#> 6            <NA>        MBON20          gaba    putative_primary
#> 7            <NA>   MBON17-like acetylcholine          DL1_dorsal
#> 8            <NA>        MBON02          gaba    putative_primary
#> 9            <NA>        MBON33 acetylcholine    putative_primary
#> 10           <NA>        MBON29 acetylcholine         DL2_ventral
#> 11           <NA>        MBON10          gaba                EBa1
#> 12           <NA>        MBON14 acetylcholine               FLAa2
#> 13           <NA>        MBON12 acetylcholine               FLAa2
#> 14           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 15           <NA>        MBON31          gaba    putative_primary
#> 16           <NA>        MBON14 acetylcholine               FLAa2
#> 17           <NA>        MBON27 acetylcholine    putative_primary
#> 18           <NA>        MBON12 acetylcholine               FLAa2
#> 19           <NA>        MBON03     glutamate    putative_primary
#> 20           <NA>   MBON15-like acetylcholine          DL1_dorsal
#> 21           <NA> MBON25,MBON34     glutamate              SMPad3
#> 22           <NA>        MBON10          gaba                EBa1
#> 23           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 24           <NA>        MBON10          gaba                EBa1
#> 25           <NA>        MBON10          gaba                EBa1
#> 26           <NA>        MBON15 acetylcholine          DL1_dorsal
#> 27           <NA>        MBON11          gaba    putative_primary
#> 28           <NA>        MBON17 acetylcholine          DL1_dorsal
#> 29           <NA> MBON25,MBON34     glutamate              SMPad3
#> 30           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 31           <NA>        MBON19 acetylcholine          DL1_dorsal
#> 32           <NA>        MBON16 acetylcholine          DL1_dorsal
#> 33           <NA>        MBON24 acetylcholine    putative_primary
#> 34           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 35           <NA>        MBON21 acetylcholine    putative_primary
#> 36           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 37           <NA>        MBON22 acetylcholine    putative_primary
#> 38           <NA>        MBON13 acetylcholine               FLAa2
#> 39           <NA>        MBON35 acetylcholine    putative_primary
#> 40           <NA>        MBON07     glutamate    putative_primary
#> 41           <NA>        MBON18 acetylcholine          DL1_dorsal
#> 42           <NA>        MBON04     glutamate    putative_primary
#> 43           <NA>        MBON09          gaba    putative_primary
#> 44           <NA>        MBON26 acetylcholine    putative_primary
#> 45           <NA>        MBON07     glutamate    putative_primary
#> 46           <NA>        MBON09          gaba    putative_primary
#> 47           <NA>        MBON06     glutamate    putative_primary
#> 48           <NA> MBON25,MBON34     glutamate              SMPad3
#>    hemibrain_type malecns_type       fbbt_id
#> 1          MBON01         <NA> FBbt_00100234
#> 2          MBON32         <NA> FBbt_00049852
#> 3          MBON10         <NA> FBbt_00111008
#> 4          MBON30         <NA> FBbt_00049850
#> 5          MBON05         <NA> FBbt_00111004
#> 6          MBON20         <NA> FBbt_00111013
#> 7     MBON17-like         <NA>          <NA>
#> 8          MBON02         <NA> FBbt_00111012
#> 9          MBON33         <NA> FBbt_00049853
#> 10         MBON29         <NA> FBbt_00049849
#> 11         MBON10         <NA> FBbt_00111008
#> 12         MBON14         <NA> FBbt_00100238
#> 13         MBON12         <NA> FBbt_00111009
#> 14    MBON15-like         <NA>          <NA>
#> 15         MBON31         <NA> FBbt_00049851
#> 16         MBON14         <NA> FBbt_00100238
#> 17         MBON27         <NA> FBbt_00049847
#> 18         MBON12         <NA> FBbt_00111009
#> 19         MBON03         <NA> FBbt_00100232
#> 20    MBON15-like         <NA>          <NA>
#> 21  MBON25,MBON34         <NA>          <NA>
#> 22         MBON10         <NA> FBbt_00111008
#> 23         MBON15         <NA> FBbt_00111010
#> 24         MBON10         <NA> FBbt_00111008
#> 25         MBON10         <NA> FBbt_00111008
#> 26         MBON15         <NA> FBbt_00111010
#> 27         MBON11         <NA> FBbt_00100246
#> 28         MBON17         <NA> FBbt_00111064
#> 29  MBON25,MBON34         <NA>          <NA>
#> 30         MBON19         <NA> FBbt_00111011
#> 31         MBON19         <NA> FBbt_00111011
#> 32         MBON16         <NA> FBbt_00111063
#> 33         MBON24         <NA> FBbt_00049844
#> 34         MBON28         <NA> FBbt_00049848
#> 35         MBON21         <NA> FBbt_00111046
#> 36         MBON23         <NA> FBbt_00049113
#> 37         MBON22         <NA> FBbt_00100240
#> 38         MBON13         <NA> FBbt_00100239
#> 39         MBON35         <NA> FBbt_00049855
#> 40         MBON07         <NA> FBbt_00111005
#> 41         MBON18         <NA> FBbt_00110101
#> 42         MBON04         <NA> FBbt_00111014
#> 43         MBON09         <NA> FBbt_00111007
#> 44         MBON26         <NA> FBbt_00049846
#> 45         MBON07         <NA> FBbt_00111005
#> 46         MBON09         <NA> FBbt_00111007
#> 47         MBON06         <NA> FBbt_00100242
#> 48  MBON25,MBON34         <NA>          <NA>

# anything with type *OR* class information
cells=flytable_cell_types(target = 'all')
# anything that mentions PN anywhere
pncands=flytable_cell_types('%PN%', target = 'all')
# }
```
