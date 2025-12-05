# Fetch flytable cell type information to a dataframe with flywire ids

`add_celltype_info` will add information to an existing dataframe.

`flytable_meta` will fetch a data.frame of metadata from flytable for a
given set of identifiers.

## Usage

``` r
add_celltype_info(
  x,
  idcol = NULL,
  version = NULL,
  suffix = NULL,
  table = c("both", "info", "optic"),
  ...
)

flytable_meta(
  ids = NULL,
  version = NULL,
  table = c("both", "info", "optic"),
  unique = FALSE,
  ...
)
```

## Arguments

- x:

  a data.frame containing root ids or a
  [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) ()

- idcol:

  Optional character vector specifying the column containing ids of the
  neurons for which cell type information should be provided.

- version:

  Optional numeric CAVE version (see `flywire_cave_query`). The special
  signalling value of `TRUE` uses the current default data dump as
  returned by
  [`flywire_connectome_data_version`](https://natverse.org/fafbseg/reference/flywire_connectome_data.md).

- suffix:

  A character suffix for the new columns (default value of `NULL`
  implies no suffix).

- table:

  Which cell type information tables to use (`info` for brain, `optic`
  for optic lobes or `both`).

- ...:

  additional arguments passed to `flytable_cell_types`

- ids:

  Flywire identifiers/query in any form understood by
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md)

- unique:

  Whether to ensure that rows contain only unique identifiers. Default
  `FALSE`. When `TRUE` duplicate rows will be returned with a warning.

## Value

a data.frame with extra columns

## Details

the root ids must be in a column called one of
`"pre_id", "post_id", "root_id", "post_pt_root_id", "pre_pt_root_id"`.
If you do not have exactly one of these columns present then you must
specify your preferred column with the `idcol` argument.

## See also

[`flytable_cell_types`](https://natverse.org/fafbseg/reference/flytable_cell_types.md)

## Examples

``` r
# \donttest{
kcin=flywire_partner_summary("720575940626474889", partners = 'in',
  cleft.threshold = 50)
kcin
#> # A tibble: 103 × 3
#>    query              pre_id             weight
#>    <chr>              <chr>               <int>
#>  1 720575940626474889 720575940640891763     29
#>  2 720575940626474889 720575940613583001     28
#>  3 720575940626474889 720575940632698797     26
#>  4 720575940626474889 720575940615834258     25
#>  5 720575940626474889 720575940607687260     19
#>  6 720575940626474889 720575940642086389     14
#>  7 720575940626474889 720575940622673703      8
#>  8 720575940626474889 720575940620709681      6
#>  9 720575940626474889 720575940621178206      6
#> 10 720575940626474889 720575940619665023      5
#> # ℹ 93 more rows
kcin2=add_celltype_info(kcin)
kcin2
#> # A tibble: 103 × 15
#>    query          pre_id weight supervoxel_id side  flow  super_class cell_class
#>    <chr>          <chr>   <int> <chr>         <chr> <chr> <chr>       <chr>     
#>  1 7205759406264… 72057…     29 807870292352… right intr… central     ALPN      
#>  2 7205759406264… 72057…     28 813486739212… right intr… central     MBIN      
#>  3 7205759406264… 72057…     26 803649542764… right intr… central     ALPN      
#>  4 7205759406264… 72057…     25 802240106295… right intr… central     ALPN      
#>  5 7205759406264… 72057…     19 802240792818… right intr… central     ALPN      
#>  6 7205759406264… 72057…     14 804353917399… right intr… central     ALPN      
#>  7 7205759406264… 72057…      8 816302178184… right intr… central     Kenyon_Ce…
#>  8 7205759406264… 72057…      6 813494986893… right intr… central     NA        
#>  9 7205759406264… 72057…      6 812080052867… right intr… central     Kenyon_Ce…
#> 10 7205759406264… 72057…      5 814192490249… right intr… central     Kenyon_Ce…
#> # ℹ 93 more rows
#> # ℹ 7 more variables: cell_sub_class <chr>, cell_type <chr>, top_nt <chr>,
#> #   ito_lee_hemilineage <chr>, hemibrain_type <chr>, malecns_type <chr>,
#> #   fbbt_id <chr>
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:nat’:
#> 
#>     intersect, setdiff, union
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
kcin2 %>%
  group_by(cell_type) %>%
  summarise(wt = sum(weight),n=n()) %>%
  arrange(desc(wt))
#> # A tibble: 20 × 3
#>    cell_type    wt     n
#>    <chr>     <int> <int>
#>  1 KCab        144    78
#>  2 VA1d_adPN    29     1
#>  3 APL          28     1
#>  4 DM2_lPN      26     2
#>  5 VM6_adPN     26     1
#>  6 VM2_adPN     19     1
#>  7 VA4_lPN      14     1
#>  8 LHPV12a1      9     2
#>  9 PAM11         4     2
#> 10 DPM           3     1
#> 11 PPL202        3     1
#> 12 PAM04         2     2
#> 13 PAM10         2     2
#> 14 PPL105        2     2
#> 15 KCapbp-m      1     1
#> 16 MBON02        1     1
#> 17 MBON06        1     1
#> 18 MBON07        1     1
#> 19 PPL106        1     1
#> 20 NA            1     1
kcin2 %>%
  count(cell_class, wt = weight)
#> # A tibble: 6 × 2
#>   cell_class      n
#>   <chr>       <int>
#> 1 ALPN          114
#> 2 DAN            14
#> 3 Kenyon_Cell   145
#> 4 MBIN           31
#> 5 MBON            3
#> 6 NA             10
# }

if (FALSE) { # \dontrun{
# read neuronlist containing "dotprops" for some olfactory projection neurons
da2=read_l2dps('DA2')
# add cell type details to that
da2=add_celltype_info(da2)
} # }
# \donttest{
flytable_meta("class:MBON")
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
flytable_meta("type:MBON2%")
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940611344078 80223254782485510 right intrinsic     central       MBON
#> 2  720575940617567206 78180912067538056  left intrinsic     central       MBON
#> 3  720575940643700640 80152611093049624 right intrinsic     central       MBON
#> 4  720575940614595218 78040449457502610  left intrinsic     central       MBON
#> 5  720575940618249797 80082723452177136 right intrinsic     central       MBON
#> 6  720575940624659943 80011255129536552  left intrinsic     central       MBON
#> 7  720575940627144069 79166692827138268 right intrinsic     central       MBON
#> 8  720575940619810389 79518055445188833 right intrinsic     central       MBON
#> 9  720575940623464316 81136880426764424 right intrinsic     central       MBON
#> 10 720575940622979277 79027192356412856  left intrinsic     central       MBON
#> 11 720575940614892182 81277617781167541 right intrinsic     central       MBON
#> 12 720575940615756690 78111917579300796  left intrinsic     central       MBON
#> 13 720575940613090673 78324741798855874  left intrinsic     central       MBON
#> 14 720575940621777391 81137979736901319 right intrinsic     central       MBON
#> 15 720575940606953858 81136880426735755 right intrinsic     central       MBON
#> 16 720575940635063135 80082242348953693 right intrinsic     central       MBON
#> 17 720575940616463477 77830305364995511  left intrinsic     central       MBON
#> 18 720575940629981440 78885355625276424  left intrinsic     central       MBON
#> 19 720575940614392999 79237542540872430  left intrinsic     central       MBON
#> 20 720575940607155890 80433124332533007 right intrinsic     central       MBON
#> 21 720575940631177803 78814711734490195  left intrinsic     central       MBON
#> 22 720575940622306625 79166761480079751 right intrinsic     central       MBON
#> 23 720575940616398303 79729161744437991  left intrinsic     central       MBON
#> 24 720575940613815210 79729161744426207  left intrinsic     central       MBON
#>    cell_sub_class     cell_type        top_nt ito_lee_hemilineage
#> 1            <NA>        MBON20          gaba    putative_primary
#> 2            <NA>        MBON23 acetylcholine          DL1_dorsal
#> 3            <NA>        MBON29 acetylcholine         DL2_ventral
#> 4            <NA>        MBON28 acetylcholine          DL1_dorsal
#> 5            <NA>        MBON27 acetylcholine    putative_primary
#> 6            <NA> MBON25,MBON34     glutamate              SMPad3
#> 7            <NA> MBON25,MBON34     glutamate              SMPad3
#> 8            <NA> MBON25,MBON34     glutamate              SMPad3
#> 9            <NA>        MBON24 acetylcholine    putative_primary
#> 10           <NA>        MBON27 acetylcholine    putative_primary
#> 11           <NA>        MBON28 acetylcholine          DL1_dorsal
#> 12           <NA>        MBON21 acetylcholine    putative_primary
#> 13           <NA>        MBON20          gaba    putative_primary
#> 14           <NA>        MBON21 acetylcholine    putative_primary
#> 15           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 16           <NA>        MBON22 acetylcholine    putative_primary
#> 17           <NA>        MBON22 acetylcholine    putative_primary
#> 18           <NA>        MBON26 acetylcholine    putative_primary
#> 19           <NA>        MBON29 acetylcholine         DL2_ventral
#> 20           <NA>        MBON26 acetylcholine    putative_primary
#> 21           <NA>        MBON24 acetylcholine    putative_primary
#> 22           <NA> MBON25,MBON34     glutamate              SMPad3
#> 23           <NA> MBON25,MBON34     glutamate              SMPad3
#> 24           <NA> MBON25,MBON34     glutamate              SMPad3
#>    hemibrain_type malecns_type       fbbt_id
#> 1          MBON20         <NA> FBbt_00111013
#> 2          MBON23         <NA> FBbt_00049113
#> 3          MBON29         <NA> FBbt_00049849
#> 4          MBON28         <NA> FBbt_00049848
#> 5          MBON27         <NA> FBbt_00049847
#> 6   MBON25,MBON34         <NA>          <NA>
#> 7   MBON25,MBON34         <NA>          <NA>
#> 8   MBON25,MBON34         <NA>          <NA>
#> 9          MBON24         <NA> FBbt_00049844
#> 10         MBON27         <NA> FBbt_00049847
#> 11         MBON28         <NA> FBbt_00049848
#> 12         MBON21         <NA> FBbt_00111046
#> 13         MBON20         <NA> FBbt_00111013
#> 14         MBON21         <NA> FBbt_00111046
#> 15         MBON23         <NA> FBbt_00049113
#> 16         MBON22         <NA> FBbt_00100240
#> 17         MBON22         <NA> FBbt_00100240
#> 18         MBON26         <NA> FBbt_00049846
#> 19         MBON29         <NA> FBbt_00049849
#> 20         MBON26         <NA> FBbt_00049846
#> 21         MBON24         <NA> FBbt_00049844
#> 22  MBON25,MBON34         <NA>          <NA>
#> 23  MBON25,MBON34         <NA>          <NA>
#> 24  MBON25,MBON34         <NA>          <NA>
# the / introduces a regex query (small performance penalty, more flexible)
flytable_meta("/type:MBON2[0-5]")
#>               root_id     supervoxel_id  side      flow super_class cell_class
#> 1  720575940611344078 80223254782485510 right intrinsic     central       MBON
#> 2  720575940617567206 78180912067538056  left intrinsic     central       MBON
#> 3  720575940624659943 80011255129536552  left intrinsic     central       MBON
#> 4  720575940627144069 79166692827138268 right intrinsic     central       MBON
#> 5  720575940619810389 79518055445188833 right intrinsic     central       MBON
#> 6  720575940623464316 81136880426764424 right intrinsic     central       MBON
#> 7  720575940615756690 78111917579300796  left intrinsic     central       MBON
#> 8  720575940613090673 78324741798855874  left intrinsic     central       MBON
#> 9  720575940621777391 81137979736901319 right intrinsic     central       MBON
#> 10 720575940606953858 81136880426735755 right intrinsic     central       MBON
#> 11 720575940635063135 80082242348953693 right intrinsic     central       MBON
#> 12 720575940616463477 77830305364995511  left intrinsic     central       MBON
#> 13 720575940631177803 78814711734490195  left intrinsic     central       MBON
#> 14 720575940622306625 79166761480079751 right intrinsic     central       MBON
#> 15 720575940616398303 79729161744437991  left intrinsic     central       MBON
#> 16 720575940613815210 79729161744426207  left intrinsic     central       MBON
#>    cell_sub_class     cell_type        top_nt ito_lee_hemilineage
#> 1            <NA>        MBON20          gaba    putative_primary
#> 2            <NA>        MBON23 acetylcholine          DL1_dorsal
#> 3            <NA> MBON25,MBON34     glutamate              SMPad3
#> 4            <NA> MBON25,MBON34     glutamate              SMPad3
#> 5            <NA> MBON25,MBON34     glutamate              SMPad3
#> 6            <NA>        MBON24 acetylcholine    putative_primary
#> 7            <NA>        MBON21 acetylcholine    putative_primary
#> 8            <NA>        MBON20          gaba    putative_primary
#> 9            <NA>        MBON21 acetylcholine    putative_primary
#> 10           <NA>        MBON23 acetylcholine          DL1_dorsal
#> 11           <NA>        MBON22 acetylcholine    putative_primary
#> 12           <NA>        MBON22 acetylcholine    putative_primary
#> 13           <NA>        MBON24 acetylcholine    putative_primary
#> 14           <NA> MBON25,MBON34     glutamate              SMPad3
#> 15           <NA> MBON25,MBON34     glutamate              SMPad3
#> 16           <NA> MBON25,MBON34     glutamate              SMPad3
#>    hemibrain_type malecns_type       fbbt_id
#> 1          MBON20         <NA> FBbt_00111013
#> 2          MBON23         <NA> FBbt_00049113
#> 3   MBON25,MBON34         <NA>          <NA>
#> 4   MBON25,MBON34         <NA>          <NA>
#> 5   MBON25,MBON34         <NA>          <NA>
#> 6          MBON24         <NA> FBbt_00049844
#> 7          MBON21         <NA> FBbt_00111046
#> 8          MBON20         <NA> FBbt_00111013
#> 9          MBON21         <NA> FBbt_00111046
#> 10         MBON23         <NA> FBbt_00049113
#> 11         MBON22         <NA> FBbt_00100240
#> 12         MBON22         <NA> FBbt_00100240
#> 13         MBON24         <NA> FBbt_00049844
#> 14  MBON25,MBON34         <NA>          <NA>
#> 15  MBON25,MBON34         <NA>          <NA>
#> 16  MBON25,MBON34         <NA>          <NA>
# }
```
