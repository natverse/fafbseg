# Flytable database queries

`flytable_query` performs a SQL query against a flytable database. You
can omit the `base` argument unless you have tables of the same name in
different bases.

## Usage

``` r
flytable_list_rows(
  table,
  base = NULL,
  view_name = NULL,
  order_by = NULL,
  desc = FALSE,
  start = 0L,
  limit = Inf,
  collapse_lists = TRUE,
  python = FALSE,
  chunksize = NULL
)

flytable_query(
  sql,
  limit = 100000L,
  base = NULL,
  python = FALSE,
  convert = TRUE,
  collapse_lists = TRUE,
  paginate = TRUE,
  chunksize = NULL
)
```

## Arguments

- table:

  The name of a table inside your database

- base:

  Character vector naming a seatable base (recommended) or a `Base`
  object returned by `flytable_base` (expert use).

- view_name:

  An optional view which may limit the rows/columns displayed.

- order_by:

  Optional name of columns to order results

- desc:

  Whether to use descending order (default `FALSE` =\> ascending order)

- start:

  Optional starting row

- limit:

  An optional limit on the total number of rows returned, which only
  applies if you do not specify a limit directly in the `sql` query. By
  default seatable limits SQL queries to 100 rows. We increase the limit
  to 100000 rows by default. See `paginate` for how this interacts with
  the server's per-call row cap.

- collapse_lists:

  Whether to collapse any list multi-select columns into simple strings.
  The default value of `collapse_lists=TRUE` will comma separate them.

- python:

  Whether to return a Python pandas `DataFrame`. The default of `FALSE`
  returns an R `data.frame`

- chunksize:

  Optional maximum number of rows to request per web request. For
  advanced use only; the default `NULL` fetches as many rows per call as
  the server allows. For `flytable_query` a non-`NULL` value forces
  `LIMIT`/`OFFSET` pagination in windows of this size (mainly useful for
  exercising the paging path against a server whose own row cap is too
  high to reach with a modest table).

- sql:

  A SQL query string. See examples and [seatable
  docs](https://seatable.github.io/seatable-scripts/python/query/).

- convert:

  Expert use only: Whether or not to allow the Python seatable module to
  process raw output from the database. This is is principally for
  debugging purposes. NB this imposes a requirement of seatable_api
  \>=2.4.0.

- paginate:

  Whether to transparently page through large results with
  `LIMIT`/`OFFSET` (default `TRUE`). Seatable's SQL endpoint silently
  caps a single call at a server-specific maximum (documented default
  10,000 rows for SELECT queries,
  <https://api.seatable.com/reference/limits>; self-hosted servers may
  allow more) with no truncation warning, so without pagination a query
  matching more rows than the cap would silently lose rows. Pagination
  is skipped automatically when you supply your own `limit`/`offset` in
  the `sql`, when `python=TRUE`, or when the first page already returns
  fewer rows than the guaranteed cap.

## Value

An R `data.frame` or Pandas `DataFrame` depending on the value of the
`python` argument.

a `data.frame` of results. There should be 0 rows if no rows matched
query.

## Details

Flytable uses programmatic access to the
[seatable](https://seatable.github.io/seatable-scripts/) API.

## See also

[`tabify_coords`](https://natverse.org/fafbseg/reference/tabify_coords.md)
to help with copy-pasting coordinates to seatable.

Other flytable:
[`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md),
[`flytable_select_options()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)

## Examples

``` r
# \donttest{
flytable_list_rows(table = "testfruit")
#>                        _id              _mtime              _ctime  fruit_name
#> 1   H8BckTnXRL2PaVuLDcRPMA 2026-08-04 14:22:21 2021-12-17 13:36:17       apple
#> 2   SrBW3vFLRxafKRYHoPrGQQ 2022-05-12 16:58:08 2021-12-17 13:36:17      banana
#> 3   F1h-TZKpTcWiHujZ2xnF8Q 2024-08-28 01:30:00 2021-12-17 13:36:17  clementine
#> 4   dwTLFmsDRoCxhE3BwY-ovQ 2026-08-20 12:49:57 2024-08-28 01:30:00        kiwi
#> 5   Im6VZG_DQ7mRJh6BAUwW7A 2024-08-28 01:31:13 2024-08-28 01:31:13        kiwi
#> 6   YsZ-5JndSpW96JW9cZZ8dw 2026-08-04 14:11:02 2026-05-16 16:31:01        kiwi
#> 7   eQVD8GmEQmq11XxfggkoVA 2026-08-04 13:58:58 2026-05-16 16:31:05        kiwi
#> 8   AtYZUS-EQT2NGEpMBco_iw 2026-08-04 13:59:02 2026-05-16 16:35:36        kiwi
#> 9   FWYTJZgDQyioqcRnVvM4TA 2026-08-04 02:40:41 2026-08-04 02:40:41        kiwi
#> 10  NpVTQf72REeeDYiQHhD7bA 2026-08-04 02:43:29 2026-08-04 02:43:29        kiwi
#> 11  cMs7rchlTOO6ArPnVtIbwQ 2026-08-04 07:33:15 2026-08-04 07:33:15        kiwi
#> 12  NkDj38vMR5yHz2yNvJWQug 2026-08-04 07:34:16 2026-08-04 07:34:16        kiwi
#> 13  YMZSVgaMS3KdvoVJ15gkaQ 2026-08-04 07:34:32 2026-08-04 07:34:32        kiwi
#> 14  NE2aRAPpQqyaSFau39x-NQ 2026-08-04 15:25:33 2026-08-04 15:25:33        kiwi
#> 15  Ugt6lOQ4SPGSIRN3KYjZaQ 2026-08-04 15:35:21 2026-08-04 15:35:21        kiwi
#> 16  JCkxWltZTQ-YpYQOumajvQ 2026-08-04 16:20:32 2026-08-04 16:20:32        kiwi
#> 17  OimH9KKhSUWbRSDjw4f3Sg 2026-08-05 11:38:09 2026-08-05 11:38:09        kiwi
#> 18  Z6a6aS1pSfqxNor6mYy_2A 2026-08-05 22:09:33 2026-08-05 22:09:33        kiwi
#> 19  YtGyNFbeQLWYmS8uOiyaQA 2026-08-05 22:28:04 2026-08-05 22:28:04        kiwi
#> 20  Y-7LDG-3QtePwU-3lQIJew 2026-08-05 22:37:32 2026-08-05 22:37:32        kiwi
#> 21  IGCLe4OyQ8SEbX8W4dNnXQ 2026-08-05 22:53:32 2026-08-05 22:53:32        kiwi
#> 22  L5_EZ_8SQIiEiwGmiUN68A 2026-08-06 01:35:28 2026-08-06 01:35:28        kiwi
#> 23  WOz9dZSYSxqJ2PASy1L9vQ 2026-08-06 01:57:27 2026-08-06 01:57:27        kiwi
#> 24  KHa1G7FUQQqbspZyQfqPBw 2026-08-06 02:00:20 2026-08-06 02:00:20        kiwi
#> 25  M7fnq2OVQXK1w5HJfXiSAQ 2026-08-06 02:01:24 2026-08-06 02:01:24        kiwi
#> 26  EHyFiZJfQ9is_kZWETSO4Q 2026-08-06 02:04:20 2026-08-06 02:04:20        kiwi
#> 27  EHl71b0ZTMKgzk3068dQrw 2026-08-06 02:29:36 2026-08-06 02:29:36        kiwi
#> 28  Bx-VOyOxTxazlZZ7s0_CdQ 2026-08-06 02:30:45 2026-08-06 02:30:45        kiwi
#> 29  A99gi51PQ7-ceAnmF7LMPw 2026-08-06 02:37:03 2026-08-06 02:37:03        kiwi
#> 30  HmI5aEbtT6ayx6xH4VnOaw 2026-08-06 07:27:33 2026-08-06 07:27:33 dragonfruit
#> 31  Hi9ONOFDTTKp_Yl-aSctgw 2026-08-06 07:27:36 2026-08-06 07:27:34   starfruit
#> 32  eUB140NYS4GkF7DPSx1thw 2026-08-06 07:27:59 2026-08-06 07:27:59 dragonfruit
#> 33  FzJthRgqT8Wfi6dKux8UsQ 2026-08-06 07:28:02 2026-08-06 07:28:00   starfruit
#> 34  dPWqcY8NSXaxXvlKrm2eeg 2026-08-06 07:30:41 2026-08-06 07:30:41        kiwi
#> 35  ZZJWrJGrRme2W45B_B-_uA 2026-08-06 07:32:14 2026-08-06 07:32:14        kiwi
#> 36  DeaPuxpDSAadYNV5gPM8Hw 2026-08-06 08:08:31 2026-08-06 08:08:31        kiwi
#> 37  I1GuWY5cT4-PP7jB-FTBiw 2026-08-06 08:10:00 2026-08-06 08:10:00        kiwi
#> 38  bJN3l4UwS1KEXeHpC7w5_g 2026-08-06 08:10:54 2026-08-06 08:10:54        kiwi
#> 39  WYB5jGVnSymeVx8oGKeA_g 2026-08-06 08:12:18 2026-08-06 08:12:18        kiwi
#> 40  Yxb_aAdETfG5jVWB7tE9xg 2026-08-06 22:45:59 2026-08-06 22:45:59        kiwi
#> 41  a5U8meTJSNaitX85ZYaSwQ 2026-08-06 23:15:23 2026-08-06 23:15:23        kiwi
#> 42  CVuZJLScQL-huoErGQQUSg 2026-08-06 23:18:44 2026-08-06 23:18:44        kiwi
#> 43  S63Uerd9QbyZaZRdKTen9g 2026-08-06 23:21:04 2026-08-06 23:21:04        kiwi
#> 44  aFFQR0MHT7SwBzHVOg7n_w 2026-08-07 03:32:39 2026-08-07 03:32:39        kiwi
#> 45  X6re_fP1QiCToV7y6omezg 2026-08-07 03:34:57 2026-08-07 03:34:57        kiwi
#> 46  L_cTHvP0S6653S6y40u3Dg 2026-08-07 03:35:43 2026-08-07 03:35:43        kiwi
#> 47  Syyip8pSSPKecbf5M3v2hg 2026-08-07 05:03:49 2026-08-07 05:03:49        kiwi
#> 48  ZdCzpd4VSzylY_0J53839g 2026-08-07 05:06:17 2026-08-07 05:06:17        kiwi
#> 49  N8D0-hAIRuueA9YBzywQSw 2026-08-07 05:08:35 2026-08-07 05:08:35        kiwi
#> 50  cb8aKpHZS5aSR6-y0sUWzw 2026-08-07 11:36:13 2026-08-07 11:36:13        kiwi
#> 51  D90iNFwXRk-9vwjhaUIr2w 2026-08-07 11:38:48 2026-08-07 11:38:48        kiwi
#> 52  A-41sdysQ2iJKzCY4HLQGw 2026-08-07 11:39:09 2026-08-07 11:39:09        kiwi
#> 53  d6D46fD8SUyMzkWaOB4Pyg 2026-08-07 11:54:14 2026-08-07 11:54:14        kiwi
#> 54  XBSX9jgBQUaYnOm_3k0KRQ 2026-08-07 11:55:24 2026-08-07 11:55:24        kiwi
#> 55  a52x6PXEQhO0wa4z9NG5Cw 2026-08-07 11:58:13 2026-08-07 11:58:13        kiwi
#> 56  LXBPookCSDiKWfzH1RdEBg 2026-08-07 12:14:56 2026-08-07 12:14:56        kiwi
#> 57  N2ioPI_sRtqZx4zhoRejCQ 2026-08-07 12:15:23 2026-08-07 12:15:23        kiwi
#> 58  e9iv1vblSjy_T4g89JiClQ 2026-08-07 14:04:43 2026-08-07 14:04:43        kiwi
#> 59  R8nPWihLSy637IcoYdJGwg 2026-08-07 14:07:05 2026-08-07 14:07:05        kiwi
#> 60  ZAzvu5mHRoCCI3c-P2mIAA 2026-08-07 14:07:54 2026-08-07 14:07:54        kiwi
#> 61  Tp8Vd_RURwqhnQhcvsILuQ 2026-08-07 14:42:02 2026-08-07 14:42:02        kiwi
#> 62  DmsSRdDnSgmcpg4tsqYJjA 2026-08-07 20:27:01 2026-08-07 20:27:01        kiwi
#> 63  BGi5AbAQRoSFndz5go74BA 2026-08-07 21:58:08 2026-08-07 21:58:08        kiwi
#> 64  LJTAvYEHScWNLAJcXUCBdw 2026-08-07 22:06:39 2026-08-07 22:06:39        kiwi
#> 65  LQ0qBNzzSHGLa6qV3Mt0pQ 2026-08-07 22:51:15 2026-08-07 22:51:15        kiwi
#> 66  SQrFblisQyCOqUIH278nTg 2026-08-07 22:52:56 2026-08-07 22:52:56        kiwi
#> 67  Ra1Tjbo_TFi0kCnpX0AJFQ 2026-08-07 23:23:03 2026-08-07 23:23:03        kiwi
#> 68  ay6IgCvuS_6I4SOesP35TQ 2026-08-07 23:23:50 2026-08-07 23:23:50        kiwi
#> 69  dh2mB49yRDar6-4RNYzSUg 2026-08-07 23:24:30 2026-08-07 23:24:30        kiwi
#> 70  TQiOM8TATbyXsgQN70M8Yw 2026-08-07 23:25:56 2026-08-07 23:25:56        kiwi
#> 71  MHptLL67TMeSyYCAsWGsgg 2026-08-07 23:26:23 2026-08-07 23:26:23        kiwi
#> 72  Cg66ZQSnRaqrIoXRf0YDWQ 2026-08-08 06:50:34 2026-08-08 06:50:34        kiwi
#> 73  djjnrPEdRZiPoKZnS2bY3w 2026-08-08 06:50:52 2026-08-08 06:50:52        kiwi
#> 74  QV1HOpIvRGiv17HvoGJqkg 2026-08-08 06:52:01 2026-08-08 06:52:01        kiwi
#> 75  U6EDM4MpQDO6_v4qHLRRtw 2026-08-08 09:37:15 2026-08-08 09:37:15        kiwi
#> 76  Rq2jfKU7TmihSbagmaNSPg 2026-08-08 09:39:52 2026-08-08 09:39:52        kiwi
#> 77  FHIEFcKXQeG_iCPm7YDxJw 2026-08-08 09:41:57 2026-08-08 09:41:57        kiwi
#> 78  YneJVZt9R9SGb2L6sAmUew 2026-08-08 09:46:10 2026-08-08 09:46:10        kiwi
#> 79  P7WaINE7RYmZOiv1jTDGDQ 2026-08-08 09:46:25 2026-08-08 09:46:25        kiwi
#> 80  LBkzCzGWSR-NDg0GYuUaZg 2026-08-08 09:51:20 2026-08-08 09:51:20        kiwi
#> 81  epzNE-ybQC6FLrP_kv1QHg 2026-08-08 09:51:40 2026-08-08 09:51:40        kiwi
#> 82  OsfHOEqFQ2yj_vqDxMIzsQ 2026-08-08 09:53:13 2026-08-08 09:53:13        kiwi
#> 83  ZZZtnJRqTSiA-K2a3CXa0w 2026-08-08 10:16:44 2026-08-08 10:16:44        kiwi
#> 84  IramS9jgQ66ZDzfe1EPmyg 2026-08-08 10:19:11 2026-08-08 10:19:11        kiwi
#> 85  eVuNlNqVSsK-nELMVjKh_Q 2026-08-08 10:20:57 2026-08-08 10:20:57        kiwi
#> 86  blCsMfbTTdSHuflmMMOvOg 2026-08-08 11:26:17 2026-08-08 11:26:17        kiwi
#> 87  LcXFYpHqR7GxBpTUgXTtCA 2026-08-08 11:26:19 2026-08-08 11:26:19        kiwi
#> 88  M4Nv9hxhQPSjYO7ESnMLTA 2026-08-08 11:26:37 2026-08-08 11:26:37        kiwi
#> 89  cdB77XIlTEihjw1vS92yOg 2026-08-08 11:28:04 2026-08-08 11:28:04        kiwi
#> 90  Bhw50Jx7TG-Bf2VH1vNdFg 2026-08-08 11:28:28 2026-08-08 11:28:28        kiwi
#> 91  UCdvH2sTSA22w0Q3hG615g 2026-08-08 11:28:57 2026-08-08 11:28:57        kiwi
#> 92  Dn77YkJTTGWzrULuI1o57w 2026-08-09 06:51:35 2026-08-09 06:51:35        kiwi
#> 93  VIMPp5hwRD2nI11mmdhsQg 2026-08-09 06:53:25 2026-08-09 06:53:25        kiwi
#> 94  INvNR1iUR4yjqJq4gaBEEw 2026-08-09 06:54:37 2026-08-09 06:54:37        kiwi
#> 95  dWyplK0EQwyKA7AgXh7IIw 2026-08-09 07:53:48 2026-08-09 07:53:48        kiwi
#> 96  HLYdTtU-S6q7yOUCGlhgcQ 2026-08-09 07:54:19 2026-08-09 07:54:19        kiwi
#> 97  Kb1Hhq_rTvORNCskaDtQEQ 2026-08-09 07:55:59 2026-08-09 07:55:59        kiwi
#> 98  DE6Ndi_HRHmLL4tcjsyhyw 2026-08-09 07:57:27 2026-08-09 07:57:27        kiwi
#> 99  IczKt5moQUyox7RL6XZEvg 2026-08-09 09:25:53 2026-08-09 09:25:53        kiwi
#> 100 a-4ZYCESSoCSB5edT_hG_Q 2026-08-09 09:28:51 2026-08-09 09:28:51        kiwi
#> 101 SwnSt9amQ1a50o0kvWhBpQ 2026-08-09 09:30:48 2026-08-09 09:30:48        kiwi
#> 102 UJl6ikARR-ems8Gq3J4pkg 2026-08-12 00:17:57 2026-08-12 00:17:57        kiwi
#> 103 AXd5hUP4RTCojXc-NZY_nQ 2026-08-12 00:20:44 2026-08-12 00:20:44        kiwi
#> 104 FTSSztBBT3-fthA6swOGcg 2026-08-12 00:23:01 2026-08-12 00:23:01        kiwi
#> 105 e1YXbSYKSz-9FwqxfPS3-A 2026-08-12 03:18:24 2026-08-12 03:18:24        kiwi
#> 106 O5svPgvKQs2DCK_sVvTUFA 2026-08-12 03:21:28 2026-08-12 03:21:28        kiwi
#> 107 eflndrlJQhGRP_CcbhgIyg 2026-08-12 03:21:43 2026-08-12 03:21:43        kiwi
#> 108 ZcL6n3hpSpiZMYqk9Iy0dg 2026-08-12 22:10:06 2026-08-12 22:10:06        kiwi
#> 109 ZYnTMQBvSKmY4aokM4EVKg 2026-08-12 22:10:32 2026-08-12 22:10:32        kiwi
#> 110 AVWJglBxQaq-RR5ARdlKGw 2026-08-12 22:29:12 2026-08-12 22:29:12        kiwi
#> 111 AG9mkBv-Qd2zk3Ofg8hmdg 2026-08-12 22:30:40 2026-08-12 22:30:40        kiwi
#> 112 eb3KPkfBThefAdtYFhTJOw 2026-08-12 22:33:04 2026-08-12 22:33:04        kiwi
#> 113 Tt2mosn5T3ydJCPeMg0LPA 2026-08-16 18:46:24 2026-08-16 18:46:24        kiwi
#> 114 ekyaJeuJTiKyslBmPaTTFQ 2026-08-16 18:48:43 2026-08-16 18:48:43        kiwi
#> 115 I11hMGUiRFSTU5kpFsvlKA 2026-08-16 18:48:58 2026-08-16 18:48:58        kiwi
#> 116 YyH3meb7Q9Cq73rqN2gsKw 2026-08-16 19:27:18 2026-08-16 19:27:18        kiwi
#> 117 a9WAiaOtSZe_zSKuk9CgdQ 2026-08-16 19:30:31 2026-08-16 19:30:31        kiwi
#> 118 C5MhzZVdRtSWsMDqbhqPLA 2026-08-16 19:30:53 2026-08-16 19:30:53        kiwi
#> 119 DTEbWvJfREe-pc-XJc4Kuw 2026-08-16 19:31:57 2026-08-16 19:31:57        kiwi
#> 120 X2BjlCg_TQirp6Bid7TmLw 2026-08-16 19:32:41 2026-08-16 19:32:41        kiwi
#> 121 NJLNzhTfRvC2cW6euVJK5w 2026-08-16 19:33:06 2026-08-16 19:33:06        kiwi
#> 122 Awz6P5dwQfuof5ExYO4cEg 2026-08-17 01:01:59 2026-08-17 01:01:59        kiwi
#> 123 FNsNvwGESsmI9AV9vm5mgA 2026-08-17 01:02:19 2026-08-17 01:02:19        kiwi
#> 124 bC7U01wlRG6nr2s1Qpz-jQ 2026-08-17 01:05:40 2026-08-17 01:05:40        kiwi
#> 125 bjRplvP-SvKcpxuRVU-fzQ 2026-08-20 02:11:50 2026-08-20 02:11:50        kiwi
#> 126 XWqTI7NmRiaIE2EtDCHCuw 2026-08-20 02:15:02 2026-08-20 02:15:02        kiwi
#> 127 L3CJj4lORzCMCwlbbVcQxg 2026-08-20 02:15:50 2026-08-20 02:15:50        kiwi
#> 128 aYnGqkVDTY6pweq98ASBLA 2026-08-20 02:56:50 2026-08-20 02:56:50        kiwi
#> 129 RJaBG6qPQI6b25b3c62zkg 2026-08-20 03:00:52 2026-08-20 03:00:52        kiwi
#> 130 EUEzAKtnSxiLLfhpgm0GhQ 2026-08-20 03:00:54 2026-08-20 03:00:54        kiwi
#> 131 d93rqahUSEqbFaVc9ccvYg 2026-08-20 12:45:42 2026-08-20 12:45:42        kiwi
#> 132 dqb2rx1YRhix_hNkdpzQcw 2026-08-20 12:47:47 2026-08-20 12:47:47        kiwi
#> 133 a9kEil17Swm7bx9MDeRK9w 2026-08-20 12:49:58 2026-08-20 12:49:58        kiwi
#>         nid              person       last_modified date_nominute
#> 1         1               Alice 2026-08-04 14:22:21    2022-01-06
#> 2         2                 Bob 2022-05-12 16:58:08    2022-01-03
#> 3         3               Clara 2024-08-28 01:30:00    2021-08-05
#> 4    976376 Frederick the Great 2026-08-20 12:49:57          <NA>
#> 5   7706772 Frederick the Great 2024-08-28 01:31:13          <NA>
#> 6   7592831 Frederick the Great 2026-08-04 14:11:02          <NA>
#> 7   8095327 Frederick the Great 2026-08-04 13:58:58          <NA>
#> 8   3462194 Frederick the Great 2026-08-04 13:59:02          <NA>
#> 9   2442444 Frederick the Great 2026-08-04 02:40:41          <NA>
#> 10  4079011 Frederick the Great 2026-08-04 02:43:29          <NA>
#> 11  3924912 Frederick the Great 2026-08-04 07:33:15          <NA>
#> 12  9139682 Frederick the Great 2026-08-04 07:34:16          <NA>
#> 13  5170183 Frederick the Great 2026-08-04 07:34:32          <NA>
#> 14  6151012 Frederick the Great 2026-08-04 15:25:33          <NA>
#> 15  4401168 Frederick the Great 2026-08-04 15:35:21          <NA>
#> 16  4361950 Frederick the Great 2026-08-04 16:20:32          <NA>
#> 17  5312251 Frederick the Great 2026-08-05 11:38:09          <NA>
#> 18  7868204 Frederick the Great 2026-08-05 22:09:33          <NA>
#> 19  1890153 Frederick the Great 2026-08-05 22:28:04          <NA>
#> 20  6994939 Frederick the Great 2026-08-05 22:37:32          <NA>
#> 21  7694898 Frederick the Great 2026-08-05 22:53:32          <NA>
#> 22  7255705 Frederick the Great 2026-08-06 01:35:28          <NA>
#> 23   329511 Frederick the Great 2026-08-06 01:57:27          <NA>
#> 24  9321031 Frederick the Great 2026-08-06 02:00:20          <NA>
#> 25  4387483 Frederick the Great 2026-08-06 02:01:24          <NA>
#> 26  8784907 Frederick the Great 2026-08-06 02:04:20          <NA>
#> 27  9478480 Frederick the Great 2026-08-06 02:29:36          <NA>
#> 28  9338701 Frederick the Great 2026-08-06 02:30:45          <NA>
#> 29  8411708 Frederick the Great 2026-08-06 02:37:03          <NA>
#> 30  1683741     Delta Sync Test 2026-08-06 07:27:33          <NA>
#> 31  2339781   Multi Select Test 2026-08-06 07:27:36          <NA>
#> 32  3508782     Delta Sync Test 2026-08-06 07:27:59          <NA>
#> 33  6158314   Multi Select Test 2026-08-06 07:28:02          <NA>
#> 34  5578437 Frederick the Great 2026-08-06 07:30:41          <NA>
#> 35  8801360 Frederick the Great 2026-08-06 07:32:14          <NA>
#> 36  7882694 Frederick the Great 2026-08-06 08:08:31          <NA>
#> 37  9472171 Frederick the Great 2026-08-06 08:10:00          <NA>
#> 38  1173603 Frederick the Great 2026-08-06 08:10:54          <NA>
#> 39  9855950 Frederick the Great 2026-08-06 08:12:18          <NA>
#> 40  4860898 Frederick the Great 2026-08-06 22:45:59          <NA>
#> 41  6449285 Frederick the Great 2026-08-06 23:15:23          <NA>
#> 42  3955252 Frederick the Great 2026-08-06 23:18:44          <NA>
#> 43  2255968 Frederick the Great 2026-08-06 23:21:04          <NA>
#> 44  9442450 Frederick the Great 2026-08-07 03:32:39          <NA>
#> 45  9014656 Frederick the Great 2026-08-07 03:34:57          <NA>
#> 46  7039635 Frederick the Great 2026-08-07 03:35:43          <NA>
#> 47  5103791 Frederick the Great 2026-08-07 05:03:49          <NA>
#> 48  3864773 Frederick the Great 2026-08-07 05:06:17          <NA>
#> 49  9911958 Frederick the Great 2026-08-07 05:08:35          <NA>
#> 50  1547744 Frederick the Great 2026-08-07 11:36:13          <NA>
#> 51  3256988 Frederick the Great 2026-08-07 11:38:48          <NA>
#> 52  5457118 Frederick the Great 2026-08-07 11:39:09          <NA>
#> 53  6517708 Frederick the Great 2026-08-07 11:54:14          <NA>
#> 54  6034946 Frederick the Great 2026-08-07 11:55:24          <NA>
#> 55  9981019 Frederick the Great 2026-08-07 11:58:13          <NA>
#> 56  6856049 Frederick the Great 2026-08-07 12:14:56          <NA>
#> 57  3784415 Frederick the Great 2026-08-07 12:15:23          <NA>
#> 58  2797287 Frederick the Great 2026-08-07 14:04:43          <NA>
#> 59  8525315 Frederick the Great 2026-08-07 14:07:05          <NA>
#> 60  7955848 Frederick the Great 2026-08-07 14:07:54          <NA>
#> 61  6591988 Frederick the Great 2026-08-07 14:42:02          <NA>
#> 62  4029646 Frederick the Great 2026-08-07 20:27:01          <NA>
#> 63  2061541 Frederick the Great 2026-08-07 21:58:08          <NA>
#> 64  8750468 Frederick the Great 2026-08-07 22:06:39          <NA>
#> 65  9815361 Frederick the Great 2026-08-07 22:51:15          <NA>
#> 66   296574 Frederick the Great 2026-08-07 22:52:56          <NA>
#> 67  3989259 Frederick the Great 2026-08-07 23:23:03          <NA>
#> 68  7450869 Frederick the Great 2026-08-07 23:23:50          <NA>
#> 69  2391563 Frederick the Great 2026-08-07 23:24:30          <NA>
#> 70  7501970 Frederick the Great 2026-08-07 23:25:56          <NA>
#> 71  4518061 Frederick the Great 2026-08-07 23:26:23          <NA>
#> 72  1837317 Frederick the Great 2026-08-08 06:50:34          <NA>
#> 73  1933166 Frederick the Great 2026-08-08 06:50:52          <NA>
#> 74  4640749 Frederick the Great 2026-08-08 06:52:01          <NA>
#> 75  8862210 Frederick the Great 2026-08-08 09:37:15          <NA>
#> 76  6155750 Frederick the Great 2026-08-08 09:39:52          <NA>
#> 77  7823333 Frederick the Great 2026-08-08 09:41:57          <NA>
#> 78  6161282 Frederick the Great 2026-08-08 09:46:10          <NA>
#> 79  1801522 Frederick the Great 2026-08-08 09:46:25          <NA>
#> 80  6612890 Frederick the Great 2026-08-08 09:51:20          <NA>
#> 81  9205598 Frederick the Great 2026-08-08 09:51:40          <NA>
#> 82  8846328 Frederick the Great 2026-08-08 09:53:13          <NA>
#> 83  7984923 Frederick the Great 2026-08-08 10:16:44          <NA>
#> 84  2316038 Frederick the Great 2026-08-08 10:19:11          <NA>
#> 85  1397803 Frederick the Great 2026-08-08 10:20:57          <NA>
#> 86  2557240 Frederick the Great 2026-08-08 11:26:17          <NA>
#> 87  3461755 Frederick the Great 2026-08-08 11:26:19          <NA>
#> 88  7901815 Frederick the Great 2026-08-08 11:26:37          <NA>
#> 89  9226856 Frederick the Great 2026-08-08 11:28:04          <NA>
#> 90  1127566 Frederick the Great 2026-08-08 11:28:28          <NA>
#> 91  9933744 Frederick the Great 2026-08-08 11:28:57          <NA>
#> 92  3748562 Frederick the Great 2026-08-09 06:51:35          <NA>
#> 93  5306125 Frederick the Great 2026-08-09 06:53:25          <NA>
#> 94   737448 Frederick the Great 2026-08-09 06:54:37          <NA>
#> 95  2730678 Frederick the Great 2026-08-09 07:53:48          <NA>
#> 96   201960 Frederick the Great 2026-08-09 07:54:19          <NA>
#> 97   715333 Frederick the Great 2026-08-09 07:55:59          <NA>
#> 98  8817349 Frederick the Great 2026-08-09 07:57:27          <NA>
#> 99  5021129 Frederick the Great 2026-08-09 09:25:53          <NA>
#> 100  719630 Frederick the Great 2026-08-09 09:28:51          <NA>
#> 101 7789592 Frederick the Great 2026-08-09 09:30:48          <NA>
#> 102 1457218 Frederick the Great 2026-08-12 00:17:57          <NA>
#> 103 4919975 Frederick the Great 2026-08-12 00:20:44          <NA>
#> 104 1967780 Frederick the Great 2026-08-12 00:23:01          <NA>
#> 105 1183637 Frederick the Great 2026-08-12 03:18:24          <NA>
#> 106 6495833 Frederick the Great 2026-08-12 03:21:28          <NA>
#> 107  722809 Frederick the Great 2026-08-12 03:21:43          <NA>
#> 108 7880248 Frederick the Great 2026-08-12 22:10:06          <NA>
#> 109 2995180 Frederick the Great 2026-08-12 22:10:32          <NA>
#> 110 3016649 Frederick the Great 2026-08-12 22:29:12          <NA>
#> 111 7490259 Frederick the Great 2026-08-12 22:30:40          <NA>
#> 112  324517 Frederick the Great 2026-08-12 22:33:04          <NA>
#> 113 4641095 Frederick the Great 2026-08-16 18:46:24          <NA>
#> 114 5582157 Frederick the Great 2026-08-16 18:48:43          <NA>
#> 115 6161229 Frederick the Great 2026-08-16 18:48:58          <NA>
#> 116   82827 Frederick the Great 2026-08-16 19:27:18          <NA>
#> 117 8087727 Frederick the Great 2026-08-16 19:30:31          <NA>
#> 118 2656949 Frederick the Great 2026-08-16 19:30:53          <NA>
#> 119 4140683 Frederick the Great 2026-08-16 19:31:57          <NA>
#> 120 5307077 Frederick the Great 2026-08-16 19:32:41          <NA>
#> 121 5457261 Frederick the Great 2026-08-16 19:33:06          <NA>
#> 122 4700550 Frederick the Great 2026-08-17 01:01:59          <NA>
#> 123 8975878 Frederick the Great 2026-08-17 01:02:19          <NA>
#> 124  621607 Frederick the Great 2026-08-17 01:05:40          <NA>
#> 125 2985318 Frederick the Great 2026-08-20 02:11:50          <NA>
#> 126 2658658 Frederick the Great 2026-08-20 02:15:02          <NA>
#> 127 7099883 Frederick the Great 2026-08-20 02:15:50          <NA>
#> 128 2682272 Frederick the Great 2026-08-20 02:56:50          <NA>
#> 129 4296935 Frederick the Great 2026-08-20 03:00:52          <NA>
#> 130 5347033 Frederick the Great 2026-08-20 03:00:54          <NA>
#> 131 5970570 Frederick the Great 2026-08-20 12:45:42          <NA>
#> 132 7995418 Frederick the Great 2026-08-20 12:47:47          <NA>
#> 133 4234014 Frederick the Great 2026-08-20 12:49:58          <NA>
#>            date_wminute                                        user
#> 1   2022-01-12 09:30:00 8adf4f5dd661449fa6cc1f5a0b1815c0@auth.local
#> 2   2022-01-03 07:56:00 c7efb8019da54923a9b04d4a74f0fde8@auth.local
#> 3   2021-08-05 08:30:00 c7efb8019da54923a9b04d4a74f0fde8@auth.local
#> 4                  <NA>                                         NaN
#> 5                  <NA>                                         NaN
#> 6                  <NA>                                         NaN
#> 7                  <NA>                                         NaN
#> 8                  <NA>                                         NaN
#> 9                  <NA>                                         NaN
#> 10                 <NA>                                         NaN
#> 11                 <NA>                                         NaN
#> 12                 <NA>                                         NaN
#> 13                 <NA>                                         NaN
#> 14                 <NA>                                         NaN
#> 15                 <NA>                                         NaN
#> 16                 <NA>                                         NaN
#> 17                 <NA>                                         NaN
#> 18                 <NA>                                         NaN
#> 19                 <NA>                                         NaN
#> 20                 <NA>                                         NaN
#> 21                 <NA>                                         NaN
#> 22                 <NA>                                         NaN
#> 23                 <NA>                                         NaN
#> 24                 <NA>                                         NaN
#> 25                 <NA>                                         NaN
#> 26                 <NA>                                         NaN
#> 27                 <NA>                                         NaN
#> 28                 <NA>                                         NaN
#> 29                 <NA>                                         NaN
#> 30                 <NA>                                         NaN
#> 31                 <NA>                                         NaN
#> 32                 <NA>                                         NaN
#> 33                 <NA>                                         NaN
#> 34                 <NA>                                         NaN
#> 35                 <NA>                                         NaN
#> 36                 <NA>                                         NaN
#> 37                 <NA>                                         NaN
#> 38                 <NA>                                         NaN
#> 39                 <NA>                                         NaN
#> 40                 <NA>                                         NaN
#> 41                 <NA>                                         NaN
#> 42                 <NA>                                         NaN
#> 43                 <NA>                                         NaN
#> 44                 <NA>                                         NaN
#> 45                 <NA>                                         NaN
#> 46                 <NA>                                         NaN
#> 47                 <NA>                                         NaN
#> 48                 <NA>                                         NaN
#> 49                 <NA>                                         NaN
#> 50                 <NA>                                         NaN
#> 51                 <NA>                                         NaN
#> 52                 <NA>                                         NaN
#> 53                 <NA>                                         NaN
#> 54                 <NA>                                         NaN
#> 55                 <NA>                                         NaN
#> 56                 <NA>                                         NaN
#> 57                 <NA>                                         NaN
#> 58                 <NA>                                         NaN
#> 59                 <NA>                                         NaN
#> 60                 <NA>                                         NaN
#> 61                 <NA>                                         NaN
#> 62                 <NA>                                         NaN
#> 63                 <NA>                                         NaN
#> 64                 <NA>                                         NaN
#> 65                 <NA>                                         NaN
#> 66                 <NA>                                         NaN
#> 67                 <NA>                                         NaN
#> 68                 <NA>                                         NaN
#> 69                 <NA>                                         NaN
#> 70                 <NA>                                         NaN
#> 71                 <NA>                                         NaN
#> 72                 <NA>                                         NaN
#> 73                 <NA>                                         NaN
#> 74                 <NA>                                         NaN
#> 75                 <NA>                                         NaN
#> 76                 <NA>                                         NaN
#> 77                 <NA>                                         NaN
#> 78                 <NA>                                         NaN
#> 79                 <NA>                                         NaN
#> 80                 <NA>                                         NaN
#> 81                 <NA>                                         NaN
#> 82                 <NA>                                         NaN
#> 83                 <NA>                                         NaN
#> 84                 <NA>                                         NaN
#> 85                 <NA>                                         NaN
#> 86                 <NA>                                         NaN
#> 87                 <NA>                                         NaN
#> 88                 <NA>                                         NaN
#> 89                 <NA>                                         NaN
#> 90                 <NA>                                         NaN
#> 91                 <NA>                                         NaN
#> 92                 <NA>                                         NaN
#> 93                 <NA>                                         NaN
#> 94                 <NA>                                         NaN
#> 95                 <NA>                                         NaN
#> 96                 <NA>                                         NaN
#> 97                 <NA>                                         NaN
#> 98                 <NA>                                         NaN
#> 99                 <NA>                                         NaN
#> 100                <NA>                                         NaN
#> 101                <NA>                                         NaN
#> 102                <NA>                                         NaN
#> 103                <NA>                                         NaN
#> 104                <NA>                                         NaN
#> 105                <NA>                                         NaN
#> 106                <NA>                                         NaN
#> 107                <NA>                                         NaN
#> 108                <NA>                                         NaN
#> 109                <NA>                                         NaN
#> 110                <NA>                                         NaN
#> 111                <NA>                                         NaN
#> 112                <NA>                                         NaN
#> 113                <NA>                                         NaN
#> 114                <NA>                                         NaN
#> 115                <NA>                                         NaN
#> 116                <NA>                                         NaN
#> 117                <NA>                                         NaN
#> 118                <NA>                                         NaN
#> 119                <NA>                                         NaN
#> 120                <NA>                                         NaN
#> 121                <NA>                                         NaN
#> 122                <NA>                                         NaN
#> 123                <NA>                                         NaN
#> 124                <NA>                                         NaN
#> 125                <NA>                                         NaN
#> 126                <NA>                                         NaN
#> 127                <NA>                                         NaN
#> 128                <NA>                                         NaN
#> 129                <NA>                                         NaN
#> 130                <NA>                                         NaN
#> 131                <NA>                                         NaN
#> 132                <NA>                                         NaN
#> 133                <NA>                                         NaN
#>                     initials  camid
#> 1                      AB,CD 100001
#> 2                       <NA> 100002
#> 3                       <NA> 100003
#> 4                       <NA> 100908
#> 5                       <NA> 100909
#> 6                       <NA> 101479
#> 7                      AB,CD 101480
#> 8                         EF 101483
#> 9                       <NA> 101692
#> 10                      <NA> 101694
#> 11                      <NA> 101697
#> 12                      <NA> 101698
#> 13                      <NA> 101700
#> 14                      <NA> 101703
#> 15                      <NA> 101706
#> 16                      <NA> 101709
#> 17                      <NA> 101712
#> 18                      <NA> 101715
#> 19                      <NA> 101718
#> 20                      <NA> 101721
#> 21                      <NA> 101724
#> 22                      <NA> 101727
#> 23                      <NA> 101730
#> 24                      <NA> 101733
#> 25                      <NA> 101736
#> 26                      <NA> 101739
#> 27                      <NA> 101742
#> 28                      <NA> 101745
#> 29                      <NA> 101748
#> 30                      <NA> 101751
#> 31  zztest-allow-new-options 101752
#> 32                      <NA> 101753
#> 33  zztest-allow-new-options 101754
#> 34                      <NA> 101755
#> 35                      <NA> 101758
#> 36                      <NA> 101761
#> 37                      <NA> 101764
#> 38                      <NA> 101767
#> 39                      <NA> 101770
#> 40                      <NA> 101773
#> 41                      <NA> 101776
#> 42                      <NA> 101779
#> 43                      <NA> 101782
#> 44                      <NA> 101785
#> 45                      <NA> 101788
#> 46                      <NA> 101790
#> 47                      <NA> 101794
#> 48                      <NA> 101797
#> 49                      <NA> 101800
#> 50                      <NA> 101803
#> 51                      <NA> 101806
#> 52                      <NA> 101807
#> 53                      <NA> 101812
#> 54                      <NA> 101815
#> 55                      <NA> 101818
#> 56                      <NA> 101821
#> 57                      <NA> 101822
#> 58                      <NA> 101827
#> 59                      <NA> 101830
#> 60                      <NA> 101831
#> 61                      <NA> 101836
#> 62                      <NA> 101839
#> 63                      <NA> 101842
#> 64                      <NA> 101845
#> 65                      <NA> 101848
#> 66                      <NA> 101851
#> 67                      <NA> 101854
#> 68                      <NA> 101855
#> 69                      <NA> 101858
#> 70                      <NA> 101863
#> 71                      <NA> 101864
#> 72                      <NA> 101869
#> 73                      <NA> 101870
#> 74                      <NA> 101875
#> 75                      <NA> 101878
#> 76                      <NA> 101881
#> 77                      <NA> 101884
#> 78                      <NA> 101887
#> 79                      <NA> 101888
#> 80                      <NA> 101893
#> 81                      <NA> 101894
#> 82                      <NA> 101899
#> 83                      <NA> 101902
#> 84                      <NA> 101905
#> 85                      <NA> 101908
#> 86                      <NA> 101911
#> 87                      <NA> 101912
#> 88                      <NA> 101913
#> 89                      <NA> 101919
#> 90                      <NA> 101921
#> 91                      <NA> 101922
#> 92                      <NA> 101929
#> 93                      <NA> 101932
#> 94                      <NA> 101933
#> 95                      <NA> 101938
#> 96                      <NA> 101939
#> 97                      <NA> 101944
#> 98                      <NA> 101947
#> 99                      <NA> 101950
#> 100                     <NA> 101953
#> 101                     <NA> 101956
#> 102                     <NA> 101959
#> 103                     <NA> 101962
#> 104                     <NA> 101965
#> 105                     <NA> 101968
#> 106                     <NA> 101971
#> 107                     <NA> 101972
#> 108                     <NA> 101977
#> 109                     <NA> 101978
#> 110                     <NA> 101983
#> 111                     <NA> 101986
#> 112                     <NA> 101989
#> 113                     <NA> 101992
#> 114                     <NA> 101995
#> 115                     <NA> 101996
#> 116                     <NA> 102001
#> 117                     <NA> 102004
#> 118                     <NA> 102005
#> 119                     <NA> 102008
#> 120                     <NA> 102011
#> 121                     <NA> 102014
#> 122                     <NA> 102019
#> 123                     <NA> 102020
#> 124                     <NA> 102025
#> 125                     <NA> 102028
#> 126                     <NA> 102031
#> 127                     <NA> 102032
#> 128                     <NA> 102037
#> 129                     <NA> 102040
#> 130                     <NA> 102041
#> 131                     <NA> 102049
#> 132                     <NA> 102052
#> 133                     <NA> 102055
# }
# \donttest{
flytable_query("SELECT person, fruit_name FROM testfruit WHERE person!='Bob'")
#>                  person  fruit_name
#> 1                 Alice       apple
#> 2                 Clara  clementine
#> 3   Frederick the Great        kiwi
#> 4   Frederick the Great        kiwi
#> 5   Frederick the Great        kiwi
#> 6   Frederick the Great        kiwi
#> 7   Frederick the Great        kiwi
#> 8   Frederick the Great        kiwi
#> 9   Frederick the Great        kiwi
#> 10  Frederick the Great        kiwi
#> 11  Frederick the Great        kiwi
#> 12  Frederick the Great        kiwi
#> 13  Frederick the Great        kiwi
#> 14  Frederick the Great        kiwi
#> 15  Frederick the Great        kiwi
#> 16  Frederick the Great        kiwi
#> 17  Frederick the Great        kiwi
#> 18  Frederick the Great        kiwi
#> 19  Frederick the Great        kiwi
#> 20  Frederick the Great        kiwi
#> 21  Frederick the Great        kiwi
#> 22  Frederick the Great        kiwi
#> 23  Frederick the Great        kiwi
#> 24  Frederick the Great        kiwi
#> 25  Frederick the Great        kiwi
#> 26  Frederick the Great        kiwi
#> 27  Frederick the Great        kiwi
#> 28  Frederick the Great        kiwi
#> 29      Delta Sync Test dragonfruit
#> 30    Multi Select Test   starfruit
#> 31      Delta Sync Test dragonfruit
#> 32    Multi Select Test   starfruit
#> 33  Frederick the Great        kiwi
#> 34  Frederick the Great        kiwi
#> 35  Frederick the Great        kiwi
#> 36  Frederick the Great        kiwi
#> 37  Frederick the Great        kiwi
#> 38  Frederick the Great        kiwi
#> 39  Frederick the Great        kiwi
#> 40  Frederick the Great        kiwi
#> 41  Frederick the Great        kiwi
#> 42  Frederick the Great        kiwi
#> 43  Frederick the Great        kiwi
#> 44  Frederick the Great        kiwi
#> 45  Frederick the Great        kiwi
#> 46  Frederick the Great        kiwi
#> 47  Frederick the Great        kiwi
#> 48  Frederick the Great        kiwi
#> 49  Frederick the Great        kiwi
#> 50  Frederick the Great        kiwi
#> 51  Frederick the Great        kiwi
#> 52  Frederick the Great        kiwi
#> 53  Frederick the Great        kiwi
#> 54  Frederick the Great        kiwi
#> 55  Frederick the Great        kiwi
#> 56  Frederick the Great        kiwi
#> 57  Frederick the Great        kiwi
#> 58  Frederick the Great        kiwi
#> 59  Frederick the Great        kiwi
#> 60  Frederick the Great        kiwi
#> 61  Frederick the Great        kiwi
#> 62  Frederick the Great        kiwi
#> 63  Frederick the Great        kiwi
#> 64  Frederick the Great        kiwi
#> 65  Frederick the Great        kiwi
#> 66  Frederick the Great        kiwi
#> 67  Frederick the Great        kiwi
#> 68  Frederick the Great        kiwi
#> 69  Frederick the Great        kiwi
#> 70  Frederick the Great        kiwi
#> 71  Frederick the Great        kiwi
#> 72  Frederick the Great        kiwi
#> 73  Frederick the Great        kiwi
#> 74  Frederick the Great        kiwi
#> 75  Frederick the Great        kiwi
#> 76  Frederick the Great        kiwi
#> 77  Frederick the Great        kiwi
#> 78  Frederick the Great        kiwi
#> 79  Frederick the Great        kiwi
#> 80  Frederick the Great        kiwi
#> 81  Frederick the Great        kiwi
#> 82  Frederick the Great        kiwi
#> 83  Frederick the Great        kiwi
#> 84  Frederick the Great        kiwi
#> 85  Frederick the Great        kiwi
#> 86  Frederick the Great        kiwi
#> 87  Frederick the Great        kiwi
#> 88  Frederick the Great        kiwi
#> 89  Frederick the Great        kiwi
#> 90  Frederick the Great        kiwi
#> 91  Frederick the Great        kiwi
#> 92  Frederick the Great        kiwi
#> 93  Frederick the Great        kiwi
#> 94  Frederick the Great        kiwi
#> 95  Frederick the Great        kiwi
#> 96  Frederick the Great        kiwi
#> 97  Frederick the Great        kiwi
#> 98  Frederick the Great        kiwi
#> 99  Frederick the Great        kiwi
#> 100 Frederick the Great        kiwi
#> 101 Frederick the Great        kiwi
#> 102 Frederick the Great        kiwi
#> 103 Frederick the Great        kiwi
#> 104 Frederick the Great        kiwi
#> 105 Frederick the Great        kiwi
#> 106 Frederick the Great        kiwi
#> 107 Frederick the Great        kiwi
#> 108 Frederick the Great        kiwi
#> 109 Frederick the Great        kiwi
#> 110 Frederick the Great        kiwi
#> 111 Frederick the Great        kiwi
#> 112 Frederick the Great        kiwi
#> 113 Frederick the Great        kiwi
#> 114 Frederick the Great        kiwi
#> 115 Frederick the Great        kiwi
#> 116 Frederick the Great        kiwi
#> 117 Frederick the Great        kiwi
#> 118 Frederick the Great        kiwi
#> 119 Frederick the Great        kiwi
#> 120 Frederick the Great        kiwi
#> 121 Frederick the Great        kiwi
#> 122 Frederick the Great        kiwi
#> 123 Frederick the Great        kiwi
#> 124 Frederick the Great        kiwi
#> 125 Frederick the Great        kiwi
#> 126 Frederick the Great        kiwi
#> 127 Frederick the Great        kiwi
#> 128 Frederick the Great        kiwi
#> 129 Frederick the Great        kiwi
#> 130 Frederick the Great        kiwi
#> 131 Frederick the Great        kiwi
#> 132 Frederick the Great        kiwi
# }
if (FALSE) { # \dontrun{
flytable_query(paste("SELECT root_id, supervoxel_id FROM info limit 5"))
} # }
```
