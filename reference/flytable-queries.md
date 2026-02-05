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
  collapse_lists = TRUE
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

  An optional limit, which only applies if you do not specify a limit
  directly in the `sql` query. By default seatable limits SQL queries to
  100 rows. We increase the limit to 100000 rows by default.

- collapse_lists:

  Whether to collapse any list multi-select columns into simple strings.
  The default value of `collapse_lists=TRUE` will comma separate them.

- python:

  Whether to return a Python pandas `DataFrame`. The default of `FALSE`
  returns an R `data.frame`

- chunksize:

  Optional The maximum number of rows to request in one web request. For
  advanced use only as the default value of `NULL` will fetch as many as
  possible.

- sql:

  A SQL query string. See examples and [seatable
  docs](https://seatable.github.io/seatable-scripts/python/query/).

- convert:

  Expert use only: Whether or not to allow the Python seatable module to
  process raw output from the database. This is is principally for
  debugging purposes. NB this imposes a requirement of seatable_api
  \>=2.4.0.

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
[`flytable_alltables_cached()`](https://natverse.org/fafbseg/reference/flytable_alltables_cached.md),
[`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md),
[`flytable_update_rows()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)

## Examples

``` r
# \donttest{
flytable_list_rows(table = "testfruit")
#>                        _id              _mtime              _ctime fruit_name
#> 1   H8BckTnXRL2PaVuLDcRPMA 2025-06-27 10:51:59 2021-12-17 13:36:17      apple
#> 2   SrBW3vFLRxafKRYHoPrGQQ 2022-05-12 16:58:08 2021-12-17 13:36:17     banana
#> 3   F1h-TZKpTcWiHujZ2xnF8Q 2024-08-28 01:30:00 2021-12-17 13:36:17 clementine
#> 4   dwTLFmsDRoCxhE3BwY-ovQ 2026-02-05 17:05:23 2024-08-28 01:30:00       kiwi
#> 5   Im6VZG_DQ7mRJh6BAUwW7A 2024-08-28 01:31:13 2024-08-28 01:31:13       kiwi
#> 6   bQRjGvpVRsSwPs64A0MCvA 2024-08-28 01:32:04 2024-08-28 01:32:04       kiwi
#> 7   GbicL5tqQNaLNEQ_yM0Klw 2024-08-31 12:38:49 2024-08-31 12:38:49       kiwi
#> 8   aw-npAVQT4mntcWx7bgFWw 2024-08-31 12:38:52 2024-08-31 12:38:52       kiwi
#> 9   ZDzN99vJTg62XCg1F9l_Sg 2024-08-31 12:39:46 2024-08-31 12:39:46       kiwi
#> 10  dsuHTT-kQCK7wiMcV_TpQA 2024-08-31 12:40:00 2024-08-31 12:40:00       kiwi
#> 11  Bw_VNvoSTCurbic6P-8Wcw 2024-08-31 12:41:13 2024-08-31 12:41:13       kiwi
#> 12  OO5EnFvtR7G8YwLwaod2yw 2024-08-31 12:41:22 2024-08-31 12:41:22       kiwi
#> 13  fnSGJR-YTaiZFHIkY5-KkQ 2024-09-02 12:04:36 2024-09-02 12:04:36       kiwi
#> 14  WGgpaimYSnqad-O6_9coPw 2024-09-02 12:06:00 2024-09-02 12:06:00       kiwi
#> 15  aNnNs9VUS1CQ0xJt2x2Aow 2024-09-02 12:07:49 2024-09-02 12:07:49       kiwi
#> 16  UsmS_5PnT2i0tyQOJbRc8g 2024-09-07 09:42:06 2024-09-07 09:42:06       kiwi
#> 17  Ar7RjcG4RDahYuzCyxutwg 2024-09-07 09:42:29 2024-09-07 09:42:29       kiwi
#> 18  C8v4eN3NSKqtUgkC7FORqQ 2024-09-07 09:42:36 2024-09-07 09:42:36       kiwi
#> 19  RX1-ov-0TBCuXe1fdEZD_A 2024-09-07 10:35:19 2024-09-07 10:35:19       kiwi
#> 20  aCtWKRz7QbyLtVqvEOQjLg 2024-09-07 10:36:23 2024-09-07 10:36:23       kiwi
#> 21  B3hDsTqPSxq8XNoG-uAk5Q 2024-09-07 10:36:38 2024-09-07 10:36:38       kiwi
#> 22  WCHZt7M9Tt264ii769syvw 2024-09-07 14:32:14 2024-09-07 14:32:14       kiwi
#> 23  cVn-ZOc_RSqzxuwlumMOLQ 2024-09-07 14:33:04 2024-09-07 14:33:04       kiwi
#> 24  Xbsa6qGVSASp3QUcQx6b-Q 2024-09-07 14:35:43 2024-09-07 14:35:43       kiwi
#> 25  GeIE3dUuQsyxkR_Mjgadng 2024-09-07 16:07:22 2024-09-07 16:07:22       kiwi
#> 26  N9locli7T6uAy0lRcoWgNQ 2024-09-07 16:08:40 2024-09-07 16:08:40       kiwi
#> 27  X6bkdGHyRO6Mr4Yg2nhjXQ 2024-09-07 16:09:26 2024-09-07 16:09:26       kiwi
#> 28  X18JYP_-SSuzhrmVXvJg_g 2024-09-12 14:59:43 2024-09-12 14:59:43       kiwi
#> 29  evAYgrC4SZ6xDEXWLKsOKA 2024-09-12 16:07:35 2024-09-12 16:07:35       kiwi
#> 30  ehStvn3lSmWLIxcdhF_LYQ 2024-09-12 16:08:23 2024-09-12 16:08:23       kiwi
#> 31  fl8ki1g4QgWoCPedqKmOkA 2024-09-12 16:08:34 2024-09-12 16:08:34       kiwi
#> 32  EG-hwTJlQLKSH8FpKocPHw 2024-09-23 05:39:41 2024-09-23 05:39:41       kiwi
#> 33  FfxUUPRDRsmDo5HSOeting 2024-09-23 10:14:08 2024-09-23 10:14:08       kiwi
#> 34  c4izfXquRIyn2j8FmD0NLA 2024-10-20 22:40:48 2024-10-20 22:40:48       kiwi
#> 35  KYKhCXbyShGjb8-hvNQ3Rw 2024-10-20 22:55:47 2024-10-20 22:55:47       kiwi
#> 36  JNCpf_3xQnyvTM5QION5YQ 2024-10-20 23:06:48 2024-10-20 23:06:48       kiwi
#> 37  FVL0GzeAQhKj5TlHWm97EQ 2024-10-21 02:43:09 2024-10-21 02:43:09       kiwi
#> 38  S1VH2tR7TXe2Ttl6iP1Wyg 2024-10-21 09:27:59 2024-10-21 09:27:59       kiwi
#> 39  Ftq8Scc2R1KYgB1NCmJlJg 2024-10-21 18:00:20 2024-10-21 18:00:20       kiwi
#> 40  EWRkx7u9THK1SKFwGggtAw 2024-10-21 18:08:44 2024-10-21 18:08:44       kiwi
#> 41  BbRhVBJ8RlitYRXpF6obCA 2024-10-21 18:30:26 2024-10-21 18:30:26       kiwi
#> 42  BOH8Y1g1T0SJC4VUYHMhCQ 2024-10-21 18:36:40 2024-10-21 18:36:40       kiwi
#> 43  RF7tSp2VSeS7HVNRFJn1dw 2024-10-21 18:42:38 2024-10-21 18:42:38       kiwi
#> 44  NLwCyXxvTvyPsHmeWyLF1A 2024-10-21 18:43:47 2024-10-21 18:43:47       kiwi
#> 45  R5bOqBHJTwGzU37IO4DbWw 2024-10-21 18:44:16 2024-10-21 18:44:16       kiwi
#> 46  Pvfl8qdLQ3e9DXvfN6lYZQ 2024-10-21 18:45:31 2024-10-21 18:45:31       kiwi
#> 47  MX6L7fw9Sg69SSU96SvvYw 2024-10-21 18:48:12 2024-10-21 18:48:12       kiwi
#> 48  Pvujd_F3TWmSdys3MjEsmQ 2024-10-21 18:49:18 2024-10-21 18:49:18       kiwi
#> 49  QZc-osr8RfGumH9wrGzBPQ 2024-10-21 18:52:46 2024-10-21 18:52:46       kiwi
#> 50  L_wNAO-yQQ-Ki8r5fayk4w 2024-10-21 19:04:00 2024-10-21 19:04:00       kiwi
#> 51  ZRn0BDHsTaiEgoHqTBwJRA 2024-10-21 19:08:30 2024-10-21 19:08:30       kiwi
#> 52  LNsqDUGCQsO9XMlq6utHyg 2024-10-21 19:08:45 2024-10-21 19:08:45       kiwi
#> 53  N6QMUi1XTTWZGhFqGNPBiw 2024-10-21 19:11:30 2024-10-21 19:11:30       kiwi
#> 54  YzwemhbLSbCAY2kfj2Q9AQ 2024-10-21 19:22:52 2024-10-21 19:22:52       kiwi
#> 55  H7T6dDrDTWikKLoEqX4lgQ 2024-10-21 19:23:43 2024-10-21 19:23:43       kiwi
#> 56  fO8HjBbPQxa3DRkbpcly6A 2024-10-21 19:24:20 2024-10-21 19:24:20       kiwi
#> 57  Bal6eWgeSZS53Xvg5iP5-g 2024-10-21 19:24:40 2024-10-21 19:24:40       kiwi
#> 58  E54Y-OonQxeZqCCuU3kVyg 2024-10-21 19:27:30 2024-10-21 19:27:30       kiwi
#> 59  OI-NfQzlSPmw2biG3TC2Rg 2024-10-21 19:27:39 2024-10-21 19:27:39       kiwi
#> 60  QlwFDxZDRnGPIIXwfc97kg 2024-10-25 05:30:59 2024-10-25 05:30:59       kiwi
#> 61  da0c04a-Qz6KIRaouhToxA 2024-10-25 05:31:42 2024-10-25 05:31:42       kiwi
#> 62  CbEJnnzLRZSPxTw7QfIi4Q 2024-10-25 05:33:25 2024-10-25 05:33:25       kiwi
#> 63  Oezyp2B7QK6bdSJNz-F9lw 2024-10-28 17:41:08 2024-10-28 17:41:08       kiwi
#> 64  e_UoBzj6Twi1Na5vbfOknA 2024-10-28 17:41:36 2024-10-28 17:41:36       kiwi
#> 65  UGbuMTThS4-Dmy2tlrqncw 2024-10-28 17:42:54 2024-10-28 17:42:54       kiwi
#> 66  L_JYyqf4QUKc5Ql2DTeZoQ 2024-10-28 18:03:03 2024-10-28 18:03:03       kiwi
#> 67  MTKjtUCXTeycaToxX67h-w 2024-10-28 18:04:36 2024-10-28 18:04:36       kiwi
#> 68  L4LaRZFtTD6oBEqNmuCG2g 2024-10-28 18:04:54 2024-10-28 18:04:54       kiwi
#> 69  fj1UDjjqSLKL5MgYX-YZPQ 2024-10-28 19:45:22 2024-10-28 19:45:22       kiwi
#> 70  B_eztaSzRD29-QAFHKteMw 2024-10-28 19:50:22 2024-10-28 19:50:22       kiwi
#> 71  fHngScV1T7mACw5pM2IEnQ 2024-10-28 19:51:23 2024-10-28 19:51:23       kiwi
#> 72  JKowCbFBQBChGTM6qCY2bw 2024-10-28 19:51:23 2024-10-28 19:51:23       kiwi
#> 73  LafWI7Y6RFCJeManNTzM-Q 2024-10-28 19:51:25 2024-10-28 19:51:25       kiwi
#> 74  T13oWVu7Q2miytL5KOE7Ig 2024-10-28 19:52:35 2024-10-28 19:52:35       kiwi
#> 75  NEdDV72WRKC6tkmY6ZZGew 2024-10-28 19:53:03 2024-10-28 19:53:03       kiwi
#> 76  Tex8Um6USiis2rLHeISlNg 2024-11-10 16:02:27 2024-11-10 16:02:27       kiwi
#> 77  CyU5mg6dRQqdvkae6jXEWQ 2024-11-10 16:04:40 2024-11-10 16:04:40       kiwi
#> 78  MGVonuKtQJeqUVKIeVDxlA 2024-11-10 16:05:44 2024-11-10 16:05:44       kiwi
#> 79  VIDnQBTHSvCDJvRpB6fAIA 2024-11-10 17:16:51 2024-11-10 17:16:51       kiwi
#> 80  RTJVuB9IT4a8lWPqR-TZ7g 2024-11-10 17:26:11 2024-11-10 17:26:11       kiwi
#> 81  Qbzj1tgmRRqtxflFmsOLlQ 2024-11-10 17:27:36 2024-11-10 17:27:36       kiwi
#> 82  OHYl4zqXToukCTSsnu4Cyg 2024-11-10 17:27:54 2024-11-10 17:27:54       kiwi
#> 83  Ipjd0t8KT4esoXujJ_fm5g 2024-11-10 17:29:18 2024-11-10 17:29:18       kiwi
#> 84  Suzx1cX6S0S2I39de5xqww 2024-11-10 17:49:24 2024-11-10 17:49:24       kiwi
#> 85  dGChDqKUSH6Y8dPHcDvF-Q 2024-11-10 17:50:56 2024-11-10 17:50:56       kiwi
#> 86  euIMyP1YSSuj6fswxV9JeA 2024-11-10 17:51:13 2024-11-10 17:51:13       kiwi
#> 87  BGbg6oWWQzuI2sn15qk7Xg 2024-11-10 17:58:10 2024-11-10 17:58:10       kiwi
#> 88  f9yV_KiiRwmnS-BZQSgv4Q 2024-11-10 18:04:23 2024-11-10 18:04:23       kiwi
#> 89  JkbFw-SZRziGIZmnXGoCXA 2024-11-10 18:06:21 2024-11-10 18:06:21       kiwi
#> 90  Ig2ch3CgTDCGl2kk2gVRxQ 2024-11-10 18:06:31 2024-11-10 18:06:31       kiwi
#> 91  A52LCbbzRqi2zcJKRA2Tyg 2024-11-10 18:06:37 2024-11-10 18:06:37       kiwi
#> 92  WGV4ZzPpQ7S-lezrPUhjmg 2024-11-29 16:40:18 2024-11-29 16:40:18       kiwi
#> 93  Ra4dGSzoQWyFS_yGY0ah6A 2024-11-29 16:43:02 2024-11-29 16:43:02       kiwi
#> 94  UeJuXY1AR027nZ-kKJq7NA 2024-11-29 16:43:16 2024-11-29 16:43:16       kiwi
#> 95  ZGQBAdyhRt6CVHousezFKw 2025-01-28 10:44:15 2025-01-28 10:44:15       kiwi
#> 96  fHdnZ4PsQJGR9kqBF8JquQ 2025-02-01 14:47:45 2025-02-01 14:47:45       kiwi
#> 97  P-PEZ1eCRT6SRJzBNfweYg 2025-02-01 14:48:15 2025-02-01 14:48:15       kiwi
#> 98  CaZTyhdDRVOj7lKnfwjYqA 2025-02-01 14:49:19 2025-02-01 14:49:19       kiwi
#> 99  V9td9yWFSnKOJuYdfTh9hw 2025-02-01 14:50:30 2025-02-01 14:50:30       kiwi
#> 100 Tegbp-sJSoy6diSXy6_WWQ 2025-03-10 10:27:29 2025-03-10 10:27:29       kiwi
#> 101 d5LJywz3Sp-v5mnSsKEdlQ 2025-03-10 11:15:28 2025-03-10 11:15:28       kiwi
#> 102 Tkwga6I5S_qe0Abq_JmBvg 2025-03-10 13:44:53 2025-03-10 13:44:53       kiwi
#> 103 D35WpbIxTIKN8xJkScUk1g 2025-03-16 08:53:41 2025-03-16 08:53:41       kiwi
#> 104 UjAesnk-Rri0_C9N9rNy3g 2025-03-16 10:01:57 2025-03-16 10:01:57       kiwi
#> 105 A-3VPCfvSPW6tjuxE4-s8Q 2025-03-19 15:21:06 2025-03-19 15:21:06       kiwi
#> 106 dQmo5jwqT9ilnLsXac_nTQ 2025-04-07 11:34:43 2025-04-07 11:34:43       kiwi
#> 107 HzWQgEAOQNKxgzgs_Ga47Q 2025-04-07 11:36:15 2025-04-07 11:36:15       kiwi
#> 108 IImIQNhDSO610ffNIVdxJw 2025-04-07 15:34:48 2025-04-07 15:34:48       kiwi
#> 109 TqVWQAp7Tqq8dM72HWsCcg 2025-04-07 15:36:18 2025-04-07 15:36:18       kiwi
#> 110 IL8MHOK0SEStaF7QZa5-gw 2025-04-07 15:44:40 2025-04-07 15:44:40       kiwi
#> 111 Kxv1V0FdSl2a_PGdAnGzBg 2025-04-08 07:26:28 2025-04-08 07:26:28       kiwi
#> 112 P2Uz89CXSwCy3lJyIauosQ 2025-04-08 07:26:54 2025-04-08 07:26:54       kiwi
#> 113 V6gSj-a9SyCqQ9jjzMxiDQ 2025-04-08 07:29:48 2025-04-08 07:29:48       kiwi
#> 114 QurG4H9iTVWTTkSrV57Pxw 2025-04-08 08:27:07 2025-04-08 08:27:07       kiwi
#> 115 IVMVNgEPRVinkfnf3TXkQg 2025-04-08 08:27:24 2025-04-08 08:27:24       kiwi
#> 116 OZUd9bhtTbCrYs7eVTTSwA 2025-04-08 08:27:38 2025-04-08 08:27:38       kiwi
#> 117 PEI0GFFTQHq_pHbW9vmS6A 2025-04-25 08:10:07 2025-04-25 08:10:07       kiwi
#> 118 H9mCTulQRtKbmUsd7TF4lA 2025-04-28 14:07:32 2025-04-28 14:07:32       kiwi
#> 119 cUEKpvCHR0q3nuptDMzKIg 2025-04-28 14:35:55 2025-04-28 14:35:55       kiwi
#> 120 eVEEbSIARbK5miHNbdYBpQ 2025-04-28 14:38:02 2025-04-28 14:38:02       kiwi
#> 121 TyHeutJ8QlG63yilw0R31g 2025-04-28 14:38:15 2025-04-28 14:38:15       kiwi
#> 122 RoWiGOlbRpuqB6TccLRAcw 2025-04-28 16:45:28 2025-04-28 16:45:28       kiwi
#> 123 aUM_u_v-TFejBtFHCXxQiw 2025-04-28 16:46:05 2025-04-28 16:46:05       kiwi
#> 124 c-9wWjzgQIeLgjgYX-JX6A 2025-04-28 16:46:36 2025-04-28 16:46:36       kiwi
#> 125 eSjwLZ9PSSSXs7DC38hK9A 2025-04-28 22:56:12 2025-04-28 22:56:12       kiwi
#> 126 YJOQkvYGQqKLpb0tKt96Pw 2025-04-28 22:56:49 2025-04-28 22:56:49       kiwi
#> 127 KTDrwo5wRlinZakmbCa18w 2025-04-28 22:57:00 2025-04-28 22:57:00       kiwi
#> 128 OKHemOsAS6eYZLP8tE92mw 2025-04-29 07:03:29 2025-04-29 07:03:29       kiwi
#> 129 bPkK_OWMRia6BDH4Jh4Q5Q 2025-05-01 11:20:18 2025-05-01 11:20:18       kiwi
#> 130 Virs3xMeTkm0kfzHFZcrxA 2025-05-01 11:21:06 2025-05-01 11:21:06       kiwi
#> 131 KoDauA_qQd61ZJj3yFleyw 2025-05-01 11:21:20 2025-05-01 11:21:20       kiwi
#> 132 B-0VYxMmQ4KPNIevWOADZA 2025-06-24 17:29:49 2025-06-24 17:29:49       kiwi
#> 133 Tp4t21bgSJevTsMsaksryQ 2025-07-07 17:19:59 2025-07-07 17:19:59       kiwi
#> 134 KS6xTgr3Q_WV8tTbgWh8BQ 2025-07-07 17:21:42 2025-07-07 17:21:42       kiwi
#> 135 U47zetvCT_Otp3s7GPJDDQ 2025-07-07 17:23:47 2025-07-07 17:23:47       kiwi
#> 136 X-b45pb5Shu2zErseYFuww 2025-07-08 12:50:21 2025-07-08 12:50:21       kiwi
#> 137 K7exC62VSYazZ-fZv18KdQ 2025-07-08 12:51:49 2025-07-08 12:51:49       kiwi
#> 138 SEcf3LUQRjC74y_o7Kjezw 2025-07-08 12:52:31 2025-07-08 12:52:31       kiwi
#> 139 SyfAiZ_TSO2GIYpl8Dw8WA 2025-09-03 17:30:45 2025-09-03 17:30:45       kiwi
#> 140 TzMsX_6TRwmiNAe9AfsrHg 2025-09-05 22:00:41 2025-09-05 22:00:41       kiwi
#> 141 U4MyybR3TkKa646qnGX6NQ 2025-09-07 08:27:54 2025-09-07 08:27:54       kiwi
#> 142 cd-CBvQlReCQnm1rBkxPFQ 2025-09-07 17:47:27 2025-09-07 17:47:27       kiwi
#> 143 bmPXsv3IRsOUk-88S9saHA 2025-09-07 17:47:51 2025-09-07 17:47:51       kiwi
#> 144 cCbPU5vpSSWJdMQRmdoi9w 2025-09-07 17:52:52 2025-09-07 17:52:52       kiwi
#> 145 AN69mR-FRMSJcgF0PKH7sA 2025-09-07 22:11:09 2025-09-07 22:11:09       kiwi
#> 146 X2CfmOPvQbu3Dc8NAqi6nw 2025-09-07 22:12:41 2025-09-07 22:12:41       kiwi
#> 147 MY0us63TTb63qHYJjF663Q 2025-09-07 22:14:39 2025-09-07 22:14:39       kiwi
#> 148 Jn91j8trRcyYhJEmtyqI6g 2025-09-07 22:32:03 2025-09-07 22:32:03       kiwi
#> 149 MdwN36eYS_S2fScuEz9BPg 2025-09-07 22:33:06 2025-09-07 22:33:06       kiwi
#> 150 Cc6N-a8uREmHA-rh_N1nWg 2025-09-07 22:36:45 2025-09-07 22:36:45       kiwi
#> 151 EdUijdWKTXSBl-yyutwh4g 2025-09-08 08:50:59 2025-09-08 08:50:59       kiwi
#> 152 RxDWEM4RTOONIyJRNwZKxQ 2025-09-08 08:51:31 2025-09-08 08:51:31       kiwi
#> 153 I00bDZl-Td66O1HeqOnQow 2025-09-08 08:51:43 2025-09-08 08:51:43       kiwi
#> 154 Jae9TGKlTBOD-QAuCkf8sA 2025-12-03 14:48:18 2025-12-03 14:48:18       kiwi
#> 155 CbO4cjkbRtypWSaNBYIKEA 2025-12-03 14:49:08 2025-12-03 14:49:08       kiwi
#> 156 Q0V5TYCvQ7mnkKnGdIsjow 2025-12-03 14:49:51 2025-12-03 14:49:51       kiwi
#> 157 W1XRDljMTv6vlXpTfwmuBA 2025-12-03 15:06:53 2025-12-03 15:06:53       kiwi
#> 158 Ib6CDF20SBOnf6fO-Tms5A 2025-12-03 15:07:23 2025-12-03 15:07:23       kiwi
#> 159 DLoBNePRSFeFuYfQRZQppw 2025-12-03 15:08:54 2025-12-03 15:08:54       kiwi
#> 160 IX7g0myFTpig7D8TIhX7qw 2025-12-03 17:50:32 2025-12-03 17:50:32       kiwi
#> 161 EJbm5S-iS8CTw8WgMx9zCw 2025-12-03 17:51:47 2025-12-03 17:51:47       kiwi
#> 162 CFRZkuU8QHO-vjoTnsHpfg 2025-12-03 17:53:36 2025-12-03 17:53:36       kiwi
#> 163 c0raLmd2Q-O_vUJ9A2mW9Q 2025-12-05 10:59:42 2025-12-05 10:59:42       kiwi
#> 164 Hui75kdAR2GK_32pMHynJA 2025-12-05 11:01:22 2025-12-05 11:01:22       kiwi
#> 165 G_Uz5I9NR0yByauUCncWbw 2025-12-05 11:02:36 2025-12-05 11:02:36       kiwi
#> 166 FzGeXLjqTI-94rjuPFEYow 2025-12-05 13:41:51 2025-12-05 13:41:51       kiwi
#> 167 QhsVpJsJSueH4mB9xWJ6Gg 2025-12-05 13:43:01 2025-12-05 13:43:01       kiwi
#> 168 DJYeC9vjRMiBPccJM254bg 2025-12-05 13:45:49 2025-12-05 13:45:49       kiwi
#> 169 aYchWJzMQXCthzCMz2-ctg 2025-12-08 10:02:42 2025-12-08 10:02:42       kiwi
#> 170 Wpq_dIeaSa2WMC64Y4sBNw 2025-12-08 10:04:50 2025-12-08 10:04:50       kiwi
#> 171 bO17HVUlQgGTROUjJoyA6A 2025-12-08 10:05:19 2025-12-08 10:05:19       kiwi
#> 172 FdN2hE2_SAq3dvlBLnZx0A 2025-12-19 13:28:52 2025-12-19 13:28:52       kiwi
#> 173 eqiqZX1_Ql6PP6ew5fpSAQ 2025-12-19 13:32:06 2025-12-19 13:32:06       kiwi
#> 174 LQxEmv2qRCi_1oFV0O2Cxg 2025-12-19 13:39:35 2025-12-19 13:39:35       kiwi
#> 175 FZixexjtSKCk5RrPEDEo6Q 2025-12-19 13:40:32 2025-12-19 13:40:32       kiwi
#> 176 U_2BRb47T9q68bXHNDyI3g 2025-12-19 13:41:53 2025-12-19 13:41:53       kiwi
#> 177 Aks1xw_RQLeYk7gMH2DvJA 2025-12-19 13:41:54 2025-12-19 13:41:54       kiwi
#> 178 PF8WP_T3Tx-yJuEvQhYnWA 2025-12-19 13:42:23 2025-12-19 13:42:23       kiwi
#> 179 Su2bCYQaTwOBSndkpo-w7g 2025-12-19 13:46:10 2025-12-19 13:46:10       kiwi
#> 180 WGdBhxxUTieGfUf6PEbnkA 2026-02-03 16:13:46 2026-02-03 16:13:46       kiwi
#> 181 PeuxoxPASjWjN8-L0sEJTA 2026-02-03 16:18:02 2026-02-03 16:18:02       kiwi
#> 182 aldzi5FySlq-uOOVS_3zrw 2026-02-03 16:18:41 2026-02-03 16:18:41       kiwi
#> 183 WyuQsBSSTqCutlgpGtCt-A 2026-02-03 16:23:52 2026-02-03 16:23:52       kiwi
#> 184 DYWUZ8Y6Sf-zYEp4_0v4hA 2026-02-03 16:26:13 2026-02-03 16:26:13       kiwi
#> 185 PXzUP6zjTdSoVmfyzee90g 2026-02-03 16:56:19 2026-02-03 16:56:19       kiwi
#> 186 GwCC5pxrTKqY0f7ycqYs2w 2026-02-03 19:03:00 2026-02-03 19:03:00       kiwi
#> 187 YU8URrsNQ16d6eLkmPFcPQ 2026-02-03 19:38:52 2026-02-03 19:38:52       kiwi
#> 188 dj489_ZfQJmTA23bb-or4A 2026-02-03 19:42:17 2026-02-03 19:42:17       kiwi
#> 189 Zwu-xh7URDy8iJpBd7fBIw 2026-02-03 19:55:25 2026-02-03 19:55:25       kiwi
#> 190 QL8zYU6LSqWHZx1o-FYwzw 2026-02-05 08:23:22 2026-02-05 08:23:22       kiwi
#> 191 dFTi6fyqTX-W9qMC2M1iKg 2026-02-05 08:28:20 2026-02-05 08:28:20       kiwi
#> 192 G7-VqkxVR4Wl5FOv-jAPIQ 2026-02-05 10:21:24 2026-02-05 10:21:24       kiwi
#> 193 VzD1QJP1T6aBvU4c7pMF2g 2026-02-05 10:21:58 2026-02-05 10:21:58       kiwi
#> 194 KCtONuDtSzuZTj7FQvYV5w 2026-02-05 10:28:23 2026-02-05 10:28:23       kiwi
#> 195 NSK9InBZTYq5DC1DWFq_vw 2026-02-05 10:30:12 2026-02-05 10:30:12       kiwi
#> 196 A4Si3vPQRHmJn6E9PLhtQw 2026-02-05 10:32:11 2026-02-05 10:32:11       kiwi
#> 197 DUusun8lTYu8YtkgUNBIMg 2026-02-05 10:34:47 2026-02-05 10:34:47       kiwi
#> 198 TJwQGMpHSWKJS4sbx5inlQ 2026-02-05 10:41:51 2026-02-05 10:41:51       kiwi
#> 199 XS6x9Hz7SMu4E50tgMTULA 2026-02-05 13:18:07 2026-02-05 13:18:07       kiwi
#> 200 Xbcm4IZbSsmhUFnSGRkpmw 2026-02-05 13:18:36 2026-02-05 13:18:36       kiwi
#> 201 HOYOLO0LSFGtkl_D876OMA 2026-02-05 13:18:41 2026-02-05 13:18:41       kiwi
#> 202 JpWIrC_fTamo4fjrCyRWaw 2026-02-05 13:20:58 2026-02-05 13:20:58       kiwi
#> 203 GSiJYzp9TEOnIrF5Qc4AEQ 2026-02-05 13:22:10 2026-02-05 13:22:10       kiwi
#> 204 BHoP8R28RhKVWi_BEtJMLQ 2026-02-05 13:22:31 2026-02-05 13:22:31       kiwi
#> 205 LrhEoszbS6GFAynDwW1p_Q 2026-02-05 14:19:33 2026-02-05 14:19:33       kiwi
#> 206 AK5i_5WoT1uUZEnHXNNLCg 2026-02-05 16:22:54 2026-02-05 16:22:54       kiwi
#> 207 BOrNqbPzRwyE4nQpf8e3Nw 2026-02-05 16:24:07 2026-02-05 16:24:07       kiwi
#> 208 fzBNhJfJRFWnjoFZMDyI6w 2026-02-05 16:24:23 2026-02-05 16:24:23       kiwi
#> 209 I4yqeAnLRq2RX_v-vmEaGA 2026-02-05 16:28:17 2026-02-05 16:28:17       kiwi
#> 210 K0GSpzpDT8CY7Xi6xWdN4Q 2026-02-05 16:28:24 2026-02-05 16:28:24       kiwi
#> 211 Hqzk8kqmTzOu6Jx8rpgr-A 2026-02-05 16:28:31 2026-02-05 16:28:31       kiwi
#> 212 EYGaNx3yT5KhgJAcITAYrQ 2026-02-05 16:33:34 2026-02-05 16:33:34       kiwi
#> 213 UD6BMflmTy6eUjeLFjIjpQ 2026-02-05 16:35:29 2026-02-05 16:35:29       kiwi
#> 214 ECcFps4KQqinV-DQPC3x4Q 2026-02-05 16:38:17 2026-02-05 16:38:17       kiwi
#> 215 cNOu8Zc0SOW0Be_dED-ljA 2026-02-05 16:39:34 2026-02-05 16:39:34       kiwi
#> 216 LEPDdA1KRZWPzo9wF6fTvA 2026-02-05 16:44:38 2026-02-05 16:44:38       kiwi
#> 217 bTOGCv8DSxaqG-su6sJKow 2026-02-05 16:50:47 2026-02-05 16:50:47       kiwi
#> 218 fOhd_7HORJS1xoW5sNct1g 2026-02-05 16:53:06 2026-02-05 16:53:06       kiwi
#> 219 d8PVGkHjS3iWHZEVjVh1vA 2026-02-05 16:53:18 2026-02-05 16:53:18       kiwi
#> 220 fOsAVHszTRWz0S6OfaUGWQ 2026-02-05 17:05:24 2026-02-05 17:05:24       kiwi
#>         nid              person       last_modified date_nominute
#> 1         1               Alice 2025-06-27 10:51:59    2022-01-06
#> 2         2                 Bob 2022-05-12 16:58:08    2022-01-03
#> 3         3               Clara 2024-08-28 01:30:00    2021-08-05
#> 4    976376 Frederick the Great 2026-02-05 17:05:23          <NA>
#> 5   7706772 Frederick the Great 2024-08-28 01:31:13          <NA>
#> 6   5105234 Frederick the Great 2024-08-28 01:32:04          <NA>
#> 7   4138893 Frederick the Great 2024-08-31 12:38:49          <NA>
#> 8   6926386 Frederick the Great 2024-08-31 12:38:52          <NA>
#> 9   6426972 Frederick the Great 2024-08-31 12:39:46          <NA>
#> 10  2384009 Frederick the Great 2024-08-31 12:40:00          <NA>
#> 11  8862077 Frederick the Great 2024-08-31 12:41:13          <NA>
#> 12  7150609 Frederick the Great 2024-08-31 12:41:22          <NA>
#> 13  9613681 Frederick the Great 2024-09-02 12:04:36          <NA>
#> 14  1634217 Frederick the Great 2024-09-02 12:06:00          <NA>
#> 15  6431695 Frederick the Great 2024-09-02 12:07:49          <NA>
#> 16  9673266 Frederick the Great 2024-09-07 09:42:06          <NA>
#> 17  4226566 Frederick the Great 2024-09-07 09:42:29          <NA>
#> 18   671154 Frederick the Great 2024-09-07 09:42:36          <NA>
#> 19  9921769 Frederick the Great 2024-09-07 10:35:19          <NA>
#> 20   173376 Frederick the Great 2024-09-07 10:36:23          <NA>
#> 21  7421177 Frederick the Great 2024-09-07 10:36:38          <NA>
#> 22  6211869 Frederick the Great 2024-09-07 14:32:14          <NA>
#> 23  9473833 Frederick the Great 2024-09-07 14:33:04          <NA>
#> 24  5196571 Frederick the Great 2024-09-07 14:35:43          <NA>
#> 25  6930475 Frederick the Great 2024-09-07 16:07:22          <NA>
#> 26   463186 Frederick the Great 2024-09-07 16:08:40          <NA>
#> 27  5047434 Frederick the Great 2024-09-07 16:09:26          <NA>
#> 28  8893869 Frederick the Great 2024-09-12 14:59:43          <NA>
#> 29  5765164 Frederick the Great 2024-09-12 16:07:35          <NA>
#> 30  4353692 Frederick the Great 2024-09-12 16:08:23          <NA>
#> 31  9891518 Frederick the Great 2024-09-12 16:08:34          <NA>
#> 32  7635402 Frederick the Great 2024-09-23 05:39:41          <NA>
#> 33  9697229 Frederick the Great 2024-09-23 10:14:08          <NA>
#> 34  8781067 Frederick the Great 2024-10-20 22:40:48          <NA>
#> 35    17840 Frederick the Great 2024-10-20 22:55:47          <NA>
#> 36  4822071 Frederick the Great 2024-10-20 23:06:48          <NA>
#> 37  5084988 Frederick the Great 2024-10-21 02:43:09          <NA>
#> 38  7358315 Frederick the Great 2024-10-21 09:27:59          <NA>
#> 39  6098978 Frederick the Great 2024-10-21 18:00:20          <NA>
#> 40  3632196 Frederick the Great 2024-10-21 18:08:44          <NA>
#> 41  7984698 Frederick the Great 2024-10-21 18:30:26          <NA>
#> 42  3310355 Frederick the Great 2024-10-21 18:36:40          <NA>
#> 43  7255713 Frederick the Great 2024-10-21 18:42:38          <NA>
#> 44  2274884 Frederick the Great 2024-10-21 18:43:47          <NA>
#> 45  5693255 Frederick the Great 2024-10-21 18:44:16          <NA>
#> 46  2906068 Frederick the Great 2024-10-21 18:45:31          <NA>
#> 47  1750299 Frederick the Great 2024-10-21 18:48:12          <NA>
#> 48  8836150 Frederick the Great 2024-10-21 18:49:18          <NA>
#> 49  2458177 Frederick the Great 2024-10-21 18:52:46          <NA>
#> 50  5772963 Frederick the Great 2024-10-21 19:04:00          <NA>
#> 51  4534791 Frederick the Great 2024-10-21 19:08:30          <NA>
#> 52  3396131 Frederick the Great 2024-10-21 19:08:45          <NA>
#> 53  4877229 Frederick the Great 2024-10-21 19:11:30          <NA>
#> 54  1434321 Frederick the Great 2024-10-21 19:22:52          <NA>
#> 55   745657 Frederick the Great 2024-10-21 19:23:43          <NA>
#> 56  6599655 Frederick the Great 2024-10-21 19:24:20          <NA>
#> 57  6948345 Frederick the Great 2024-10-21 19:24:40          <NA>
#> 58  1843960 Frederick the Great 2024-10-21 19:27:30          <NA>
#> 59  7760922 Frederick the Great 2024-10-21 19:27:39          <NA>
#> 60  2389238 Frederick the Great 2024-10-25 05:30:59          <NA>
#> 61  5069956 Frederick the Great 2024-10-25 05:31:42          <NA>
#> 62   814106 Frederick the Great 2024-10-25 05:33:25          <NA>
#> 63  7399448 Frederick the Great 2024-10-28 17:41:08          <NA>
#> 64  8960039 Frederick the Great 2024-10-28 17:41:36          <NA>
#> 65  1210310 Frederick the Great 2024-10-28 17:42:54          <NA>
#> 66  4446669 Frederick the Great 2024-10-28 18:03:03          <NA>
#> 67  1732186 Frederick the Great 2024-10-28 18:04:36          <NA>
#> 68   387879 Frederick the Great 2024-10-28 18:04:54          <NA>
#> 69  2237894 Frederick the Great 2024-10-28 19:45:22          <NA>
#> 70   967360 Frederick the Great 2024-10-28 19:50:22          <NA>
#> 71   474449 Frederick the Great 2024-10-28 19:51:23          <NA>
#> 72  3952671 Frederick the Great 2024-10-28 19:51:23          <NA>
#> 73  6051336 Frederick the Great 2024-10-28 19:51:25          <NA>
#> 74  1974284 Frederick the Great 2024-10-28 19:52:35          <NA>
#> 75  6289655 Frederick the Great 2024-10-28 19:53:03          <NA>
#> 76  7968096 Frederick the Great 2024-11-10 16:02:27          <NA>
#> 77  4835475 Frederick the Great 2024-11-10 16:04:40          <NA>
#> 78  3202270 Frederick the Great 2024-11-10 16:05:44          <NA>
#> 79  4394759 Frederick the Great 2024-11-10 17:16:51          <NA>
#> 80  7139744 Frederick the Great 2024-11-10 17:26:11          <NA>
#> 81  2592944 Frederick the Great 2024-11-10 17:27:36          <NA>
#> 82  1055186 Frederick the Great 2024-11-10 17:27:54          <NA>
#> 83  6670652 Frederick the Great 2024-11-10 17:29:18          <NA>
#> 84  4539236 Frederick the Great 2024-11-10 17:49:24          <NA>
#> 85  2745841 Frederick the Great 2024-11-10 17:50:56          <NA>
#> 86   508370 Frederick the Great 2024-11-10 17:51:13          <NA>
#> 87  7253854 Frederick the Great 2024-11-10 17:58:10          <NA>
#> 88  9837326 Frederick the Great 2024-11-10 18:04:23          <NA>
#> 89  2452371 Frederick the Great 2024-11-10 18:06:21          <NA>
#> 90  6756713 Frederick the Great 2024-11-10 18:06:31          <NA>
#> 91  9204244 Frederick the Great 2024-11-10 18:06:37          <NA>
#> 92  4165115 Frederick the Great 2024-11-29 16:40:18          <NA>
#> 93  9607939 Frederick the Great 2024-11-29 16:43:02          <NA>
#> 94  7816176 Frederick the Great 2024-11-29 16:43:16          <NA>
#> 95  5733303 Frederick the Great 2025-01-28 10:44:15          <NA>
#> 96  7339867 Frederick the Great 2025-02-01 14:47:45          <NA>
#> 97  9794800 Frederick the Great 2025-02-01 14:48:15          <NA>
#> 98  2500049 Frederick the Great 2025-02-01 14:49:19          <NA>
#> 99  5885887 Frederick the Great 2025-02-01 14:50:30          <NA>
#> 100  217887 Frederick the Great 2025-03-10 10:27:29          <NA>
#> 101 7753823 Frederick the Great 2025-03-10 11:15:28          <NA>
#> 102 2021897 Frederick the Great 2025-03-10 13:44:53          <NA>
#> 103 5776329 Frederick the Great 2025-03-16 08:53:41          <NA>
#> 104 5653568 Frederick the Great 2025-03-16 10:01:57          <NA>
#> 105 6573911 Frederick the Great 2025-03-19 15:21:06          <NA>
#> 106 6198893 Frederick the Great 2025-04-07 11:34:43          <NA>
#> 107 4115871 Frederick the Great 2025-04-07 11:36:15          <NA>
#> 108 1840592 Frederick the Great 2025-04-07 15:34:48          <NA>
#> 109 8437758 Frederick the Great 2025-04-07 15:36:18          <NA>
#> 110 1078102 Frederick the Great 2025-04-07 15:44:40          <NA>
#> 111  805986 Frederick the Great 2025-04-08 07:26:28          <NA>
#> 112  774985 Frederick the Great 2025-04-08 07:26:54          <NA>
#> 113 5703052 Frederick the Great 2025-04-08 07:29:48          <NA>
#> 114 8724900 Frederick the Great 2025-04-08 08:27:07          <NA>
#> 115 5826682 Frederick the Great 2025-04-08 08:27:24          <NA>
#> 116 7885878 Frederick the Great 2025-04-08 08:27:38          <NA>
#> 117 9031193 Frederick the Great 2025-04-25 08:10:07          <NA>
#> 118  103664 Frederick the Great 2025-04-28 14:07:32          <NA>
#> 119 7892849 Frederick the Great 2025-04-28 14:35:55          <NA>
#> 120 2215925 Frederick the Great 2025-04-28 14:38:02          <NA>
#> 121 5126185 Frederick the Great 2025-04-28 14:38:15          <NA>
#> 122  691832 Frederick the Great 2025-04-28 16:45:28          <NA>
#> 123 2865605 Frederick the Great 2025-04-28 16:46:05          <NA>
#> 124 5095725 Frederick the Great 2025-04-28 16:46:36          <NA>
#> 125 9464519 Frederick the Great 2025-04-28 22:56:12          <NA>
#> 126 8679792 Frederick the Great 2025-04-28 22:56:49          <NA>
#> 127 6437621 Frederick the Great 2025-04-28 22:57:00          <NA>
#> 128 4243417 Frederick the Great 2025-04-29 07:03:29          <NA>
#> 129  973479 Frederick the Great 2025-05-01 11:20:18          <NA>
#> 130 6085960 Frederick the Great 2025-05-01 11:21:06          <NA>
#> 131 6451946 Frederick the Great 2025-05-01 11:21:20          <NA>
#> 132 9390270 Frederick the Great 2025-06-24 17:29:49          <NA>
#> 133 3272722 Frederick the Great 2025-07-07 17:19:59          <NA>
#> 134 2142906 Frederick the Great 2025-07-07 17:21:42          <NA>
#> 135 3927292 Frederick the Great 2025-07-07 17:23:47          <NA>
#> 136 1160269 Frederick the Great 2025-07-08 12:50:21          <NA>
#> 137 5564163 Frederick the Great 2025-07-08 12:51:49          <NA>
#> 138 7850454 Frederick the Great 2025-07-08 12:52:31          <NA>
#> 139 9820216 Frederick the Great 2025-09-03 17:30:45          <NA>
#> 140 2638522 Frederick the Great 2025-09-05 22:00:41          <NA>
#> 141 3822479 Frederick the Great 2025-09-07 08:27:54          <NA>
#> 142 8401325 Frederick the Great 2025-09-07 17:47:27          <NA>
#> 143 1465510 Frederick the Great 2025-09-07 17:47:51          <NA>
#> 144 9768728 Frederick the Great 2025-09-07 17:52:52          <NA>
#> 145 6735755 Frederick the Great 2025-09-07 22:11:09          <NA>
#> 146 6583669 Frederick the Great 2025-09-07 22:12:41          <NA>
#> 147 2784993 Frederick the Great 2025-09-07 22:14:39          <NA>
#> 148 1286975 Frederick the Great 2025-09-07 22:32:03          <NA>
#> 149 1213812 Frederick the Great 2025-09-07 22:33:06          <NA>
#> 150 2712697 Frederick the Great 2025-09-07 22:36:45          <NA>
#> 151  962402 Frederick the Great 2025-09-08 08:50:59          <NA>
#> 152 4135840 Frederick the Great 2025-09-08 08:51:31          <NA>
#> 153 5407546 Frederick the Great 2025-09-08 08:51:43          <NA>
#> 154 1919924 Frederick the Great 2025-12-03 14:48:18          <NA>
#> 155 9711691 Frederick the Great 2025-12-03 14:49:08          <NA>
#> 156 3767845 Frederick the Great 2025-12-03 14:49:51          <NA>
#> 157 2827280 Frederick the Great 2025-12-03 15:06:53          <NA>
#> 158 1678804 Frederick the Great 2025-12-03 15:07:23          <NA>
#> 159 8493314 Frederick the Great 2025-12-03 15:08:54          <NA>
#> 160 8000541 Frederick the Great 2025-12-03 17:50:32          <NA>
#> 161 9162859 Frederick the Great 2025-12-03 17:51:47          <NA>
#> 162 8274129 Frederick the Great 2025-12-03 17:53:36          <NA>
#> 163 3371705 Frederick the Great 2025-12-05 10:59:42          <NA>
#> 164 3490862 Frederick the Great 2025-12-05 11:01:22          <NA>
#> 165 5310260 Frederick the Great 2025-12-05 11:02:36          <NA>
#> 166 1328540 Frederick the Great 2025-12-05 13:41:51          <NA>
#> 167 8586656 Frederick the Great 2025-12-05 13:43:01          <NA>
#> 168 4393667 Frederick the Great 2025-12-05 13:45:49          <NA>
#> 169 2851132 Frederick the Great 2025-12-08 10:02:42          <NA>
#> 170 9600526 Frederick the Great 2025-12-08 10:04:50          <NA>
#> 171 7858766 Frederick the Great 2025-12-08 10:05:19          <NA>
#> 172 2559155 Frederick the Great 2025-12-19 13:28:52          <NA>
#> 173 2327606 Frederick the Great 2025-12-19 13:32:06          <NA>
#> 174 5700750 Frederick the Great 2025-12-19 13:39:35          <NA>
#> 175  695325 Frederick the Great 2025-12-19 13:40:32          <NA>
#> 176 9632500 Frederick the Great 2025-12-19 13:41:53          <NA>
#> 177 5163700 Frederick the Great 2025-12-19 13:41:54          <NA>
#> 178 3589609 Frederick the Great 2025-12-19 13:42:23          <NA>
#> 179 3511460 Frederick the Great 2025-12-19 13:46:10          <NA>
#> 180  889575 Frederick the Great 2026-02-03 16:13:46          <NA>
#> 181  400328 Frederick the Great 2026-02-03 16:18:02          <NA>
#> 182 8223532 Frederick the Great 2026-02-03 16:18:41          <NA>
#> 183 8136198 Frederick the Great 2026-02-03 16:23:52          <NA>
#> 184 2514481 Frederick the Great 2026-02-03 16:26:13          <NA>
#> 185 7643346 Frederick the Great 2026-02-03 16:56:19          <NA>
#> 186 4781400 Frederick the Great 2026-02-03 19:03:00          <NA>
#> 187  485682 Frederick the Great 2026-02-03 19:38:52          <NA>
#> 188 2260867 Frederick the Great 2026-02-03 19:42:17          <NA>
#> 189 5405169 Frederick the Great 2026-02-03 19:55:25          <NA>
#> 190  594809 Frederick the Great 2026-02-05 08:23:22          <NA>
#> 191 1703904 Frederick the Great 2026-02-05 08:28:20          <NA>
#> 192 2640021 Frederick the Great 2026-02-05 10:21:24          <NA>
#> 193 3148335 Frederick the Great 2026-02-05 10:21:58          <NA>
#> 194 9836545 Frederick the Great 2026-02-05 10:28:23          <NA>
#> 195 4208212 Frederick the Great 2026-02-05 10:30:12          <NA>
#> 196 4416622 Frederick the Great 2026-02-05 10:32:11          <NA>
#> 197 4194983 Frederick the Great 2026-02-05 10:34:47          <NA>
#> 198 6221841 Frederick the Great 2026-02-05 10:41:51          <NA>
#> 199 2906736 Frederick the Great 2026-02-05 13:18:07          <NA>
#> 200 9634960 Frederick the Great 2026-02-05 13:18:36          <NA>
#> 201 5958093 Frederick the Great 2026-02-05 13:18:41          <NA>
#> 202  115433 Frederick the Great 2026-02-05 13:20:58          <NA>
#> 203 3512885 Frederick the Great 2026-02-05 13:22:10          <NA>
#> 204 2795463 Frederick the Great 2026-02-05 13:22:31          <NA>
#> 205 7453728 Frederick the Great 2026-02-05 14:19:33          <NA>
#> 206 3408645 Frederick the Great 2026-02-05 16:22:54          <NA>
#> 207 5172372 Frederick the Great 2026-02-05 16:24:07          <NA>
#> 208 4914163 Frederick the Great 2026-02-05 16:24:23          <NA>
#> 209 2882404 Frederick the Great 2026-02-05 16:28:17          <NA>
#> 210 3850270 Frederick the Great 2026-02-05 16:28:24          <NA>
#> 211  100693 Frederick the Great 2026-02-05 16:28:31          <NA>
#> 212 1745362 Frederick the Great 2026-02-05 16:33:34          <NA>
#> 213 5057545 Frederick the Great 2026-02-05 16:35:29          <NA>
#> 214 9147750 Frederick the Great 2026-02-05 16:38:17          <NA>
#> 215 7690774 Frederick the Great 2026-02-05 16:39:34          <NA>
#> 216 9695969 Frederick the Great 2026-02-05 16:44:38          <NA>
#> 217 3403434 Frederick the Great 2026-02-05 16:50:47          <NA>
#> 218 3402792 Frederick the Great 2026-02-05 16:53:06          <NA>
#> 219  512378 Frederick the Great 2026-02-05 16:53:18          <NA>
#> 220 6277608 Frederick the Great 2026-02-05 17:05:24          <NA>
#>            date_wminute                                        user  camid
#> 1   2022-01-12 09:30:00 8adf4f5dd661449fa6cc1f5a0b1815c0@auth.local 100001
#> 2   2022-01-03 07:56:00 c7efb8019da54923a9b04d4a74f0fde8@auth.local 100002
#> 3   2021-08-05 08:30:00 c7efb8019da54923a9b04d4a74f0fde8@auth.local 100003
#> 4                  <NA>                                         NaN 100908
#> 5                  <NA>                                         NaN 100909
#> 6                  <NA>                                         NaN 100910
#> 7                  <NA>                                         NaN 100911
#> 8                  <NA>                                         NaN 100912
#> 9                  <NA>                                         NaN 100913
#> 10                 <NA>                                         NaN 100914
#> 11                 <NA>                                         NaN 100915
#> 12                 <NA>                                         NaN 100916
#> 13                 <NA>                                         NaN 100917
#> 14                 <NA>                                         NaN 100918
#> 15                 <NA>                                         NaN 100919
#> 16                 <NA>                                         NaN 100920
#> 17                 <NA>                                         NaN 100921
#> 18                 <NA>                                         NaN 100922
#> 19                 <NA>                                         NaN 100923
#> 20                 <NA>                                         NaN 100924
#> 21                 <NA>                                         NaN 100925
#> 22                 <NA>                                         NaN 100926
#> 23                 <NA>                                         NaN 100927
#> 24                 <NA>                                         NaN 100928
#> 25                 <NA>                                         NaN 100929
#> 26                 <NA>                                         NaN 100930
#> 27                 <NA>                                         NaN 100931
#> 28                 <NA>                                         NaN 100932
#> 29                 <NA>                                         NaN 100933
#> 30                 <NA>                                         NaN 100934
#> 31                 <NA>                                         NaN 100935
#> 32                 <NA>                                         NaN 100936
#> 33                 <NA>                                         NaN 100937
#> 34                 <NA>                                         NaN 100938
#> 35                 <NA>                                         NaN 100939
#> 36                 <NA>                                         NaN 100940
#> 37                 <NA>                                         NaN 100941
#> 38                 <NA>                                         NaN 100942
#> 39                 <NA>                                         NaN 100943
#> 40                 <NA>                                         NaN 100944
#> 41                 <NA>                                         NaN 100945
#> 42                 <NA>                                         NaN 100946
#> 43                 <NA>                                         NaN 100947
#> 44                 <NA>                                         NaN 100948
#> 45                 <NA>                                         NaN 100949
#> 46                 <NA>                                         NaN 100950
#> 47                 <NA>                                         NaN 100951
#> 48                 <NA>                                         NaN 100952
#> 49                 <NA>                                         NaN 100953
#> 50                 <NA>                                         NaN 100954
#> 51                 <NA>                                         NaN 100955
#> 52                 <NA>                                         NaN 100956
#> 53                 <NA>                                         NaN 100957
#> 54                 <NA>                                         NaN 100958
#> 55                 <NA>                                         NaN 100959
#> 56                 <NA>                                         NaN 100960
#> 57                 <NA>                                         NaN 100961
#> 58                 <NA>                                         NaN 100962
#> 59                 <NA>                                         NaN 100963
#> 60                 <NA>                                         NaN 100964
#> 61                 <NA>                                         NaN 100965
#> 62                 <NA>                                         NaN 100966
#> 63                 <NA>                                         NaN 100967
#> 64                 <NA>                                         NaN 100968
#> 65                 <NA>                                         NaN 100969
#> 66                 <NA>                                         NaN 100970
#> 67                 <NA>                                         NaN 100971
#> 68                 <NA>                                         NaN 100972
#> 69                 <NA>                                         NaN 100973
#> 70                 <NA>                                         NaN 100974
#> 71                 <NA>                                         NaN 100975
#> 72                 <NA>                                         NaN 100976
#> 73                 <NA>                                         NaN 100977
#> 74                 <NA>                                         NaN 100978
#> 75                 <NA>                                         NaN 100979
#> 76                 <NA>                                         NaN 100980
#> 77                 <NA>                                         NaN 100981
#> 78                 <NA>                                         NaN 100982
#> 79                 <NA>                                         NaN 100983
#> 80                 <NA>                                         NaN 100984
#> 81                 <NA>                                         NaN 100985
#> 82                 <NA>                                         NaN 100986
#> 83                 <NA>                                         NaN 100987
#> 84                 <NA>                                         NaN 100988
#> 85                 <NA>                                         NaN 100989
#> 86                 <NA>                                         NaN 100990
#> 87                 <NA>                                         NaN 100991
#> 88                 <NA>                                         NaN 100992
#> 89                 <NA>                                         NaN 100993
#> 90                 <NA>                                         NaN 100994
#> 91                 <NA>                                         NaN 100995
#> 92                 <NA>                                         NaN 100996
#> 93                 <NA>                                         NaN 100997
#> 94                 <NA>                                         NaN 100998
#> 95                 <NA>                                         NaN 100999
#> 96                 <NA>                                         NaN 101000
#> 97                 <NA>                                         NaN 101001
#> 98                 <NA>                                         NaN 101002
#> 99                 <NA>                                         NaN 101003
#> 100                <NA>                                         NaN 101004
#> 101                <NA>                                         NaN 101005
#> 102                <NA>                                         NaN 101006
#> 103                <NA>                                         NaN 101007
#> 104                <NA>                                         NaN 101008
#> 105                <NA>                                         NaN 101009
#> 106                <NA>                                         NaN 101010
#> 107                <NA>                                         NaN 101011
#> 108                <NA>                                         NaN 101012
#> 109                <NA>                                         NaN 101013
#> 110                <NA>                                         NaN 101014
#> 111                <NA>                                         NaN 101015
#> 112                <NA>                                         NaN 101016
#> 113                <NA>                                         NaN 101017
#> 114                <NA>                                         NaN 101018
#> 115                <NA>                                         NaN 101019
#> 116                <NA>                                         NaN 101020
#> 117                <NA>                                         NaN 101021
#> 118                <NA>                                         NaN 101022
#> 119                <NA>                                         NaN 101023
#> 120                <NA>                                         NaN 101024
#> 121                <NA>                                         NaN 101025
#> 122                <NA>                                         NaN 101026
#> 123                <NA>                                         NaN 101027
#> 124                <NA>                                         NaN 101028
#> 125                <NA>                                         NaN 101029
#> 126                <NA>                                         NaN 101030
#> 127                <NA>                                         NaN 101031
#> 128                <NA>                                         NaN 101032
#> 129                <NA>                                         NaN 101033
#> 130                <NA>                                         NaN 101034
#> 131                <NA>                                         NaN 101035
#> 132                <NA>                                         NaN 101036
#> 133                <NA>                                         NaN 101037
#> 134                <NA>                                         NaN 101038
#> 135                <NA>                                         NaN 101039
#> 136                <NA>                                         NaN 101040
#> 137                <NA>                                         NaN 101041
#> 138                <NA>                                         NaN 101042
#> 139                <NA>                                         NaN 101043
#> 140                <NA>                                         NaN 101044
#> 141                <NA>                                         NaN 101045
#> 142                <NA>                                         NaN 101046
#> 143                <NA>                                         NaN 101047
#> 144                <NA>                                         NaN 101048
#> 145                <NA>                                         NaN 101049
#> 146                <NA>                                         NaN 101050
#> 147                <NA>                                         NaN 101051
#> 148                <NA>                                         NaN 101052
#> 149                <NA>                                         NaN 101053
#> 150                <NA>                                         NaN 101054
#> 151                <NA>                                         NaN 101055
#> 152                <NA>                                         NaN 101056
#> 153                <NA>                                         NaN 101057
#> 154                <NA>                                         NaN 101058
#> 155                <NA>                                         NaN 101059
#> 156                <NA>                                         NaN 101060
#> 157                <NA>                                         NaN 101061
#> 158                <NA>                                         NaN 101062
#> 159                <NA>                                         NaN 101063
#> 160                <NA>                                         NaN 101064
#> 161                <NA>                                         NaN 101065
#> 162                <NA>                                         NaN 101066
#> 163                <NA>                                         NaN 101067
#> 164                <NA>                                         NaN 101068
#> 165                <NA>                                         NaN 101069
#> 166                <NA>                                         NaN 101070
#> 167                <NA>                                         NaN 101071
#> 168                <NA>                                         NaN 101072
#> 169                <NA>                                         NaN 101073
#> 170                <NA>                                         NaN 101074
#> 171                <NA>                                         NaN 101075
#> 172                <NA>                                         NaN 101076
#> 173                <NA>                                         NaN 101077
#> 174                <NA>                                         NaN 101078
#> 175                <NA>                                         NaN 101079
#> 176                <NA>                                         NaN 101080
#> 177                <NA>                                         NaN 101081
#> 178                <NA>                                         NaN 101082
#> 179                <NA>                                         NaN 101083
#> 180                <NA>                                         NaN 101084
#> 181                <NA>                                         NaN 101085
#> 182                <NA>                                         NaN 101086
#> 183                <NA>                                         NaN 101087
#> 184                <NA>                                         NaN 101088
#> 185                <NA>                                         NaN 101089
#> 186                <NA>                                         NaN 101090
#> 187                <NA>                                         NaN 101091
#> 188                <NA>                                         NaN 101092
#> 189                <NA>                                         NaN 101093
#> 190                <NA>                                         NaN 101094
#> 191                <NA>                                         NaN 101095
#> 192                <NA>                                         NaN 101096
#> 193                <NA>                                         NaN 101097
#> 194                <NA>                                         NaN 101098
#> 195                <NA>                                         NaN 101099
#> 196                <NA>                                         NaN 101100
#> 197                <NA>                                         NaN 101101
#> 198                <NA>                                         NaN 101102
#> 199                <NA>                                         NaN 101103
#> 200                <NA>                                         NaN 101104
#> 201                <NA>                                         NaN 101105
#> 202                <NA>                                         NaN 101106
#> 203                <NA>                                         NaN 101107
#> 204                <NA>                                         NaN 101108
#> 205                <NA>                                         NaN 101109
#> 206                <NA>                                         NaN 101110
#> 207                <NA>                                         NaN 101111
#> 208                <NA>                                         NaN 101112
#> 209                <NA>                                         NaN 101113
#> 210                <NA>                                         NaN 101114
#> 211                <NA>                                         NaN 101115
#> 212                <NA>                                         NaN 101116
#> 213                <NA>                                         NaN 101117
#> 214                <NA>                                         NaN 101118
#> 215                <NA>                                         NaN 101119
#> 216                <NA>                                         NaN 101120
#> 217                <NA>                                         NaN 101121
#> 218                <NA>                                         NaN 101122
#> 219                <NA>                                         NaN 101123
#> 220                <NA>                                         NaN 101124
# }
# \donttest{
flytable_query("SELECT person, fruit_name FROM testfruit WHERE person!='Bob'")
#>                  person fruit_name
#> 1                 Alice      apple
#> 2                 Clara clementine
#> 3   Frederick the Great       kiwi
#> 4   Frederick the Great       kiwi
#> 5   Frederick the Great       kiwi
#> 6   Frederick the Great       kiwi
#> 7   Frederick the Great       kiwi
#> 8   Frederick the Great       kiwi
#> 9   Frederick the Great       kiwi
#> 10  Frederick the Great       kiwi
#> 11  Frederick the Great       kiwi
#> 12  Frederick the Great       kiwi
#> 13  Frederick the Great       kiwi
#> 14  Frederick the Great       kiwi
#> 15  Frederick the Great       kiwi
#> 16  Frederick the Great       kiwi
#> 17  Frederick the Great       kiwi
#> 18  Frederick the Great       kiwi
#> 19  Frederick the Great       kiwi
#> 20  Frederick the Great       kiwi
#> 21  Frederick the Great       kiwi
#> 22  Frederick the Great       kiwi
#> 23  Frederick the Great       kiwi
#> 24  Frederick the Great       kiwi
#> 25  Frederick the Great       kiwi
#> 26  Frederick the Great       kiwi
#> 27  Frederick the Great       kiwi
#> 28  Frederick the Great       kiwi
#> 29  Frederick the Great       kiwi
#> 30  Frederick the Great       kiwi
#> 31  Frederick the Great       kiwi
#> 32  Frederick the Great       kiwi
#> 33  Frederick the Great       kiwi
#> 34  Frederick the Great       kiwi
#> 35  Frederick the Great       kiwi
#> 36  Frederick the Great       kiwi
#> 37  Frederick the Great       kiwi
#> 38  Frederick the Great       kiwi
#> 39  Frederick the Great       kiwi
#> 40  Frederick the Great       kiwi
#> 41  Frederick the Great       kiwi
#> 42  Frederick the Great       kiwi
#> 43  Frederick the Great       kiwi
#> 44  Frederick the Great       kiwi
#> 45  Frederick the Great       kiwi
#> 46  Frederick the Great       kiwi
#> 47  Frederick the Great       kiwi
#> 48  Frederick the Great       kiwi
#> 49  Frederick the Great       kiwi
#> 50  Frederick the Great       kiwi
#> 51  Frederick the Great       kiwi
#> 52  Frederick the Great       kiwi
#> 53  Frederick the Great       kiwi
#> 54  Frederick the Great       kiwi
#> 55  Frederick the Great       kiwi
#> 56  Frederick the Great       kiwi
#> 57  Frederick the Great       kiwi
#> 58  Frederick the Great       kiwi
#> 59  Frederick the Great       kiwi
#> 60  Frederick the Great       kiwi
#> 61  Frederick the Great       kiwi
#> 62  Frederick the Great       kiwi
#> 63  Frederick the Great       kiwi
#> 64  Frederick the Great       kiwi
#> 65  Frederick the Great       kiwi
#> 66  Frederick the Great       kiwi
#> 67  Frederick the Great       kiwi
#> 68  Frederick the Great       kiwi
#> 69  Frederick the Great       kiwi
#> 70  Frederick the Great       kiwi
#> 71  Frederick the Great       kiwi
#> 72  Frederick the Great       kiwi
#> 73  Frederick the Great       kiwi
#> 74  Frederick the Great       kiwi
#> 75  Frederick the Great       kiwi
#> 76  Frederick the Great       kiwi
#> 77  Frederick the Great       kiwi
#> 78  Frederick the Great       kiwi
#> 79  Frederick the Great       kiwi
#> 80  Frederick the Great       kiwi
#> 81  Frederick the Great       kiwi
#> 82  Frederick the Great       kiwi
#> 83  Frederick the Great       kiwi
#> 84  Frederick the Great       kiwi
#> 85  Frederick the Great       kiwi
#> 86  Frederick the Great       kiwi
#> 87  Frederick the Great       kiwi
#> 88  Frederick the Great       kiwi
#> 89  Frederick the Great       kiwi
#> 90  Frederick the Great       kiwi
#> 91  Frederick the Great       kiwi
#> 92  Frederick the Great       kiwi
#> 93  Frederick the Great       kiwi
#> 94  Frederick the Great       kiwi
#> 95  Frederick the Great       kiwi
#> 96  Frederick the Great       kiwi
#> 97  Frederick the Great       kiwi
#> 98  Frederick the Great       kiwi
#> 99  Frederick the Great       kiwi
#> 100 Frederick the Great       kiwi
#> 101 Frederick the Great       kiwi
#> 102 Frederick the Great       kiwi
#> 103 Frederick the Great       kiwi
#> 104 Frederick the Great       kiwi
#> 105 Frederick the Great       kiwi
#> 106 Frederick the Great       kiwi
#> 107 Frederick the Great       kiwi
#> 108 Frederick the Great       kiwi
#> 109 Frederick the Great       kiwi
#> 110 Frederick the Great       kiwi
#> 111 Frederick the Great       kiwi
#> 112 Frederick the Great       kiwi
#> 113 Frederick the Great       kiwi
#> 114 Frederick the Great       kiwi
#> 115 Frederick the Great       kiwi
#> 116 Frederick the Great       kiwi
#> 117 Frederick the Great       kiwi
#> 118 Frederick the Great       kiwi
#> 119 Frederick the Great       kiwi
#> 120 Frederick the Great       kiwi
#> 121 Frederick the Great       kiwi
#> 122 Frederick the Great       kiwi
#> 123 Frederick the Great       kiwi
#> 124 Frederick the Great       kiwi
#> 125 Frederick the Great       kiwi
#> 126 Frederick the Great       kiwi
#> 127 Frederick the Great       kiwi
#> 128 Frederick the Great       kiwi
#> 129 Frederick the Great       kiwi
#> 130 Frederick the Great       kiwi
#> 131 Frederick the Great       kiwi
#> 132 Frederick the Great       kiwi
#> 133 Frederick the Great       kiwi
#> 134 Frederick the Great       kiwi
#> 135 Frederick the Great       kiwi
#> 136 Frederick the Great       kiwi
#> 137 Frederick the Great       kiwi
#> 138 Frederick the Great       kiwi
#> 139 Frederick the Great       kiwi
#> 140 Frederick the Great       kiwi
#> 141 Frederick the Great       kiwi
#> 142 Frederick the Great       kiwi
#> 143 Frederick the Great       kiwi
#> 144 Frederick the Great       kiwi
#> 145 Frederick the Great       kiwi
#> 146 Frederick the Great       kiwi
#> 147 Frederick the Great       kiwi
#> 148 Frederick the Great       kiwi
#> 149 Frederick the Great       kiwi
#> 150 Frederick the Great       kiwi
#> 151 Frederick the Great       kiwi
#> 152 Frederick the Great       kiwi
#> 153 Frederick the Great       kiwi
#> 154 Frederick the Great       kiwi
#> 155 Frederick the Great       kiwi
#> 156 Frederick the Great       kiwi
#> 157 Frederick the Great       kiwi
#> 158 Frederick the Great       kiwi
#> 159 Frederick the Great       kiwi
#> 160 Frederick the Great       kiwi
#> 161 Frederick the Great       kiwi
#> 162 Frederick the Great       kiwi
#> 163 Frederick the Great       kiwi
#> 164 Frederick the Great       kiwi
#> 165 Frederick the Great       kiwi
#> 166 Frederick the Great       kiwi
#> 167 Frederick the Great       kiwi
#> 168 Frederick the Great       kiwi
#> 169 Frederick the Great       kiwi
#> 170 Frederick the Great       kiwi
#> 171 Frederick the Great       kiwi
#> 172 Frederick the Great       kiwi
#> 173 Frederick the Great       kiwi
#> 174 Frederick the Great       kiwi
#> 175 Frederick the Great       kiwi
#> 176 Frederick the Great       kiwi
#> 177 Frederick the Great       kiwi
#> 178 Frederick the Great       kiwi
#> 179 Frederick the Great       kiwi
#> 180 Frederick the Great       kiwi
#> 181 Frederick the Great       kiwi
#> 182 Frederick the Great       kiwi
#> 183 Frederick the Great       kiwi
#> 184 Frederick the Great       kiwi
#> 185 Frederick the Great       kiwi
#> 186 Frederick the Great       kiwi
#> 187 Frederick the Great       kiwi
#> 188 Frederick the Great       kiwi
#> 189 Frederick the Great       kiwi
#> 190 Frederick the Great       kiwi
#> 191 Frederick the Great       kiwi
#> 192 Frederick the Great       kiwi
#> 193 Frederick the Great       kiwi
#> 194 Frederick the Great       kiwi
#> 195 Frederick the Great       kiwi
#> 196 Frederick the Great       kiwi
#> 197 Frederick the Great       kiwi
#> 198 Frederick the Great       kiwi
#> 199 Frederick the Great       kiwi
#> 200 Frederick the Great       kiwi
#> 201 Frederick the Great       kiwi
#> 202 Frederick the Great       kiwi
#> 203 Frederick the Great       kiwi
#> 204 Frederick the Great       kiwi
#> 205 Frederick the Great       kiwi
#> 206 Frederick the Great       kiwi
#> 207 Frederick the Great       kiwi
#> 208 Frederick the Great       kiwi
#> 209 Frederick the Great       kiwi
#> 210 Frederick the Great       kiwi
#> 211 Frederick the Great       kiwi
#> 212 Frederick the Great       kiwi
#> 213 Frederick the Great       kiwi
#> 214 Frederick the Great       kiwi
#> 215 Frederick the Great       kiwi
#> 216 Frederick the Great       kiwi
#> 217 Frederick the Great       kiwi
#> 218 Frederick the Great       kiwi
#> 219 Frederick the Great       kiwi
# }
if (FALSE) { # \dontrun{
flytable_query(paste("SELECT root_id, supervoxel_id FROM info limit 5"))
} # }
```
