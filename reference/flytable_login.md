# Low level functions to access the flytable metadata service

`flytable_login` uses your flytable user name and email to log into the
service.

`flytable_set_token` will obtain and store a permanent seatable
user-level API token.

`flytable_base` returns a `base` object (equivalent to a mysql database)
which allows you to access one or more tables, logging in to the service
if necessary. The returned base object give you full access to the
Python
[`Base`](https://seatable.github.io/seatable-scripts/python/base/) API
allowing a range of row/column manipulations.

`flytable_alltables` lists all tables across all flytables bases.

`flytable_columns` returns the name and type of all regular columns in a
base as well as the default R type. Private columns such as `_id` are
not included.

## Usage

``` r
flytable_login(
  url = getOption("fafbseg.flytable.url", "https://flytable.mrc-lmb.cam.ac.uk/"),
  token = Sys.getenv("FLYTABLE_TOKEN", unset = NA_character_)
)

flytable_set_token(
  user,
  pwd,
  url = getOption("fafbseg.flytable.url", "https://flytable.mrc-lmb.cam.ac.uk/")
)

flytable_base(
  table = NULL,
  base_name = NULL,
  workspace_id = NULL,
  url = getOption("fafbseg.flytable.url", "https://flytable.mrc-lmb.cam.ac.uk/"),
  cached = TRUE
)

flytable_alltables(ac = NULL, cached = TRUE)

flytable_columns(table, base = NULL, cached = TRUE)
```

## Arguments

- url:

  Optional URL to the server

- token:

  normally retrieved from `FLYTABLE_TOKEN` environment variable.

- user, pwd:

  flytable user and password used by `flytable_set_token` to obtain a
  token

- table:

  Character vector specifying a table foe which you want a `base`
  object.

- base_name:

  Character vector specifying the `base`

- workspace_id:

  A numeric id specifying the workspace. Advanced use only since we can
  normally figure this out from `base_name`.

- cached:

  Whether to use a cached version of the response if available. Set to
  `FALSE` if you know tables have been added or renamed during your
  session.

- ac:

  Optional account object returned by flytables_login

- base:

  Optional character vector naming a seatable base (recommended) or a
  `Base` object returned by `flytable_base` (expert use). The default
  value of `NULL` will rely on the `table` so long as it is unique
  across the flytable server.

## Value

For `flytable_login`, a Python
[`Account`](https://seatable.github.io/seatable-scripts/python/account/)
object from the seatable api as wrapped by reticulate.

For `flytable_base`, a Python
[`Base`](https://seatable.github.io/seatable-scripts/python/base/)
object from the seatable api as wrapped by reticulate.

A `data.frame` containing the `base_name`, `workspace_id`, table `name`
and table `_id`.

`flytable_columns` a data.frame containing columns `name`, `type` and
`rtype`

## Details

Besides initial setup (next paragraph), you should not need to use these
lower level functions directly. Instead we recommend higher level
functions such as
[`flytable_query`](https://natverse.org/fafbseg/reference/flytable-queries.md).

In order to start using flytable, you must get an API token. There
doesn't seem to be a convenient way to do this from the seatable web
interface but you can get one by calling `flytable_set_token` with your
flytable user and password. This should be a once only step. Thereafter
you should have a `FLYTABLE_TOKEN` environment variable set in your
.Renviron file.

`flytable_base` will use your flytable API token to log into the
service.

## See also

Other flytable:
[`flytable-queries`](https://natverse.org/fafbseg/reference/flytable-queries.md),
[`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_select_options()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flytable_login()
} # }
if (FALSE) { # \dontrun{
flytable_set_token(user='xxx@gmail.com', pwd='yyy')
} # }
if (FALSE) { # \dontrun{
hemilineages=flytable_base(base_name='hemilineages')
# equivalent, but simpler since you only have to remember the table name
hemilineages=flytable_base('fafb_hemilineages_survey')
} # }

# \donttest{
flytable_alltables()
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named): requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714496380>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
# }
# \donttest{
flytable_columns("info")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named): requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714497820>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
# }
```
