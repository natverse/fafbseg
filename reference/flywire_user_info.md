# Fetch information for one or more users

Fetch information for one or more users

## Usage

``` r
flywire_user_info(
  uids,
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")
)
```

## Arguments

- uids:

  A vector of user ids or a dataframe with a `user_id` column

- datastack_name:

  defaults to the value selected by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and to "flywire_fafb_production" when that is missing. See
  <https://global.daf-apis.com/info/> for other options.

## Value

A dataframe of user information

## Examples

``` r
# \donttest{
flywire_user_info(60, datastack_name = "flywire_fafb_public")
#>   user_id affiliations                       created          name
#> 1      60              Tue, 30 Jun 2020 15:57:50 GMT Greg Jefferis
#>                       pi service_account
#> 1 Cambridge Connectomics           FALSE
# }
```
