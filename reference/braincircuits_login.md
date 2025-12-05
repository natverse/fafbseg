# Create and log into a braincircuits account

[braincircuits](https://braincircuits.io/) is a website built by Stephan
Gerhard that provides some services for FAFB flywire data. For example,
information on predictions for dense core vesicles (DCVs). These
functions assist you in registering and logging into a braincircuits
account in order to query its API. You must first log into the website
[here](https://braincircuits.io/app/login). You then run
`braincircuits_login`, which will call `braincircuits_register`. At the
moment, this function is only needed for `flywire_dcvs` and is called
under the hood there. Therefore, beyond testing that you can connect to
[braincircuits.io](https://braincircuits.io/) there is little reason to
call this function directly.

## Usage

``` r
braincircuits_register(
  open = TRUE,
  email = NULL,
  password = NULL,
  url = "https://api.braincircuits.io"
)

braincircuits_login(
  email = NULL,
  password = NULL,
  url = "https://api.braincircuits.io"
)

braincircuits_token(
  email = NULL,
  password = NULL,
  url = "https://api.braincircuits.io"
)
```

## Arguments

- open:

  logical, whether or not to open a browser window to the [login
  page](https://braincircuits.io/app/login) for
  [braincircuits.io](https://braincircuits.io/).

- email:

  the email you have registered/want to register with
  [braincircuits.io](https://braincircuits.io/).

- password:

  the password you want to set (when using `braincircuits_register`) or
  have set (when using `braincircuits_login`) for your account.

- url:

  the URL for the braincircuits API. Typically you should not need to
  change this.

## Value

A bearer access token with which to query the braincircuits API.

## See also

[`flywire_dcvs`](https://natverse.org/fafbseg/reference/flywire_dcvs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run this function:
braincircuits_login()
# If you run it for the first time, a window will open
# to the braincircuits log in page. Log in with an email.
# then when prompted in the R command line, re-give your email address and
# give a new password. You can save these in your .Renviron file to prevent having
# to do this in future.
} # }
```
