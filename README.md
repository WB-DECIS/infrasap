
# infrasap

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the released version of infrasap from [CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("tonyfujs/infrasap")
```

## How to deploy on RstudioConnect

- Get information about your RstudioConnect account
`rsconnect::accounts(server = "{internal-server}.worldbank.org")`

- Deploy your app
`rsconnect::deployApp(appId = 831, server = "w0lxpjekins05.worldbank.org")`

