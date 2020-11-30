
# stantargets <img src='man/figures/logo.png' align="right" height="139"/>

[![cran](http://www.r-pkg.org/badges/version/stantargets)](https://cran.r-project.org/package=stantargets)
[![active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/wlandau/stantargets/workflows/check/badge.svg)](https://github.com/wlandau/stantargets/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/stantargets/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://codecov.io/gh/wlandau/targets)
[![lint](https://github.com/wlandau/stantargets/workflows/lint/badge.svg)](https://github.com/wlandau/stantargets/actions?query=workflow%3Alint)

The `targets` package is a pipeline toolkit that brings together
function-oriented programming and Make-like declarative workflows for
Statistics and data science in R. The `stantargets` package provides
convenient user-side functions to create Bayesian data analysis
workflows with `targets` and Stan. These functions streamline the
implementation so users can focus on model development instead of
software development. `stantargets` invokes Stan through the `cmdstanr`
package: <https://mc-stan.org/cmdstanr>.

## Installation

Install the GitHub development version to access the latest features and
patches.

``` r
remotes::install_github("wlandau/stantargets")
```

The [`cmdstan`](https://github.com/stan-dev/cmdstan) command line
interface is also required.

``` r
cmdstanr::install_cmdstan()
```

## Documentation

The `stantargets` website at <https://wlandau.github.io/stantargets/>
has function documentation and vignettes. Prior familiarity with
[`targets`](https://github.com/wlandau/targets) and
[`cmdstanr`](https://github.com/stan-dev/cmdstanr) is highly
recommended, and you can learn more at
<https://wlandau.github.io/targets> and <https://mc-stan.org/cmdstanr/>,
respectively.

## Participation

Development is a community effort, and we welcome discussion and
contribution. By participating in this project, you agree to abide by
the [code of
conduct](https://github.com/wlandau/stantargets/blob/main/CODE_OF_CONDUCT.md)
and the [contributing
guide](https://github.com/wlandau/stantargets/blob/main/CONTRIBUTING.md).

## Citation

``` r
citation("stantargets")
#> Warning in citation("stantargets"): no date field in DESCRIPTION file of package
#> 'stantargets'
#> Warning in citation("stantargets"): could not determine year for 'stantargets'
#> from package DESCRIPTION file
#> 
#> To cite package 'stantargets' in publications use:
#> 
#>   William Michael Landau (NA). stantargets: Targets for Stan Workflows.
#>   https://wlandau.github.io/stantargets/,
#>   https://github.com/wlandau/stantargets.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {stantargets: Targets for Stan Workflows},
#>     author = {William Michael Landau},
#>     note = {https://wlandau.github.io/stantargets/, https://github.com/wlandau/stantargets},
#>   }
```
