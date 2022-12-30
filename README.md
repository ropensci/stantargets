
# stantargets <img src='man/figures/logo.png' align="right" height="139"/>

[![ropensci](https://badges.ropensci.org/430_status.svg)](https://github.com/ropensci/software-review/issues/430)
[![joss](https://joss.theoj.org/papers/10.21105/joss.03193/status.svg)](https://doi.org/10.21105/joss.03193)
[![zenodo](https://zenodo.org/badge/315447649.svg)](https://zenodo.org/badge/latestdoi/315447649)
[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
<!--
[![cran](http://www.r-pkg.org/badges/version/stantargets)](https://cran.r-project.org/package=stantargets)
-->
[![active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/ropensci/stantargets/workflows/check/badge.svg)](https://github.com/ropensci/stantargets/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/ropensci/stantargets/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/ropensci/stantargets)
[![lint](https://github.com/ropensci/stantargets/workflows/lint/badge.svg)](https://github.com/ropensci/stantargets/actions?query=workflow%3Alint)

Bayesian data analysis usually incurs long runtimes and cumbersome
custom code, and the process of prototyping and deploying custom
[Stan](https://mc-stan.org) models can become a daunting software
engineering challenge. To ease this burden, the `stantargets` R package
creates [Stan](https://mc-stan.org) pipelines that are concise,
efficient, scalable, and tailored to the needs of Bayesian
statisticians. Leveraging
[`targets`](https://docs.ropensci.org/targets/), `stantargets` pipelines
automatically parallelize the computation and skip expensive steps when
the results are already up to date. Minimal custom user-side code is
required, and there is no need to manually configure branching, so
`stantargets` is easier to use than
[`targets`](https://docs.ropensci.org/targets/) and
[`CmdStanR`](https://mc-stan.org/cmdstanr/) directly. `stantargets` can
access all of [`cmdstanr`](https://github.com/stan-dev/cmdstanr)’s major
algorithms (MCMC, variational Bayes, and optimization) and it supports
both single-fit workflows and multi-rep simulation studies.

## Prerequisites

1.  The [prerequisites of the `targets` R
    package](https://docs.ropensci.org/targets/index.html#prerequisites).
2.  Basic familiarity with
    [`targets`](https://docs.ropensci.org/targets/): watch minutes 7
    through 40 of [this video](https://youtu.be/Gqn7Xn4d5NI?t=439), then
    read [this
    chapter](https://books.ropensci.org/targets/walkthrough.html) of the
    [user manual](https://books.ropensci.org/targets/).
3.  Familiarity with Bayesian Statistics and
    [Stan](https://mc-stan.org/). Prior knowledge of
    [`cmdstanr`](https://mc-stan.org/cmdstanr/) helps.

## How to get started

Read the `stantargets`
[introduction](https://docs.ropensci.org/stantargets/articles/introduction.html)
and
[simulation](https://docs.ropensci.org/stantargets/articles/simulation.html)
vignettes, and use <https://docs.ropensci.org/stantargets/> as a
reference while constructing your own workflows. Visit
<https://github.com/wlandau/stantargets-example-validation> for an
example project based on the [simulation
vignette](https://docs.ropensci.org/stantargets/articles/simulation.html).
The example has an [RStudio Cloud
workspace](https://rstudio.cloud/project/2466069) which allows you to
run the project in a web browser.

## Example projects

| Description                                                                                                        | Link                                                |
|--------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| Validating a minimal Stan model                                                                                    | <https://github.com/wlandau/targets-stan>           |
| Using Target Markdown and `stantargets` to validate a Bayesian longitudinal model for clinical trial data analysis | <https://github.com/wlandau/rmedicine2021-pipeline> |

## Installation

Install the GitHub development version to access the latest features and
patches.

``` r
remotes::install_github("ropensci/stantargets")
```

The [CmdStan](https://github.com/stan-dev/cmdstan) command line
interface is also required.

``` r
cmdstanr::install_cmdstan()
```

If you have problems installing
[CmdStan](https://github.com/stan-dev/cmdstan), please consult the
[installation guide of
`cmdstanr`](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) and the
[installation guide of
CmdStan](https://mc-stan.org/docs/2_26/cmdstan-guide/cmdstan-installation.html).
Alternatively, the [Stan discourse](https://discourse.mc-stan.org) is a
friendly place to ask Stan experts for help.

## Usage

First, write a [`_targets.R`
file](https://books.ropensci.org/targets/walkthrough.html) that loads
your packages, defines a function to generate
[Stan](https://mc-stan.org/) data, and lists a pipeline of targets. The
target list can call target factories like
[`tar_stan_mcmc()`](https://docs.ropensci.org/stantargets/reference/tar_stan_mcmc.html)
as well as ordinary targets with
[`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html).
The following minimal example is simple enough to contain entirely
within the `_targets.R` file, but for larger projects, you may wish to
store functions in separate files as in the
[`targets-stan`](https://github.com/wlandau/targets-stan) example.

``` r
# _targets.R
library(targets)
library(stantargets)

generate_data <- function() {
  true_beta <- stats::rnorm(n = 1, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- stats::rnorm(n, x * true_beta, 1)
  list(n = n, x = x, y = y, true_beta = true_beta)
}

list(
  tar_stan_mcmc(
    name = example,
    stan_files = "x.stan",
    data = generate_data()
  )
)
```

Run
[`tar_visnetwork()`](https://docs.ropensci.org/targets/reference/tar_visnetwork.html)
to check `_targets.R` for correctness, then call
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
to run the pipeline. Access the results using
[`tar_read()`](https://docs.ropensci.org/targets/reference/tar_read.html),
e.g. `tar_read(example_summary_x)`. Visit the [introductory
vignette](https://docs.ropensci.org/stantargets/articles/introduction.html)
to read more about this example.

## How it works behind the scenes

`stantargets` supports specialized [target
factories](https://ropensci.org/blog/2021/02/03/targets/#target-factories)
that create ensembles of [target
objects](https://docs.ropensci.org/targets/reference/tar_target.html)
for [`cmdstanr`](https://github.com/stan-dev/cmdstanr) workflows. These
[target
factories](https://ropensci.org/blog/2021/02/03/targets/#target-factories)
abstract away the details of
[`targets`](https://docs.ropensci.org/targets/) and
[`cmdstanr`](https://github.com/stan-dev/cmdstanr) and make both
packages easier to use. For details, please read the [introductory
vignette](https://docs.ropensci.org/stantargets/articles/introduction.html).

## Help

Please first read the [help
guide](https://books.ropensci.org/targets/help.html) to learn how best
to ask for help.

If you have trouble using `stantargets`, you can ask for help in the
[GitHub discussions
forum](https://github.com/ropensci/stantargets/discussions/categories/help).
Because the purpose of `stantargets` is to combine
[`targets`](https://docs.ropensci.org/targets/) and
[`cmdstanr`](https://github.com/stan-dev/cmdstanr), your issue may have
something to do with one of the latter two packages, a [dependency of
`targets`](https://github.com/ropensci/targets/blob/4e3ef2a6c986f558a25e544416f480fc01236b6b/DESCRIPTION#L49-L88),
or [Stan](https://mc-stan.org) itself. When you troubleshoot, peel back
as many layers as possible to isolate the problem. For example, if the
issue comes from [`cmdstanr`](https://github.com/stan-dev/cmdstanr),
create a [reproducible example](https://reprex.tidyverse.org) that
directly invokes [`cmdstanr`](https://github.com/stan-dev/cmdstanr)
without invoking `stantargets`. The GitHub discussion and issue forums
of those packages, as well as the [Stan
discourse](https://discourse.mc-stan.org), are great resources.

## Participation

Development is a community effort, and we welcome discussion and
contribution. Please note that this package is released with a
[Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Citation

``` r
citation("stantargets")
#> 
#> To cite stantargets in publications use:
#> 
#>   Landau, W. M., (2021). The stantargets R package: a workflow
#>   framework for efficient reproducible Stan-powered Bayesian data
#>   analysis pipelines. Journal of Open Source Software, 6(60), 3193,
#>   https://doi.org/10.21105/joss.03193
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {The stantargets {R} package: a workflow framework for efficient reproducible {S}tan-powered {B}ayesian data analysis pipelines},
#>     author = {William Michael Landau},
#>     journal = {Journal of Open Source Software},
#>     year = {2021},
#>     volume = {6},
#>     number = {60},
#>     pages = {3193},
#>     url = {https://doi.org/10.21105/joss.03193},
#>   }
```
