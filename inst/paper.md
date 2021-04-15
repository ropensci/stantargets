---
title: 'The stantargets R package: a workflow framework for efficient reproducible Stan-powered Bayesian data analysis pipelines'
tags:
- R
- reproducibility
- high-performance computing
- pipeline
- workflow
- Make
- Bayesian
- Stan
date: "2020"
output: pdf_document
authors:
- name: William Michael Landau
  orcid: 0000-0003-1878-3253
  email: will.landau@gmail.com
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Eli Lilly and Company
  index: 1
---

# Summary

Real-world Bayesian data analysis is a thorough process of investigation and experimentation. Statisticians iteratively refine and compare multiple models to improve inference and understand model behavior [@bayesworkflow]. Intermediate results inform downstream model-building decisions, and the final models do not always agree with the initial choices. A typical step of this empirical development process requires a computational method such as Markov chain Monte Carlo to approximate the posterior distribution of the model parameters given the data [@bda3]. Even with fast and flexible probabilistic programming languages like Stan [@stan], this computation can take several minutes or hours to complete, and successive iterations become expensive enough to obstruct the research.

The [`stantargets`](https://github.com/ropensci/stantargets) R package [@stantargets] reduces the practical burdens of developing and maintaining Bayesian data analysis workflows with Stan. It expresses the models, datasets, and inferential results as interdependent components of a formal pipeline, tracks these components for changes, and automatically reruns the affected steps in response to these changes, optionally with distributed computing on a cluster. If a step is already up to date with its upstream dependencies, [`stantargets`](https://github.com/ropensci/stantargets) automatically skips it, potentially saving hours of runtime. When the whole pipeline is up to date, the user has tangible evidence that the output matches the underlying code and data, which affirms reproducibility.

The [`stantargets`](https://github.com/ropensci/stantargets) package is an extension of [`cmdstanr`](https://github.com/stan-dev/cmdstanr) [@cmdstanr], a lightweight interface to Stan, and [`targets`](https://github.com/ropensci/targets) [@targets], a general-purpose pipeline toolkit for reproducible research and high-performance computing. [`stantargets`](https://github.com/ropensci/stantargets) builds [`targets`](https://github.com/ropensci/targets)-powered pipelines specifically tailored to Bayesian statistics with [`cmdstanr`](https://github.com/stan-dev/cmdstanr), from single-run workflows to large-scale simulation studies. Using domain knowledge, [`stantargets`](https://github.com/ropensci/stantargets) abstracts away burdensome low-level configuration details and streamlines pipeline construction, freeing Bayesian statisticians to focus less on software development and more on model development. [`stantargets`](https://github.com/ropensci/stantargets) is part of the [R Targetopia](https://wlandau.github.io/targetopia) [@targetopia], an emerging ecosystem of R packages to democratize reproducible analysis pipelines across multiple domains of Statistics and data science.

# References
