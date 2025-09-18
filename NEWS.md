
# MultiTraits 0.6.0 (2025-9)







# MultiTraits 0.5.0 (2025-4-20)

## Bug fixes and minor improvements

* Modify the community detection algorithm of the TN_metrics function to cluster_edge_betweenness
* Modified TN function to support unweighted networks


# MultiTraits 0.4.0 (2025-4-16)

## Bug fixes and minor improvements

* Modified TN_metrics function to support unweighted networks




# MultiTraits 0.3.0 (2025-2-21)

## Infrastructure

* Added unit testing infrastructure using testthat package
* Added test coverage for core functions

## Bug fixes and minor improvements

* Removed deprecated vegan::summary() call in NPT function in favor of direct scores() method




# MultiTraits 0.2.0 (2024-12-21)

## Bug fixes and minor improvements

* Fixed bug where discrete scales could not map aesthetics only consisting of
  `NA`s
* Fixed spurious warnings from `sec_axis()` with `breaks = NULL`.
* Patterns and gradients are now also enabled in `geom_sf()`.
* The default behaviour of `resolution()` has been reverted to pre-3.5.0 
  behaviour. Whether mapped discrete vectors should be treated as having 
  resolution of 1 is controlled by the new `discrete` argument.
* Fixed bug in `guide_bins()` and `guide_coloursteps()` where discrete breaks,
  such as the levels produced by `cut()`, were ordered incorrectly.
* Fixed calculation issues in CSR module
* Resolved NaN errors in TN module
* Optimized visualization effects across all modules (CSR, LHS, NPT, TN)
* Removed WH dataset
* Updated examples across package modules




# MultiTraits 0.1.0 (2024-11-26)



