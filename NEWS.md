# MultiTraits 1.0.0 (2026-3-22)

## New Features and Major Improvements

-   **PTMN Module (Plant Trait Multilayer Networks)**: Introduced a comprehensive new module for constructing, analyzing, and visualizing plant trait multilayer networks. This module systematically integrates multilayer network theory with plant functional trait analysis and includes three core functions:

    -   **PTMN()**: Constructs phylogenetic trait multilayer networks where nodes represent traits organized into layers (e.g., organs or functional systems) and edges represent significant correlations. It supports correlation thresholding, p-value filtering, FDR correction, and optional phylogenetic correction using independent contrasts.
    -   **PTMN_metrics()**: Calculates node-level and global network metrics for PTMNs, identifying hub traits and cross-layer modules. Included metrics are interlayer degree, closeness, clustering coefficient, edge density, path length, and modular association.
    -   **PTMN_plot()**: Visualizes PTMNs with options for standard and circular layouts, customizable node and edge aesthetics, and layer/module highlighting. It clearly differentiates intralayer edges (black) from interlayer edges (red).

## Bug fixes

-   **LHS_strategy_scheme()**: Corrected an error in the strategy description mapping. The trait sequence was previously misinterpreted as a Leaf-Seed-Height order. The text descriptions in the strategy column have now been fixed to accurately reflect the standard Leaf-Height-Seed (L-H-S) framework.

# MultiTraits 0.6.0 (2025-10-01)

## New Features and Major Improvements

-   **PTN Module (Plant Trait Network)**: Functions in the trait network module have been renamed from the `TN_` prefix to `PTN_` for consistency (e.g., `PTN()`, `PTN_metrics()`). Functionality for phylogenetic correction has been added, allowing `PTN()` and `PTN_corr()` to account for species' evolutionary relatedness during analysis.

-   **CSR Module**: The module has been expanded with the `CSR_hodgson()` function, implementing an alternative CSR classification method based on Hodgson et al. (1999).

-   **NPT Module**: A discrete classification scheme has been added to the Niche Periodic Table module.

-   **Data**: A new phylogenetic tree for the PFF dataset (`PFF_tree`) has been included to support the new phylogenetic correction capabilities.

-   Other minor improvements and documentation enhancements have been made across the package.

## Bug fixes

-   **`CSR_plot()`**: Removed the dependency on the `ggtern` package (no longer maintained) and implemented plotting using ggplot2 to ensure continued compatibility and improve stability.

# MultiTraits 0.5.0 (2025-4-20)

## Bug fixes and minor improvements

-   Modify the community detection algorithm of the TN_metrics function to cluster_edge_betweenness
-   Modified TN function to support unweighted networks

# MultiTraits 0.4.0 (2025-4-16)

## Bug fixes and minor improvements

-   Modified TN_metrics function to support unweighted networks

# MultiTraits 0.3.0 (2025-2-21)

## Infrastructure

-   Added unit testing infrastructure using testthat package
-   Added test coverage for core functions

## Bug fixes and minor improvements

-   Removed deprecated vegan::summary() call in NPT function in favor of direct scores() method

# MultiTraits 0.2.0 (2024-12-21)

## Bug fixes and minor improvements

-   Fixed bug where discrete scales could not map aesthetics only consisting of `NA`s
-   Fixed spurious warnings from `sec_axis()` with `breaks = NULL`.
-   Patterns and gradients are now also enabled in `geom_sf()`.
-   The default behaviour of `resolution()` has been reverted to pre-3.5.0 behaviour. Whether mapped discrete vectors should be treated as having resolution of 1 is controlled by the new `discrete` argument.
-   Fixed bug in `guide_bins()` and `guide_coloursteps()` where discrete breaks, such as the levels produced by `cut()`, were ordered incorrectly.
-   Fixed calculation issues in CSR module
-   Resolved NaN errors in TN module
-   Optimized visualization effects across all modules (CSR, LHS, NPT, TN)
-   Removed WH dataset
-   Updated examples across package modules

# MultiTraits 0.1.0 (2024-11-26)
