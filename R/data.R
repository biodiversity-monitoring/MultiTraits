
#' Plant Functional Traits Dataset from Ponderosa Pine Forests Flora (PFF)
#'
#' @description
#' A dataset containing functional traits for 133 plant species commonly found in
#' southwestern USA Pinus ponderosa var. scopulorum P. & C. Lawson (ponderosa pine) forests.
#'
#' @format A data frame with 137 rows and 20 variables:
#' \describe{
#'   \item{family}{Plant family}
#'   \item{species}{Plant species}
#'   \item{Height}{Plant height (cm)}
#'   \item{Leaf_area}{Leaf area (mm^2)}
#'   \item{LDMC}{Leaf dry matter content (%)}
#'   \item{SLA}{Specific leaf area (mm^2/mg)}
#'   \item{SRL}{Specific root length (m/g)}
#'   \item{SeedMass}{Seed mass (mg)}
#'   \item{FltDate}{Flowering date (Julian day)}
#'   \item{FltDur}{Flowering duration (days)}
#'   \item{k_value}{k-value (not specified)}
#'   \item{Leaf_Cmass}{Leaf carbon content (% dry mass)}
#'   \item{Leaf_Nmass}{Leaf nitrogen content (% dry mass)}
#'   \item{Leaf_CN}{Leaf carbon to nitrogen ratio}
#'   \item{Leaf_Pmass}{Leaf phosphorus content (% dry mass)}
#'   \item{Leaf_NP}{Leaf nitrogen to phosphorus ratio}
#'   \item{Leaf_CP}{Leaf carbon to phosphorus ratio}
#'   \item{Root_Cmass}{Root carbon content (% dry mass)}
#'   \item{Root_Nmass}{Root nitrogen content (% dry mass)}
#'   \item{Root_CN}{Root carbon to nitrogen ratio}
#' }
#'
#' @details
#' This dataset contains measurements of a core set of functional traits that reflect
#' aspects of each species' ability to disperse, establish, acquire water and nutrients,
#' and photosynthesize. Traits include specific leaf area (SLA), height, seed mass,
#' specific root length (SRL), leaf and fine root nitrogen concentration, leaf phosphorus
#' concentration, and leaf dry matter content (LDMC). Julian flowering date and flowering
#' duration were also obtained for each species. Leaf litter decomposition rates were
#' measured on 103 species.
#'
#' @source
#' Laughlin, D. C., Leppert, J. J., Moore, M. M., & Sieg, C. H. (2010). A multi-trait test
#' of the leaf-height-seed plant strategy scheme with 133 species from a pine forest flora.
#' Functional Ecology, 24(3), 485-700.
#'
#' @examples
#' data(PFF)
#' head(PFF)
"PFF"


#' Wetland Herbaceous Plant Traits Dataset
#'
#' A dataset containing functional traits for 46 herbaceous wetland plant species grown in an experimental garden.
#'
#' @format A data frame with 46 rows and 23 variables:
#' \describe{
#'   \item{Species}{Full scientific name of the species}
#'   \item{species}{Abbreviated species name}
#'   \item{ROH}{Root overwintering habits (P: Perennial, S: Autumn-senescing)}
#'   \item{Height}{Shoot height (cm)}
#'   \item{ShDM}{Shoot dry mass (g)}
#'   \item{Depth}{Rooting depth (cm)}
#'   \item{RBD}{Root branching density (no. cm^-1)}
#'   \item{RBL}{Root branching length (cm cm^-1)}
#'   \item{RD_AR}{Root diameter of axial roots (mm)}
#'   \item{RD_LR}{Root diameter of lateral roots (mm)}
#'   \item{RDMC_AR}{Root dry-matter content of axial roots (g g^-1)}
#'   \item{RDMC_LR}{Root dry-matter content of lateral roots (g g^-1)}
#'   \item{SRL_AR}{Specific root length of axial roots (m g^-1)}
#'   \item{SRL_LR}{Specific root length of lateral roots (m g^-1)}
#'   \item{RP_AR}{Root porosity of axial roots (mm^3 mm^-3)}
#'   \item{RP_LR}{Root porosity of lateral roots (mm^3 mm^-3)}
#'   \item{LT}{Leaf thickness (mm)}
#'   \item{LDMC}{Leaf dry-matter content (g g^-1)}
#'   \item{SLA}{Specific leaf area (m^2 kg^-1)}
#'   \item{LP}{Leaf porosity (mm^3 mm^-3)}
#'   \item{RhDMC}{Rhizome dry-matter content (g g^-1)}
#'   \item{RhP}{Rhizome porosity (mm^3 mm^-3)}
#'   \item{LSI}{Leaf senescence index (% senescence)}
#' }
#'
#' @details
#' This dataset contains measurements of 21 below- and aboveground traits for 46 herbaceous
#' wetland species grown in a common garden setting. The study employs a detailed approach
#' to trait measurement on roots, leaves, shoots, and selectively on rhizomes. Root traits
#' are classified into functional entities – axial roots, lateral roots, and the entire
#' root system – to capture the functional variation in root branching structures.
#' Measurements include leaf morphology, porosity, and phenology, as well as
#' characterization of fibrous root systems by their phenology, branching architecture,
#' morphology, and tissue porosity.
#'
#' @source
#' Ye, Z., Mu, Y., Van Duzen, S. and Ryser, P. (2024). Root and shoot phenology,
#' architecture, and organ properties: an integrated trait network among 44 herbaceous
#' wetland species. New Phytol. https://doi.org/10.1111/nph.19747
#'
#' @examples
#' data(WH)
#' head(WH)
"WH"



