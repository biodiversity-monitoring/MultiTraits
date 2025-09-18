#' Calculate Leaf-Height-Seed (LHS) Plant Ecology Strategy Scheme
#'
#' This function implements the LHS (Leaf-Height-Seed) plant ecology strategy scheme
#' proposed by Westoby (1998). The LHS scheme uses three fundamental plant traits
#' to characterize plant strategies: specific leaf area (SLA), plant height at maturity,
#' and seed mass. These three axes reflect well-established trade-offs that have
#' substantial consequences for how species cope with their physical environment
#' and biotic interactions.
#'
#' @description
#' The LHS scheme provides a quantitative framework for positioning any vascular
#' land plant species within a three-dimensional strategy space without requiring
#' time-consuming measurements of metabolic rates or field performance relative
#' to other species. All three axes are log-scaled, and the strategy of a species
#' is described by its position in the volume formed by the three axes.
#'
#' The three traits represent fundamental trade-offs:
#' \itemize{
#'   \item \strong{SLA (Specific Leaf Area)}: Light-capturing area deployed per
#'         dry mass allocated. Reflects variation in responsiveness to opportunities
#'         for rapid growth.
#'   \item \strong{Height}: Canopy height at maturity. Related to competitive ability
#'         for light and coping with disturbance duration.
#'   \item \strong{Seed Mass}: Reflects dispersal capabilities and seedling
#'         establishment strategies, expressing separate aspects of coping with disturbance.
#' }
#'
#' @param data A data frame containing plant trait measurements with required columns:
#'   \itemize{
#'     \item \code{SLA}: Specific leaf area (area per unit dry mass of mature leaves)
#'     \item \code{Height}: Plant height at maturity (canopy height)
#'     \item \code{SeedMass}: Seed mass
#'   }
#'   All values must be positive numbers. NA values are not allowed.
#'
#' @return A data frame with the original data plus additional columns:
#'   \itemize{
#'     \item \code{L}: Percentage contribution of leaf strategy (0-100)
#'     \item \code{H}: Percentage contribution of height strategy (0-100)
#'     \item \code{S}: Percentage contribution of seed strategy (0-100)
#'     \item \code{type}: Classified LHS strategy type based on dominant traits
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Log-transforms all three trait values to handle the wide range of variation
#'   \item Applies translation method to ensure all log-transformed values are positive
#'   \item Normalizes values to percentages that sum to 100 for each species
#'   \item Classifies each species into one of 19 predefined LHS strategy types
#'         based on the closest match in the three-dimensional space
#' }
#'
#' The LHS scheme allows for worldwide comparison of plant strategies and facilitates
#' meta-analysis across species from different regions, as it provides an explicit
#' protocol for determining a species' position without relying on local-context
#' information.
#'
#' @note
#' This implementation assumes that differences in these traits between species
#' are ecologically meaningful, while recognizing that the traits may show some
#' environmental modulation and genetic variation within species.
#'
#' @references
#' 1. Westoby, M. (1998). A leaf-height-seed (LHS) plant ecology strategy scheme. Plant and Soil, 199, 213–227.
#' \url{https://doi.org/10.1023/A:1004327224729}
#'
#' @examples
#' data(PFF)
#' pff <- PFF[, c("SLA", "Height", "SeedMass")]
#' result <- LHS(pff)
#' head(result)
#'
#' @export
LHS <- function(data) {
  # Check input data
  if (!is.data.frame(data)) {
    stop("The input must be a dataframe")
  }
  if(!all(c("SLA", "Height", "SeedMass") %in% names(data))) {
    stop("Input data must contain columns: SLA, Height, SeedMass")
  }
  # Check for NA values in required columns
  required_columns <- c("SLA", "Height", "SeedMass")
  na_check <- sapply(data[required_columns], function(x) any(is.na(x)))
  if (any(na_check)) {
    na_cols <- names(na_check)[na_check]
    stop(paste("NA values found in column(s):",
               paste(na_cols, collapse = ", "),
               ". Please remove or handle NA values before processing."))
  }
  if (any(data$SLA <= 0 | data$Height <= 0 | data$SeedMass <= 0, na.rm = TRUE)) {
    stop("SLA, Height and SeedMass must be positive numbers to calculate the LHS percentage")
  }
  # Log transformation
  log_sla       <- log(data$SLA)
  log_height    <- log(data$Height)
  log_seedmass  <- log(data$SeedMass)
  # Translation Method: Set Minimum Value to Zero
  min_val <- min(c(log_sla, log_height, log_seedmass), na.rm = TRUE)
  log_sla_pos      <- log_sla      + abs(min_val)
  log_height_pos   <- log_height   + abs(min_val)
  log_seedmass_pos <- log_seedmass + abs(min_val)
  # Normalize to percentage
  sum_pos <- log_sla_pos + log_height_pos + log_seedmass_pos
  data$L <- (log_sla_pos / sum_pos) * 100
  data$H <- (log_height_pos / sum_pos) * 100
  data$S <- (log_seedmass_pos / sum_pos) * 100
  # Reference LHS strategy types
  L_values <- c(90, 73, 73, 48, 54, 48, 42, 42, 23, 33, 23, 23, 23, 5, 17, 5, 5, 5, 5)
  H_values <- c(5, 5, 23, 5, 23, 48, 17, 42, 5, 33, 73, 23, 54, 5, 42, 90, 23, 73, 48)
  S_values <- c(5, 23, 5, 48, 23, 5, 42, 17, 73, 33, 5, 54, 23, 90, 42, 5, 73, 23, 48)
  LHS_types <- c("L", "L/LS", "L/LH", "LS", "L/LHS", "LH", "LS/LHS", "LH/LHS",
                 "S/LS", "LHS", "H/LH", "S/LHS", "H/LHS", "S", "HS/LHS",
                 "H", "S/HS", "H/HS", "HS")
  # Determine closest LHS type
  data$type <- sapply(1:nrow(data), function(i) {
    variances <- (data$L[i] - L_values)^2 +
      (data$H[i] - H_values)^2 +
      (data$S[i] - S_values)^2
    min_variance_index <- which.min(variances)
    return(LHS_types[min_variance_index])
  })
  return(data)
}
