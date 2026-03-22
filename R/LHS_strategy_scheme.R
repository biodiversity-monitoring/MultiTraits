#' Create a table of Leaf-Height-Seed (LHS) strategy types
#'
#' This function generates a data frame containing different plant growth strategies
#' based on the Leaf-Height-Seed (LHS) scheme. Each strategy is described by a
#' combination of traits and their corresponding ecological interpretation.
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{type}{Character vector of LHS strategy combinations (e.g., "L-L-L", "L-L-S", etc.)}
#'     \item{strategy}{Character vector describing the ecological strategy for each type}
#'   }
#'
#'
#' @references
#' 1. Westoby, M. (1998). A leaf-height-seed (LHS) plant ecology strategy scheme.
#' Plant and Soil, 199, 213–227.
#' 2. Yang, J., Wang, Z., Zheng, Y., & Pan, Y. (2022). Shifts in plant ecological strategies
#' in remnant forest patches along urbanization gradients. Forest Ecology and Management, 524, 120540.
#'
#' @examples
#' LHS_strategy_scheme()
#'
#' @export
LHS_strategy_scheme <- function(){
  LHS_strategy_table <- data.frame(
    type=c( "L-L-L","L-L-S",  "L-S-L","L-S-S", "S-L-L","S-L-S","S-S-L", "S-S-S"),
    strategy=c(
      "Rapid growth, strong competitiveness and strong survivability",
      "Rapid growth, strong competitiveness and long-distance dispersal",
      "Rapid growth, weak competitiveness and strong survivability",
      "Rapid growth, weak competitiveness and long-distance dispersal",
      "Slow growth, strong competitiveness and strong survivability",
      "Slow growth, strong competitiveness and long-distance dispersal",
      "Slow growth, weak competitiveness and strong survivability",
      "Slow growth, weak competitiveness and long-distance dispersal")
  )
  return(LHS_strategy_table)
}
