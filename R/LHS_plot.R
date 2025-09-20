#' Create a Ternary Plot for LHS Plant Strategy Analysis
#'
#' This function creates a ternary plot visualization for the Leaf-Height-Seed (LHS)
#' plant ecology strategy scheme. The plot displays species according to their relative
#' positions on three fundamental plant strategy axes: specific leaf area (L), canopy
#' height (H), and seed mass (S).
#'
#' @param data A data frame containing the LHS analysis results with columns:
#'   \describe{
#'     \item{L}{Numeric. Leaf area proportion (percentage)}
#'     \item{H}{Numeric. Height proportion (percentage)}
#'     \item{S}{Numeric. Seed mass proportion (percentage)}
#'     \item{type}{Character. Strategy classification (e.g., "L", "H", "S", "LH", "LS", "HS", "LHS", etc.)}
#'   }
#' @param point_size Numeric. Size of points in the plot. Default is 3.
#' @param point_shape Numeric. Shape of points following ggplot2 conventions.
#'   Default is 21 (filled circles with borders).
#' @param expand_margin Numeric. Value to expand the ternary plot margins.
#'   Default is 1.
#' @param custom_colors Named character vector. Colors for different LHS strategy
#'   types. Should include colors for all 19 possible strategy combinations:
#'   "L", "L/LS", "L/LH", "LS", "L/LHS", "LH", "LS/LHS", "LH/LHS", "S/LS",
#'   "LHS", "H/LH", "S/LHS", "H/LHS", "S", "HS/LHS", "H", "S/HS", "H/HS", "HS".
#'
#' @return A ggplot object representing the ternary plot visualization of LHS
#'   plant strategies. The plot includes:
#'   \itemize{
#'     \item Ternary coordinate system with S, L, and H axes
#'     \item Color-coded points representing different strategy types
#'     \item Legend showing strategy classifications
#'     \item Grid lines and axis arrows for reference
#'   }
#'
#' @details
#' The LHS scheme represents plant strategies in a three-dimensional space where
#' each axis reflects fundamental trade-offs controlling plant strategies:
#'
#' \describe{
#'   \item{Specific Leaf Area (SLA)}{Light-capturing area deployed per unit dry mass,
#'     reflecting the trade-off between rapid resource capture and leaf longevity}
#'   \item{Height}{Canopy height at maturity, representing the trade-off between
#'     access to light and structural investment costs}
#'   \item{Seed Mass}{Reflecting the trade-off between seed output per unit
#'     reproductive effort and seedling survival capacity}
#' }
#'
#' The function creates a ternary plot using the \code{ggtern} package, where
#' species are positioned according to their relative emphasis on each of the
#' three strategy dimensions. Different colors represent different strategy types
#' based on which axes are most prominent for each species.
#'
#' The legend automatically adjusts to display in one or two columns depending on
#' the number of strategy types present in the data (single column for ≤10 types,
#' two columns for >10 types).
#
#' @seealso
#' \code{\link{LHS}} for performing LHS analysis on raw trait data.
#'
#' @references
#' 1. Westoby, M. (1998). A leaf-height-seed (LHS) plant ecology strategy scheme. Plant and Soil, 199, 213–227.
#' \url{https://doi.org/10.1023/A:1004327224729}
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme element_rect element_text margin unit scale_fill_manual guides guide_legend
#' @importFrom ggtern ggtern limit_tern geom_mask theme_rgbw theme_showarrows theme_showgrid
#' @importFrom rlang .data
#'
#' @examples
#' data(PFF)
#' pff <- PFF[, c("SLA", "Height", "SeedMass")]
#' result <- LHS(pff)
#' LHS_plot(result)
#'
#' @export
LHS_plot <- function(
    data,
    point_size = 3,
    point_shape = 21,
    expand_margin = 1,
    custom_colors = c(
      "L" = "#E60D0D",
      "L/LS" = "#BA0D3B",
      "L/LH" = "#BA3B0D",
      "LS" = "#7A0D7A",
      "L/LHS" = "#8A3B3B",
      "LH" = "#7A7A0D",
      "LS/LHS" = "#6B2B6B",
      "LH/LHS" = "#6B6B2B",
      "S/LS" = "#3B0DBA",
      "LHS" = "#545454",
      "H/LH" = "#3BBA0D",
      "S/LHS" = "#3B3B8A",
      "H/LHS" = "#3B8A3B",
      "S" = "#0D0DE6",
      "HS/LHS" = "#2B6B6B",
      "H" = "#0DE60D",
      "S/HS" = "#0D3BBA",
      "H/HS" = "#0DBA3B",
      "HS" = "#0D7A7A")
){
  p1 <- ggtern::ggtern(data=data, aes(x=.data$S, y=.data$L, z=.data$H, fill=.data$type)) +
    ggtern::limit_tern(T=expand_margin, L=expand_margin, R=expand_margin) +
    ggtern::geom_mask() +
    ggplot2::geom_point(size=point_size, shape=point_shape, color="black") +
    ggtern::theme_rgbw() +
    ggtern::theme_clockwise() +
    scale_fill_manual(values = custom_colors) +
    ggplot2::labs(x="S (%)", y="L (%)", z="H (%)", fill="Types") +
    # Theme Setting
    ggplot2::theme(
      # Legend position and border settings
      legend.position = "right",
      legend.background = element_rect(color="black", linewidth=0.5),
      legend.margin = margin(4, 4, 4, 4),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.size = unit(0.4, "cm"),
      # text setting
      text = element_text(size=12, family="serif"),
      legend.text = element_text(size=8),
      legend.title = element_text(size=9, face="bold"),
      # Legend Spacing Settings
      legend.spacing.y = unit(0.2, "cm"),
      # Maintain legend as single or double column
      legend.box = "vertical"
    ) +
    # Automatic adjustment of the number of columns according to the number of legend items
    guides(fill = guide_legend(
      ncol = ifelse(length(unique(data$type)) > 10, 2, 1),
      byrow = TRUE
    )) +
    ggtern::theme_showarrows() +
    ggtern::theme_showgrid()
  return(p1)
}
