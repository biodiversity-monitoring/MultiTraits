#' Create a Ternary Plot for CSR Plant Ecological Strategies
#'
#' This function creates a ternary plot to visualize plant ecological strategies
#' based on the CSR (Competitor-Stress tolerator-Ruderal) framework developed by
#' Grime (1974). The plot displays the relative proportions of C, S, and R
#' strategies for each species or sample.
#'
#' @param data A data frame containing CSR strategy data. Must include columns:
#'   \describe{
#'     \item{C}{Numeric vector of Competitor strategy values (0-100)}
#'     \item{S}{Numeric vector of Stress-tolerator strategy values (0-100)}
#'     \item{R}{Numeric vector of Ruderal strategy values (0-100)}
#'     \item{type}{Character vector indicating the CSR strategy type/classification}
#'   }
#' @param point_size Numeric value specifying the size of points in the plot.
#'   Default is 3.
#' @param point_shape Numeric value specifying the shape of points in the plot.
#'   Default is 21 (filled circle with border).
#' @param expand_margin Numeric value to expand the plot margins beyond the
#'   ternary triangle boundaries. Default is 1.
#' @param custom_colors Named character vector specifying custom colors for each
#'   CSR strategy type. Default includes 19 predefined colors for all possible
#'   CSR combinations.
#'
#' @return A ggplot object representing a ternary plot with:
#'   \itemize{
#'     \item Points colored by CSR strategy type
#'     \item Ternary coordinate system with C, S, R axes
#'     \item Legend showing strategy types and their colors
#'     \item Grid lines and arrows for better visualization
#'   }
#'
#' @details
#' The CSR strategy framework classifies plants into three primary functional
#' types based on their ecological strategies:
#' \describe{
#'   \item{C (Competitors)}{Species adapted to productive, low-stress environments}
#'   \item{S (Stress-tolerators)}{Species adapted to unproductive, high-stress environments}
#'   \item{R (Ruderals)}{Species adapted to productive, high-disturbance environments}
#' }
#'
#' The ternary plot allows visualization of the relative contribution of each
#' strategy, where each point represents a species positioned according to its
#' C, S, and R values (which sum to 100%).
#'
#' @seealso
#' \code{\link{CSR}} or \code{\link{CSR_hodgson}}for calculating CSR strategies from plant functional traits
#'
#' @importFrom ggtern ggtern geom_mask theme_rgbw limit_tern theme_showarrows theme_showgrid
#' @importFrom ggplot2 aes geom_point labs theme element_rect element_text
#' scale_fill_manual unit margin guides guide_legend
#'
#' @references
#' 1. Grime, J.P. (1974). Vegetation classification by reference to strategies. Nature, 250, 26–31.
#' 2. Pierce, S., Negreiros, D., Cerabolini, B.E.L., Kattge, J., Díaz, S., et al. (2017). A global method for calculating plant CSR ecological strategies applied across biomes world-wide. Functional Ecology, 31: 444-457.
#' \url{https://doi.org/10.1111/1365-2435.12722}
#' @examples
#' data(PFF)
#' head(PFF)
#' traits <- data.frame(LA=PFF$Leaf_area, LDMC=PFF$LDMC, SLA=PFF$SLA)
#' head(traits)
#' result <- CSR(data = traits)
#' head(result)
#' CSR_plot(data=result)
#'
#' @export
CSR_plot <- function(
    data,
    point_size = 3,
    point_shape = 21,
    expand_margin = 1,
    custom_colors = c(
      "C" = "#E60D0D",
      "C/CR" = "#BA0D3B",
      "C/CS" = "#BA3B0D",
      "CR" = "#7A0D7A",
      "C/CSR" = "#8A3B3B",
      "CS" = "#7A7A0D",
      "CR/CSR" = "#6B2B6B",
      "CS/CSR" = "#6B6B2B",
      "R/CR" = "#3B0DBA",
      "CSR" = "#545454",
      "S/CS" = "#3BBA0D",
      "R/CSR" = "#3B3B8A",
      "S/CSR" = "#3B8A3B",
      "R" = "#0D0DE6",
      "SR/CSR" = "#2B6B6B",
      "S" = "#0DE60D",
      "R/SR" = "#0D3BBA",
      "S/SR" = "#0DBA3B",
      "SR" = "#0D7A7A")
){
  p1 <- ggtern::ggtern(data=data, aes(x=.data$R, y=.data$C, z=.data$S, fill=.data$type)) +
    ggtern::limit_tern(T=expand_margin, L=expand_margin, R=expand_margin) +
    ggtern::geom_mask() +
    ggplot2::geom_point(size=point_size, shape=point_shape, color="black") +
    ggtern::theme_rgbw() +
    ggtern::theme_clockwise() +
    scale_fill_manual(values = custom_colors) +
    ggplot2::labs(x="R (%)", y="C (%)", z="S (%)", fill="Types") +
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
