
library(ggtern)

#' Calculate CSR (Competition-Stress-Ruderal) Strategy
#'
#' This function calculates the CSR strategy based on leaf traits.
#'
#' @param LA Leaf area in mm^2
#' @param LDMC Leaf dry matter content in %
#' @param SLA Specific leaf area in mm^2/mg
#'
#' @return A list containing:
#' * `C`: Proportion of competition strategy
#' * `S`: Proportion of stress-tolerance strategy
#' * `R`: Proportion of ruderal strategy
#' * `type`: Type of CSR strategy
#'
#' @references
#' 1. Grime, J.P. (1974). Vegetation classification by reference to strategies. Nature, 250, 26–31.
#' 2. Pierce, S., Negreiros, D., Cerabolini, B.E.L., Kattge, J., Díaz, S., et al. (2017). A global method for calculating plant CSR ecological strategies applied across biomes world-wide. Funct Ecol, 31: 444-457.
#'
#' @examples
#' Strate_CSR(LA = 369615.7, LDMC = 25.2, SLA = 17.4)
#'
#' @export
Strate_CSR <- function(LA, LDMC, SLA) {
  # transformations
  sqrt_LA <- sqrt(LA / 894205) * 100
  logit_LDMC <- log((LDMC / 100) / (1 - (LDMC / 100)))
  log_SLA <- log(SLA)

  # Regression against calibration PCA
  pca2_coord_c <- -0.8678 + 1.6464 * sqrt_LA
  pca1_coord_s <- 1.3369 + 0.000010019 * (1 - exp(-0.0000000000022303 * logit_LDMC)) + 4.5835 * (1 - exp(-0.2328 * logit_LDMC))
  pca1_coord_r <- -57.5924 + 62.6802 * exp(-0.0288 * log_SLA)
  min_c <- 0
  min_s <- -0.756451214853076
  min_r <- -11.3467682227961
  Neg_outlier_correction_C <- ifelse(pca2_coord_c < min_c, min_c, pca2_coord_c)
  Neg_outlier_correction_S <- ifelse(pca1_coord_s < min_s, min_s, pca1_coord_s)
  Neg_outlier_correction_R <- ifelse(pca1_coord_r < min_r, min_r, pca1_coord_r)
  max_c <- 57.3756711966087
  max_s <- 5.79158377609218
  max_r <- 1.10795515716546
  Pos_outlier_correction_C <- ifelse(Neg_outlier_correction_C > max_c, max_c, Neg_outlier_correction_C)
  Pos_outlier_correction_S <- ifelse(Neg_outlier_correction_S > max_s, max_s, Neg_outlier_correction_S)
  Pos_outlier_correction_R <- ifelse(Neg_outlier_correction_R > max_r, max_r, Neg_outlier_correction_R)

  # +ve translation (values ranges spanning zero are shifted to all become positive)
  C_positive_translation <- Pos_outlier_correction_C + abs(min_c)
  S_positive_translation <- Pos_outlier_correction_S + abs(min_s)
  R_positive_translation <- Pos_outlier_correction_R + abs(min_r)
  Range_PCA2_C_positive_translation <- max_c + abs(min_c)
  Range_PCA1_S_positive_translation <- max_s + abs(min_s)
  Range_PCA1_R_positive_translation <- max_r + abs(min_r)
  Proportion_of_total_variability_C <- C_positive_translation/Range_PCA2_C_positive_translation*100
  Proportion_of_total_variability_S <- S_positive_translation/Range_PCA1_S_positive_translation*100
  Proportion_of_total_variability_R <- 100-(R_positive_translation/Range_PCA1_R_positive_translation*100)
  Percentage_Conversion_Coefficient <- 100/sum(c(Proportion_of_total_variability_C, Proportion_of_total_variability_S, Proportion_of_total_variability_R))
  Percentage_Conversion_Coefficient

  # Tertiary CSR strategy
  C_values <- c(90, 73, 73, 48, 54, 48, 42, 42, 23, 33, 23, 23, 23, 5, 17, 5, 5, 5, 5)
  S_values <- c(5, 5, 23, 5, 23, 48, 17, 42, 5, 33, 73, 23, 54, 5, 42, 90, 23, 73, 48)
  R_values <- c(5, 23, 5, 48, 23, 5, 42, 17, 73, 33, 5, 54, 23, 90, 42, 5, 73, 23, 48)
  C_output <- Proportion_of_total_variability_C*Percentage_Conversion_Coefficient
  S_output <- Proportion_of_total_variability_S*Percentage_Conversion_Coefficient
  R_output <- Proportion_of_total_variability_R*Percentage_Conversion_Coefficient
  variances <- (C_output - C_values)^2 + (S_output - S_values)^2 + (R_output - R_values)^2
  min_variance_index <- which.min(variances)
  csr_type <- c("C", "C/CR", "C/CS", "CR", "C/CSR", "CS", "CR/CSR", " CS/CSR", "R/CR", "CSR", "S/CS", "R/CSR", "S/CSR", "R", "SR/CSR", "S", "R/SR", "S/SR", "SR")[min_variance_index]

  result <- list(c_proportion = C_output,s_proportion = S_output,r_proportion = R_output,csr_type = csr_type)
  return(result)
}


#' Calculate CSR strategy for multiple plant species
#'
#' @description
#' This function processes a dataframe containing leaf traits (LA, LDMC, SLA) and applies the Strate_CSR function to each row.
#'
#' @param data A dataframe containing columns for LA (Leaf Area), LDMC (Leaf Dry Matter Content), and SLA (Specific Leaf Area).
#'
#' @return A dataframe with the original input data and additional columns:
#' * C: Competitive strategy score
#' * S: Stress-tolerant strategy score
#' * R: Ruderal strategy score
#' * type: The dominant CSR strategy type
#'
#' @examples
#' LA <- c(369615.7, 11.8, 55.7, 36061.2, 22391.8, 30068.1, 31059.5, 29895.1)
#' LDMC <- c(25.2, 39.7, 13.3, 35.5, 33.2, 36.1, 35.2, 34.9)
#' SLA <- c(17.4, 6.6, 34.1, 14.5, 8.1, 12.1, 9.4, 10.9)
#' traits <- data.frame(LA, LDMC, SLA)
#' CSR(data = traits)
#'
#' @references
#' 1. Grime, J.P. (1974). Vegetation classification by reference to strategies. Nature, 250, 26–31.
#' 2. Pierce, S., Negreiros, D., Cerabolini, B.E.L., Kattge, J., Díaz, S., et al. (2017). A global method for calculating plant CSR ecological strategies applied across biomes world-wide. Funct Ecol, 31: 444-457.
#'
#' @export
CSR <- function(data) {
  # Check input data
  if (!is.data.frame(data)) {
    stop("The input must be a dataframe")
  }

  required_columns <- c("LA", "LDMC", "SLA")
  if (!all(required_columns %in% colnames(data))) {
    stop("The input data frame must contain LA, LDMC and SLA columns")
  }

  # Apply the Strate_CSR function to each row of data
  results <- t(apply(data, 1, function(row) {
    result <- Strate_CSR(LA = row["LA"], LDMC = row["LDMC"], SLA = row["SLA"])
    unlist(result)
  }))

  # Converting results to dataframes
  results_df <- as.data.frame(results)
  colnames(results_df) <- c("C", "S", "R", "type")
  results_df$C <- as.numeric( results_df$C)
  results_df$S <- as.numeric( results_df$S)
  results_df$R <- as.numeric( results_df$R)

  results <- cbind(data,results_df)

  return(results)
}


#' Create a ternary plot of CSR strategies
#'
#' This function creates a ternary plot of Competition-Stress-Ruderal (CSR) strategies using the ggtern package.
#'
#' @param data A dataframe containing columns C, S, R, and type for CSR strategy values and plant types.
#'
#' @return A ggplot object representing the ternary plot of CSR strategies.
#'
#' @details The function uses ggtern to create a ternary plot with the following features:
#' * Points colored and filled by plant type
#' * Axis labels for C, S, and R percentages
#' * A white background with RGB colored axes
#' * Legend positioned at the bottom
#'
#' @importFrom ggtern ggtern geom_mask theme_rgbw
#' @importFrom ggplot2 aes geom_point labs theme element_rect element_text
#'
#'
#' @references
#' 1. Grime, J.P. (1974). Vegetation classification by reference to strategies. Nature, 250, 26–31.
#' 2. Pierce, S., Negreiros, D., Cerabolini, B.E.L., Kattge, J., Díaz, S., et al. (2017). A global method for calculating plant CSR ecological strategies applied across biomes world-wide. Funct Ecol, 31: 444-457.
#'
#' @examples
#' LA <- c(369615.7, 11.8, 55.7, 36061.2, 22391.8, 30068.1, 31059.5, 29895.1)
#' LDMC <- c(25.2, 39.7, 13.3, 35.5, 33.2, 36.1, 35.2, 34.9)
#' SLA <- c(17.4, 6.6, 34.1, 14.5, 8.1, 12.1, 9.4, 10.9)
#' traits <- data.frame(LA, LDMC, SLA)
#' result <- CSR(data = traits)
#' CSR_plot(data=result)
#'
#' @export
CSR_plot <- function(data){
  p1 <- ggtern::ggtern(data=data, aes_string(x="C", y="S", z="R", fill="type")) +
    ggtern::geom_mask() +
    ggplot2::geom_point(size=2, shape=21) +
    ggtern::theme_rgbw() +
    ggplot2::labs(x="C (%)", y="S (%)", z="R (%)") +
    ggplot2::theme(legend.position = "bottom",
                   legend.key = ggplot2::element_rect(fill = NA),
                   text = ggplot2::element_text(size=14, family="serif"))
  return(p1)
}



