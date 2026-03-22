#' Plot Plant Trait Multilayer Networks (PTMN)
#'
#' @description
#' This function creates visualizations of plant trait multilayer networks (PTMNs) constructed using the PTMN framework.
#' PTMNs systematically integrate multilayer network theory with plant functional trait analysis, enabling quantitative
#' assessments of trait relationships across plant organs and functional systems.
#'
#' @param data A data frame containing the PTMN edge list with columns: node.from, node.to, layer.from, layer.to.
#'   This should be the output from the \code{PTMN} function.
#' @param style Integer specifying the layout style. Options are:
#'   \itemize{
#'     \item 1 - Standard network layout with cross-layer module highlighting (default)
#'     \item 2 - Circular layout with nodes arranged by layer
#'   }
#' @param vertex.size Numeric value controlling the size of network nodes (vertices). Default is 15.
#' @param show.labels Logical indicating whether to display node labels. Default is TRUE.
#' @param vertex.label.cex Numeric value controlling the size of vertex labels. Default is 0.7.
#' @param vertex.label.dist Numeric value controlling the distance of labels from vertices. Default is 0.2.
#' @param edge.width Numeric value controlling the width of network edges. Default is 2.
#' @param vertex.label.font Integer specifying the font style for vertex labels (1=plain, 2=bold, 3=italic, 4=bold italic). Default is 2.
#' @param node.alpha Numeric value between 0 and 1 controlling the transparency of nodes. Default is 1 (opaque).
#' @param module.alpha Numeric value between 0 and 1 controlling the transparency of module highlighting. Default is 0.3.
#' @param show.legend Logical indicating whether to display the layer legend. Default is TRUE.
#' @param legend.pos Character string specifying legend position. Options include "bottomright", "bottomleft", "topright", "topleft". Default is "bottomright".
#' @param legend.cex Numeric value controlling the size of legend text. Default is 1.
#' @param legend.pt.size Numeric value controlling the size of legend symbols. Default is 2.5.
#' @param x.intersp Numeric value controlling horizontal spacing in legend. Default is 2.
#' @param y.intersp Numeric value controlling vertical spacing in legend. Default is 2.
#' @param title.font Integer specifying the font style for legend title. Default is 2 (bold).
#' @param title.cex Numeric value controlling the size of legend title. Default is 1.2.
#'
#' @details
#' The PTMN visualization distinguishes between intralayer edges (black lines) showing relationships between traits
#' within the same organ or functional system, and interlayer edges (red lines) representing interactions between
#' traits in different layers. Each layer corresponds to specific functional or structural units
#' such as individual plant organs (leaves, stems, roots) or functional systems.
#'
#' In style 1, cross-layer modules are highlighted with shaded areas, representing tightly connected
#' functional groups that span multiple layers. In style 2, the circular layout arranges nodes by layer
#' in a circular pattern, making layer organization more visually apparent.
#'
#' Node colors are automatically assigned based on layer membership using the "ggsci::nrc_npg" color palette,
#' ensuring visual distinction between different functional layers.
#'
#' @return
#' No return value. This function is called for its side effect of creating a network plot.
#'
#' @examples
#' \dontrun{
#' data(forest_invader_tree)
#' data(forest_invader_traits)
#' traits <- forest_invader_traits[, 6:73]
#' layers <- list(
#'   shoot_dynamics = c("LeafDuration", "LeafFall50", "LeafRate_max",
#'                      "Chl_shade50", "LAgain", "FallDuration",
#'                      "LeafOut", "Chl_sun50", "EmergeDuration",
#'                      "LeafTurnover"),
#'   leaf_structure = c("PA_leaf", "Mass_leaf", "Lifespan_leaf",
#'                      "Thick_leaf", "SLA", "Lobe", "LDMC",
#'                      "Stomate_size", "Stomate_index"),
#'   leaf_metabolism = c("J_max", "Vc_max", "Asat_area", "CC_mass",
#'                       "LSP", "AQY", "CC_area", "Rd_area",
#'                       "Asat_mass", "WUE", "Rd_mass", "PNUE"),
#'   leaf_chemistry = c("N_area", "Chl_area", "DNA", "Phenolics",
#'                      "Cellulose", "N_mass", "N_litter", "Chl_ab",
#'                      "Chl_mass", "N_res", "C_litter", "C_area",
#'                      "C_mass", "Ash", "Lignin", "Solubles",
#'                      "Decomp_leaf", "Hemi"),
#'   root = c("NPP_root", "SS_root", "SRL", "RTD", "RDMC",
#'            "NSC_root", "Decomp_root", "Starch_root",
#'            "C_root", "N_root", "Lignin_root"),
#'   stem = c("Latewood_diam", "Metaxylem_diam", "Earlywood_diam",
#'            "NSC_stem", "Vessel_freq", "SS_stem", "Cond_stem",
#'            "Starch_stem")
#' )
#' graph <- PTMN(traits, layers_list = layers, method = "pearson")
#' PTMN_plot(graph, style = 1, vertex.size = 8,
#'           vertex.label.cex = 0.5, edge.width = 2,
#'           show.legend = FALSE)
#'}
#'
#' @seealso
#' \code{\link{PTMN}} for constructing plant trait multilayer networks
#'
#' @importFrom igraph graph_from_data_frame V layout_in_circle
#' @importFrom paletteer paletteer_d
#' @importFrom graphics plot legend
#' @importFrom scales alpha
#' @export
PTMN_plot <- function(data,
                      style = 1,
                      vertex.size = 15,
                      show.labels = TRUE,
                      vertex.label.cex = 0.7,
                      vertex.label.dist = 0.2,
                      edge.width = 2,
                      vertex.label.font = 2,
                      node.alpha = 1,
                      module.alpha = 0.3,
                      show.legend = TRUE,
                      legend.pos = "bottomright",
                      legend.cex = 1,
                      legend.pt.size = 2.5,
                      x.intersp = 2,
                      y.intersp = 2,
                      title.font = 2,
                      title.cex = 1.2
) {
  gg <- data[,c("node.from","node.to")]
  g <- igraph::graph_from_data_frame(gg, directed = FALSE)
  node_info <- unique(rbind(
    data.frame(node = data$node.from, layer = data$layer.from),
    data.frame(node = data$node.to, layer = data$layer.to)))
  nlayer <- length(unique(node_info$layer))

  layer_color_map <- scales::alpha(paletteer::paletteer_d("ggsci::nrc_npg", n = nlayer), node.alpha)
  node_info$layer_code <- as.numeric(as.factor(node_info$layer))
  igraph::V(g)$layer_code <- node_info$layer_code[match(igraph::V(g)$name, node_info$node)]
  igraph::V(g)$color <- layer_color_map[igraph::V(g)$layer_code]
  cross_groups <- cross_layer_groups(data)

  module_colors <-  scales::alpha("#E2EEF2", module.alpha)

  edge_layer_from <- data$layer.from
  edge_layer_to <- data$layer.to
  edge_colors <- ifelse(edge_layer_from != edge_layer_to, "red", "black")

  if (show.labels) {
    igraph::V(g)$label <- igraph::V(g)$name
  } else {
    igraph::V(g)$label <- NA
  }

  unique_layers <- unique(node_info[, c("layer", "layer_code")])
  unique_layers <- unique_layers[order(unique_layers$layer_code), ]
  layer_names <- unique_layers$layer

  if (style == 1) {
    plot(g,
         vertex.color = igraph::V(g)$color,
         vertex.size = vertex.size,
         vertex.label = igraph::V(g)$label,
         vertex.label.cex = vertex.label.cex,
         vertex.label.dist = vertex.label.dist,
         vertex.label.font = vertex.label.font,
         vertex.frame.color = "black",
         vertex.label.color = "black",
         edge.width = edge.width,
         edge.color = edge_colors,
         mark.groups = cross_groups,
         mark.col = module_colors,
         mark.border = "black"
    )
  } else if (style == 2) {
    ordered_nodes <- node_info[order(node_info$layer_code), "node"]
    layout <- igraph::layout_in_circle(g, order = match(ordered_nodes, igraph::V(g)$name))
    plot(g, layout = layout,
         vertex.color = igraph::V(g)$color,
         vertex.size = vertex.size,
         vertex.label = igraph::V(g)$label,
         vertex.label.cex = vertex.label.cex,
         vertex.label.dist = vertex.label.dist,
         vertex.label.color = "black",
         vertex.label.font = vertex.label.font,
         edge.width = edge.width,
         edge.color = edge_colors
    )
  } else {
    stop("Invalid style. Please choose 1 or 2.")
  }

  if (show.legend) {
    layer_colors_for_legend <- paletteer::paletteer_d("ggsci::nrc_npg", n = nlayer)
    legend(legend.pos,
           legend = layer_names,
           col = "black",
           pt.bg = layer_colors_for_legend,
           pch = 21,
           pt.cex = legend.pt.size,
           title = "Layers",
           cex = legend.cex,
           bty = "n",
           x.intersp = x.intersp,
           y.intersp = y.intersp,
           title.font = title.font,
           title.cex = title.cex)
  }
}
