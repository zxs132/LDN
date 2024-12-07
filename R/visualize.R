#' @export visualize
#'
#' @title Network visualization for predictie relationships
#'
#' @description
#' This function is designed to visualize predictive relationships between study variables with directed and bi-directed edges. Nodes represent study variables, and edges represent significant predictive relationships.
#'
#'
#' @param data A list to visualize the results (preferably from compute_test())
#' @param edge_width Width of edge in the graph
#' @param cutoff A cutoff for significance
#' @param shape A character for shape of nodes in the graph
#' @param size Node and text size for graph and its legend
#' @param color A character vector to distinguish each year in the data (length: number of repeated measurements)
#'
#' @references
#' \itemize{
#' \item Heckerman D, Chickering DM, Meek C, Rounthwaite R, Kadie C. Dependency networks for inference, collaborative filtering, and data visualization. J Mach Learn Res. 2000;1:49-75.
#' }
#'
#' @export
#'
#' @examples
#' color_brewer <- RColorBrewer::brewer.pal(8, "Pastel1")
#'
#' visualize(ia_test2, color = color_brewer[1:3])





visualize <- function(data, edge_width = 2, cutoff = 0.05, shape = "dot", size = 12, color){
  n <- dim(data$alt_p)
  psig <- matrix(nrow = n, ncol = n)
  psig[data$alt_p <= cutoff] <- 1
  psig[(data$alt_p > cutoff & data$alt_p <= 1) | is.na(data$alt_p)] <- 0
  vars <- colnames(data$alt_p)
  dimnames(psig) <- list(vars, vars)

  ## generate a network object from the adjacency matrix
  n <- igraph::graph.adjacency(psig)
  v <- igraph::get.vertex.attribute(n, "name")

  table_year <- table(data$years)
  year_char <- names(table_year)

  v_list <- list()
  vl_list <- list()
  x <- c()
  y <- c()
  ind <- 1
  x_ind <- 2
  for(i in 1:length(table_year)){
    v_list[[i]] <- v[grepl(year_char[i], v)]
    vl_list[[i]] <- unlist(strsplit(v_list[[i]], paste0(".",year_char[i])))

    igraph::V(n)[ind:(ind + length(vl_list[[i]]) - 1)]$label <- vl_list[[i]]
    ind <- ind + length(vl_list[[i]])

    if(length(vl_list[[i]]) <= 6){ # More than 6 variables would be separated
      x <- c(x, rep(x_ind, length.out = length(vl_list[[i]])))
      x_ind <- x_ind + 3
      y <- c(y, seq(1, 10, length.out = length(vl_list[[i]])))
    }
    else{
      line <- ceiling(length(vl_list[[i]]) / 6)
      q <- length(vl_list[[i]]) %/% line
      r <- length(vl_list[[i]]) %% line
      p <- c(rep(q + 1, r), rep(q, line - r))
      for(j in 1:length(p)){
        temp_ind <- ifelse(j %% 2 == 0, 1, 0)
        y <- c(y, seq(1 + temp_ind, 9 + temp_ind, length.out = p[j]))
        x <- c(x, rep(x_ind, length.out = p[j]))
        x_ind <- x_ind + 1
      }
      x_ind <- x_ind + 2
    }
  }
  igraph::V(n)$group <- rep(as.character(1:(length(table_year))), c(unlist(lapply(vl_list, length))))
  igraph::E(n)$width <- edge_width

  layout <- cbind(x, y)
  color <- color
  legend <- data.frame(label = year_char,
                       shape = shape,
                       size = size,
                       color = color,
                       font.size = size,
                       shadow = rep(TRUE, length(year_char)))

  plot <- visNetwork::visIgraph(n, idToLabel = F) %>%

    visNetwork::visIgraphLayout(layout = "layout.norm", layoutMatrix = layout) %>%

    visNetwork::visEdges(shadow = T) %>%

    visNetwork::visNodes(shadow = T, font = list(size = size*2)) %>%

    visNetwork::visLegend(useGroups = F, addNodes = legend, stepX = 185, position = "right", width = .335)

  tables <- table(igraph::V(n)$group)
  for(i in 1:length(tables)){
    plot <- plot %>%
      visNetwork::visGroups(groupname = names(tables[i]), color = color[i])
  }
  plot
}
