########## Calling packages ##########

library(reshape)
library(dplyr)
library(ranger)
library(igraph)
library(visNetwork)
library(ztable)
library(grDevices)



########## Reshaping data set ##########

data_reshape <- function(data, time_dep, time_indep, year, id){
  # data: matrix (long form data)
  # time_dep: character vector of time-dependent variables
  # time_indep: character vector of time-independent variables
  # year: character representing year variable
  # id: character representing id variable
  
  ### reshape time dependent variables in wide format
  data <- data %>% arrange(id, year)
  data_reshape1 <- reshape(data, timevar = year, idvar = id,
                           v.names = c(time_indep, time_dep), direction = "wide") %>%
    select_if(~ !all(is.na(.))) 
  
  dup_time_indep <- c()
  for(name in time_indep){
    temp <- colnames(data_reshape1)[grep(name, colnames(data_reshape1))]
    dup_time_indep <- c(dup_time_indep, temp[2:length(temp)])
  }
  data_reshape1 <- data_reshape1[,!(names(data_reshape1) %in% dup_time_indep)]
  
  years <- as.character(sort(unique(data[,year])))
  data_list <- list()
  for(i in 1:length(years)){
    year.ind <- years[1:i]
    data_list[[i]] <- data_reshape1 %>% 
      dplyr::select(ends_with(year.ind)) %>%
      mutate_if(function(x) length(unique(x)) <= 5, as.factor)
  }
  names(data_list) <- paste0("year", years)
  return(list(data = data_list,
              year = data[,year],
              attr = list(n_time_dep = length(time_dep),
                          n_time_indep = length(time_indep),
                          time_dep = time_dep)))
}

  
########## Tuning RFs ##########

tuning <- function(data, target, num_permutation) { 
  # data: matrix
  # target: character representing response variable
  
  data <- data[complete.cases(data),]
  p <- dim(data)[2] - 1 # number of predictors
  f = as.formula(paste(target, '~.'))
  
  ## creating a hyper parameter grid
  hyper_grid <- expand.grid(mvar = seq(1, floor(sqrt(p))*2, by = 1),
                            ntree = seq(100, 1000, by = 50), 
                            OOB_error = 0)
  
  
  for (i in 1:nrow(hyper_grid)) {
    
    rf <- ranger(formula = f, data = data,
                 num.trees = hyper_grid$ntree[i],
                 mtry = hyper_grid$mvar[i],
                 respect.unordered.factors = "partition",
                 seed = 123)
    
    hyper_grid$OOB_error[i] <- rf$prediction.error
  }
  
  ## finding optimal values for mtry and min.node.size
  hyper_grid.ordered <- hyper_grid %>% arrange(OOB_error)
  
  mtry.best <- hyper_grid.ordered[1,]$mvar
  
  ntree.best <- hyper_grid.ordered[1,]$ntree
  
  
  ## computing impurity importance scores
  rf_mdg <- ranger(formula = f, data = data,
                   num.trees = ntree.best,
                   mtry = mtry.best, 
                   respect.unordered.factors = "partition",
                   seed = 123,
                   importance = "impurity")
  
  
  #computing Altmann's p-values based on the impurity importance scores
  
  set.seed(123)
  mdg_df <- importance_pvalues(rf_mdg, method = "altmann",
                               num.permutations = num_permutation,
                               formula = f, data = data) 
  
  # can used importance = 'impurity_corrected' option for the actual impurity reduction (AIR)
  
  results <- list("mtry.best" = mtry.best, 
                  "ntree.best" = ntree.best,
                  "mdg" = mdg_df[,1], 
                  "alt" = mdg_df[,2])
  
  return(results)
  
}


compute_test <- function(data_list){
  vars <- data_list[["data"]][[length(data_list[["data"]])]] %>% colnames
  n <- length(vars)
  years <- as.character(sort(unique(data_list[["year"]])))
  
  # Create empty matrices for p values, optimized number of trees, and MDG
  alt_p <- array(dim = c(n, n), dimnames = list(vars, vars))
  num_tree <- array(dim = c(n, n), dimnames = list(vars, vars))
  mdg <- array(dim = c(n, n), dimnames = list(vars, vars))
  
  # Discover target variables for each year
  target_list <- list()
  for(i in 1:length(years)){
    target_list[[i]] <- data_list[["data"]][[i]] %>% dplyr::select(ends_with(years[i])) %>% colnames
  }
  # Fitting random forest with optimized number of trees
  for(i in 2:length(target_list)){
    for(name in target_list[[i]]){
      results <- tuning(data_list[["data"]][[i]], name, 2000)
      alt_p[names(results$alt), name] <- results$alt
      num_tree[names(results$alt), name] <- results$ntree.best
      mdg[names(results$alt), name] <- results$mdg
    }
  }  
  return(list(data = data_list[["data"]],
              alt_p = alt_p,
              num_tree = num_tree,
              mdg = mdg,
              years = data_list[["year"]],
              attr = data_list[["attr"]]))
}

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
    
    V(n)[ind:(ind + length(vl_list[[i]]) - 1)]$label <- vl_list[[i]]
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
  V(n)$group <- rep(as.character(1:(length(table_year))), c(unlist(lapply(vl_list, length))))
  E(n)$width <- edge_width
  
  layout <- cbind(x, y)
  color <- color
  legend <- data.frame(label = year_char,
                    shape = shape,
                    size = size,
                    color = color,
                    font.size = size,
                    shadow = rep(TRUE, length(year_char)))
  
  plot <- visNetwork::visIgraph(n, idToLabel = F) %>%
    
    visIgraphLayout(layout = "layout.norm", layoutMatrix = layout) %>%
    
    visEdges(shadow = T) %>%
    
    visNodes(shadow = T, font = list(size = size*2)) %>%
    
    visLegend(useGroups = F, addNodes = legend, stepX = 185, position = "right", width = .335)
    
  tables <- table(V(n)$group)
  for(i in 1:length(tables)){
    plot <- plot %>% 
      visGroups(groupname = names(tables[i]), color = color[i])
  }
  plot
}



# ########## Extracting data for cytoscape generation ###########
# setwd("~/Desktop/Yonsei/Semester_2/Probabilistic Graphical Model/Final")
# p <- read.csv("altmann_pvalues.csv", sep = ",", row.names = 1, header = T)
# labels <- rownames(p)
# new_labels <- c("Peripartum depression", "Gender", "Mother's current depression", "Internet addiction", "Having a smartphone", "Happiness", "Sleep duration", "Health status", 
#                 "CBCL (psychiatric symptoms)", "Family composition", "Household income", "Parents' employment status")
# node.name.labels <- c(new_labels, new_labels[3:length(new_labels)])
# new_labels <- c(new_labels, paste0(new_labels[3:length(new_labels)], 2019))
# 
# 
# ### Node names for classification
# node.name1 <- c()
# node.name2 <- c()
# pval <- c()
# 
# for(i in 1:nrow(p)){
#   for(j in 1:ncol(p)){
#       if(!is.na(p[i,j])){
#          if(p[i,j] < 0.05){
#             node.name1 <- c(node.name1, labels[i])
#             node.name2 <- c(node.name2, labels[j])
#             pval <- c(pval, p[i, j])
#          } 
#       }
#   }
# } 
# 
# ### Generating node names in cytoscape
# new.node.name1 <- c()
# new.node.name2 <- c()
# node.name.labels1 <- c()
# node.name.labels2 <- c()
# for(i in 1:length(labels)){
#   if(labels[i] %in% node.name1){
#     ind <- which(node.name1 == labels[i])
#     new.node.name1[ind] <- new_labels[i]
#     node.name.labels1[ind] <- node.name.labels[i]
#   }
#   if(labels[i] %in% node.name2){
#     ind <- which(node.name2 == labels[i])
#     new.node.name2[ind] <- new_labels[i]
#     node.name.labels2[ind] <- node.name.labels[i]
#   }
# }
# 
# data_for_cytoscape <- data.frame(node1 = new.node.name1, node2 = new.node.name2, pval = pval, node_name1 = node.name.labels1, node_name2 = node.name.labels2)
# year1 <- ifelse(substr(node.name1, nchar(node.name1), nchar(node.name1) + 1) == 9, 2019, ifelse(substr(node.name1, nchar(node.name1), nchar(node.name1) + 1) == 7, 2017, 2008))
# year2 <- ifelse(substr(node.name2, nchar(node.name2), nchar(node.name2) + 1) == 9, 2019, ifelse(substr(node.name2, nchar(node.name2), nchar(node.name2) + 1) == 7, 2017, 2008))
# data_for_cytoscape$year1 <- year1
# data_for_cytoscape$year2 <- year2
# 
# ind1 <- c()
# ind2 <- c()
# for(i in 1:nrow(data_for_cytoscape)){
#   ind1[i] <- which(data_for_cytoscape$node1[i] == node.name.labels)
#   ind2[i] <- which(data_for_cytoscape$node2[i] == node.name.labels)
# }
# data_for_cytoscape$node.ind1 <- ind1
# data_for_cytoscape$node.ind2 <- ind2
# 
# write.table(data_for_cytoscape, "~/Desktop/Yonsei/Semester_2/Probabilistic Graphical Model/Final/data_for_cytoscape.txt", quote = FALSE, sep ="\t", row.names = FALSE)


