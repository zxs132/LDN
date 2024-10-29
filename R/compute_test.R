#' @export compute_test

#' @title Run random forest models to find significant predictive relationships
#'
#' @param data_list A list containing data and its attributes (preferably from data_reshape())
#'
#' @return A list containing results of random forest model (MDG, p-value, optimized number of trees, etc)
#' @export
#'
#' @examples
#' ia_test2 <- compute_test(ia_test1)
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
