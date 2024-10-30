#' @export tuning

########## Tuning RFs ##########

#' @title Tuning random forest
#'
#' @param data A dataframe containing possible predictors from same and preceding blocks
#' @param target A character representing response variable for random forest model
#' @param num_permutation A numeric number indicating number of permutation for Altmann's approach
#'
#' @return A list containing results of tuning process
#' @export
#'
#' @importFrom ranger ranger
#'

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

    rf <- ranger::ranger(formula = f, data = data,
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


