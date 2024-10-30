#' @export tuning

########## Tuning RFs ##########

#' @title Tuning random forest
#'
#' @description
#' This function tunes random forest models by first calculating optimal number of trees for each model and finding mean decrease in Gini (MDG) representing importance of one variable in predicting another.
#'
#'
#' @param data A dataframe containing possible predictors from same and preceding blocks
#' @param target A character representing response variable for random forest model
#' @param num_permutation A numeric number indicating number of permutation for Altmann's approach
#'
#' @return A list containing results of tuning process
#'
#' @references
#' \itemize{
#' \item Breiman, L. (2001). Random forests. Mach Learn, 45:5-32. https://doi.org/10.1023/A:1010933404324.
#' \item Wright, M. N. & Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C++ and R. J Stat Softw 77:1-17. https://doi.org/10.18637/jss.v077.i01.
#' \item Altmann A, Toloşi L, Sander O, Lengauer T. Permutation importance: a corrected feature importance measure. Bioinformatics. 2010;26(10):1340-1347.
#' }
#' #'

#'
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
  rf_mdg <- ranger::ranger(formula = f, data = data,
                   num.trees = ntree.best,
                   mtry = mtry.best,
                   respect.unordered.factors = "partition",
                   seed = 123,
                   importance = "impurity")


  #computing Altmann's p-values based on the impurity importance scores

  set.seed(123)
  mdg_df <- ranger::importance_pvalues(rf_mdg, method = "altmann",
                               num.permutations = num_permutation,
                               formula = f, data = data)

  # can used importance = 'impurity_corrected' option for the actual impurity reduction (AIR)

  results <- list("mtry.best" = mtry.best,
                  "ntree.best" = ntree.best,
                  "mdg" = mdg_df[,1],
                  "alt" = mdg_df[,2])

  return(results)

}


