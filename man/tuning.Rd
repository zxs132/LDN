% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tuning.R
\name{tuning}
\alias{tuning}
\title{Tuning random forest}
\usage{
tuning(data, target, num_permutation)
}
\arguments{
\item{data}{A dataframe containing possible predictors from same and preceding blocks}

\item{target}{A character representing response variable for random forest model}

\item{num_permutation}{A numeric number indicating number of permutation for Altmann's approach}
}
\value{
A list containing results of tuning process
}
\description{
This function tunes random forest models by first calculating optimal number of trees for each model and finding mean decrease in Gini (MDG) representing importance of one variable in predicting another.
}
\references{
\itemize{
\item Breiman, L. (2001). Random forests. Mach Learn, 45:5-32. https://doi.org/10.1023/A:1010933404324.
\item Wright, M. N. & Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C++ and R. J Stat Softw 77:1-17. https://doi.org/10.18637/jss.v077.i01.
\item Altmann A, Toloşi L, Sander O, Lengauer T. Permutation importance: a corrected feature importance measure. Bioinformatics. 2010;26(10):1340-1347.
}
#'
}
