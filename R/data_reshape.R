#' @export

########## Reshaping data set ##########

#' Title Data reshaping for the analysis
#'
#' @param data A wide format dataframe containing panel data
#' @param time_dep A string of time dependent variables
#' @param time_indep A string of time independent variables (if any)
#' @param year The column name representing year 
#' @param id The column name representing subject ID
#'
#' @return a list containing manipulated data and its attributes
#' @export
#'
#' @examples

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