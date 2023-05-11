#' @title Get parameters from a formula
#' @description Returns the linear and nonlinear parameters of a \code{formula}
#' @param form Either a string in the form \code{'y ~ ...'} or an object of \code{formula} class
#' @param var_names A string array with the column names of the data.frame containing the data to be fitted.
#' @return  A list containing the names of the linear and the nonlinear parameters of the formula.
#'
#' @importFrom stats as.formula terms
#'
#'
get_parameters <- function(form, var_names) {
  form <- as.formula(form)

  indep_vars <- var_names[var_names != get_lhs(form)]
  vars <- all.vars(form)
  if (!all(var_names %in% vars)) stop("Variables names in data are not in formula")
  parameters <- vars[which(!vars %in% var_names)]
  labs <- labels(terms(form))
  linear_parameters <- parameters[which(parameters %in% labs)]

  # checking for combinations of linear parameters

  lin_par_grid <- expand.grid(linear_parameters,linear_parameters)
  lin_par_comb <- apply(lin_par_grid, MARGIN = 1, FUN =  function(x) paste(x[1],x[2],sep = ":"))
  if(any(lin_par_comb %in% labs)) stop("Bad formula. Two linear parameters are multiplying each other...")

  nonlinear_parameters <- parameters[!parameters %in% linear_parameters]

  return(list(linear_parameters = linear_parameters, nonlinear_parameters = nonlinear_parameters))
}
