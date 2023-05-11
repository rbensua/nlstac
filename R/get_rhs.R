#' @title Get right hand side of a formula
#' @description Returns the dependent variable in a formula given by a string or a \code{formula}
#' @param form Either a string in the form \code{'y ~ ...'} or an object of \code{formula} class
#' @return  A string with the name of the left hand side variable in the formula
#' @importFrom stats as.formula
#' @importFrom methods is
#' 
get_rhs <- function(form) {
  if(is(form, "formula")){
    form <- paste(deparse(form), collapse = " ")
    form <- gsub("\\s+", " ", form, perl = FALSE)
  }
  if(is.character(form)){
    rhs_var <- unlist(strsplit(as.character(form), "~"))[2]
    rhs_var <- trimws(rhs_var)
    form <- as.formula(form)
  }

  return(rhs_var)
}
