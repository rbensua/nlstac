#' @title Get nonlinear functions from a separable nonlinear formula
#' @description Returns the nonlinear functions of a \code{formula} as charater strings.
#' @param form Either a string in the form \code{'y ~ ...'} or an object of \code{formula} class
#' @param lp A string array with the names of the linear parameters contained in the formula as obtained with \code{get_parameters} function
#' @details This is an internal function used by \code{nls_tac}. A separable nonlinear formula is of the form
#' \deqn{y ~ a_1 f_1(x;p) + a_2 f_2(x;p) + \ldots + a_n f_n(x;p),}
#' where \eqn{f_1},..., \eqn{f_n} are general nonlinear functions, \eqn{a_1},...,\eqn{a_n}, are the linear coefficients and \eqn{p} is the vector of nonlinear parameters.
#' The \code{formula} given in the input should be of this form and \code{get_functions} will return an array with the string expressions of functions \eqn{f_i}.
#' @note Also formulas of the form
#' \deqn{y ~ a_1/f_1(x;p) + a_2/f_2(x;p) + \ldots}
#' could be given.
#' @return  An array containing the strings for the  nonlinear functions of the formula.
#' @author
#' \strong{Mariano Rodríguez-Arias} (\email{arias@@unex.es}).
#' \emph{Deptartment of Mathematics}
#'
#' \strong{Juan Antonio Fernández Torvisco} (\email{jfernandck@@alumnos.unex.es}).
#' \emph{Department of Mathematics}
#'
#' University of Extremadura (Spain)
#'
#' \strong{Rafael Benítez} (\email{rafael.suarez@@uv.es}).
#' \emph{Department of Business Mathematics}
#'
#' University of Valencia (Spain)
#' @importFrom stringr str_remove str_split str_remove_all str_replace
#'

get_functions <- function(form, lp){
  # If form is given as a formula class we convert it to a character string
  if(inherits(form, "formula")){
    form <- paste(deparse(form), collapse = " ")
    form <- gsub("\\s+", " ", form, perl = FALSE)
  }

  # Get lhs and rhs
  lhs <- get_lhs(form)
  rhs <- get_rhs(form)

  # Remove blanks from rhs
  rhs_nb <- str_remove_all(rhs, " ")

  # We split rhs with delimiters the linear parameters
  # first we build the query pattern (which is "a1|a2|....|an")
  splitpatt <- paste0(lp, "|",collapse = "")
  splitpatt <- str_remove(splitpatt, "\\|$")

  # We make the split
  parts <- str_split(rhs_nb, pattern = splitpatt)[[1]]
  # If one the functions is of the form "a/f(x)", being "a" a linear parameter, then we need to convert it to 1/f(x)
  parts <- str_replace(parts,"^/","1/")

  #We need to deal with the blank elements (the first and/or the independent terms (offsets)))
  blnk <- which(parts == "")
  n <- length(parts)
  for(i in blnk){
    if (i == n){
      parts[i] <- "1"
    } else if (parts[i+1] == "+"){
      parts[i] <- "1"
      parts[i+1] <- ""
    }
  }
  blnk_again <- which(parts == "")
  if (length(blnk_again)>0){
  parts <- parts[-blnk_again]
}

  # We remove the starting "+" and the ending "*"....
  parts <- str_remove(parts,"^\\*")
  nlfunctions <- str_remove(parts,"\\+$")

  # We name the array with their corresponding linear parameters
  names(nlfunctions) <- lp

  # The end
  return(nlfunctions)
}
