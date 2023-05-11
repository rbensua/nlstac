#' @title Extract Fitted Values from a nsltac Fit
#' @description Returns the fitted values from an object returned by a nlstac model fit.
#'
#' @param object An object of class \code{"nlstac"} obtained by the \code{nls_tac} function.
#' @param ... Ignored, for compatibility issues.
#'
#'
#' @return A single numeric value for the deviance of the model
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
#'
#' @method fitted nlstac
#' @export
#' 

fitted.nlstac <- function(object, ...){
  val <- as.vector(object$fitted)
  lab <- "Fitted values"
  attr(val, "label") <- lab
  val
}