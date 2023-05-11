#' @title Residuals Degree-of-Freedom of a nsltac Fit
#' @description Returns the residuals degrees-of-freedom from a nlstac model fit.
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
#' @method df.residual nlstac
#' @export
#' 

df.residual.nlstac <- function(object, ...){
  object$df
}