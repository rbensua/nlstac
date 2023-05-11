#' @title Calculate Variance-Covariance Matrix for a nlstac Fitted Model Object
#' @description Returns the variance-covariance matrix of the main parameters of a fitted model object. 
#' The “main” parameters of model correspond to those returned by coef,
#'
#' @param object An object of class \code{"nlstac"} obtained by the \code{nls_tac} function.
#' @param ... Ignored, for compatibility issues.
#'
#'
#' @return A matrix of the estimated covariances between the parameter estimates.
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
#' @method vcov nlstac
#' @export
#' 

vcov.nlstac <- function(object, ...){
  XtXinv <- chol2inv(object$Rmat)
  pnames <- names(coefficients(object))
  dimnames(XtXinv) <- list(pnames, pnames)
  resvar <- deviance.nlstac(object) / df.residual.nlstac(object)
  sigma <- sqrt(resvar)
  XtXinv * sigma^2
}