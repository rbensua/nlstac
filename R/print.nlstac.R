#' @title Print a nlstac Model
#' @description Standard method for overriding the print.list method for nlstac model fit.
#'
#' @param x An object of class \code{"nlstac"} obtained by the \code{nls_tac} function.
#' @param digits a positive integer indicating how many significant digits are to be shown. 
#' @param ... Ignored, for compatibility issues.
#'
#'
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
#' @method print nlstac
#' @export
#' 


print.nlstac <- function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
  cat("Nonlinear nlstac regression model\n")
  cat("  model: ", deparse(formula(x)), "\n", sep = "")
  cat("   data: ", deparse(x$dataset), "\n", sep = "")
  print(x$coefficients, digits = digits, ...)
  cat("\nNumber of iteration to convergence:",x$convInfo$niter,
      "\nAchieved convergence tolerance:",
      format(x$convInfo$tolerance, digits = digits))
  cat("\n")
  invisible(x)
}