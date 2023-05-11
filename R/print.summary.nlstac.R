#' @title Prints the summary a summary.nlstac object.
#' @description Internal function for printing the summary of a nlstac.
#'
#' @param x An object of class \code{"nlstac"} obtained by the \code{fit_tac} function.
#' @param digits Number of significant digits to be shown (defaults to 3).
#' @param signif.stars logical. If TRUE, ‘significance stars’ are printed for each coefficient.
#' @param ... Ignored, for compatibility issues.
#'
#'
#'
#' @method print summary.nlstac
#'
#' @importFrom stats printCoefmat
#' @export
print.summary.nlstac <- function(x, digits = max(3L, getOption("digits") - 3L), #symbolic.cor = x$symbolic.cor,
          signif.stars = getOption("show.signif.stars"), ...)
{
  cat("\nFormula: ", paste(deparse(x$formula), sep = "\n",
                           collapse = "\n"), "\n", sep = "")
  df <- x$df
  rdf <- df[2L]
  cat("\nParameters:\n")
  printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
               ...)
  cat("\nResidual standard error:", format(signif(x$sigma,
                                                  digits)), "on", rdf, "degrees of freedom")
  cat("\n")
  cat("\nNumber of iterations to convergence: ", format(signif(x$convInfo$niter, digits)))
  cat("\nAchieved convergence tolerance: ", format(signif(x$convInfo$tolerance, digits)))
  cat("\n")
  invisible(x)
}
