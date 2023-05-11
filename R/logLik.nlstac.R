#' @title Extract Log-Likelihood from a nlstac Model
#' @description Returns the log-likelihood value from an object returned by a nlstac model fit.
#'
#' @param object An object of class \code{"nlstac"} obtained by the \code{nls_tac} function.
#' @param ... Ignored, for compatibility issues.
#'
#'
#' @return A single numeric value for the log-likelihood of the model
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
#' @method logLik nlstac
#' @export
#' 

logLik.nlstac <- function(object, ...){
  res <- as.numeric(object$resid[,1])
  N <- length(res)
  w <- rep_len(1, N)
  zw <- w == 0
  N <- sum(!zw)
  val <- -N * (log(2 * pi) + 1 - log(N) - sum(log(w + zw))/N + 
                 log(sum(res^2)))/2
  attr(val, "df") <- 1L + length(coef(object))
  attr(val, "nobs") <- attr(val, "nall") <- N
  class(val) <- "logLik"
  val
}

