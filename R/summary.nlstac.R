#' @title Summary a nls tac fit.
#' @description Gives the fitted coefficients and the convergence information of the fit.
#'
#' @param object An object of class \code{"nlstac"} obtained by the \code{fit_tac} function.
#' @param ... Ignored, for compatibility issues.
#'
#'
#' @return Returns, via the \code{print.nlstac} function the following items:
#'     - Formula: The formula fitted to the data
#'     - Parameters: The value of the estimated parameters (Estimated) together
#'                   with their standard errors (Std. Error), and their statistical
#'                   significance (t value, Pr(>|t|), signif. stars)
#'     - SSR and df.
#'     - Convergence information: N. of iterations and the tolerance achieved.
#'
#' @method summary nlstac
#' @importFrom stats coef pt
#' @export
#'

summary.nlstac <- function(object, ...){

  if(!is.nlstac(object)){
    stop("Input should be of class nlstac!")
  }
  r <- as.vector(object$resid[,1])
  w <- object$weights
  n <- if (!is.null(w)){
    sum(w > 0)
  }else {
      length(r)
    }
  param <- coef(object)
  pnames <- names(coef(object))
  p <- length(param)
  rdf <- n - p
  resvar <- if (rdf <= 0){
    NaN
  } else{
    object$SSR/rdf
  }
  if(!is.null(object$Rmat)){
  XtXinv <- chol2inv(object$Rmat)
  dimnames(XtXinv) <- list(pnames, pnames)
  se <- sqrt(diag(XtXinv) * resvar)
  tval <- param/se
  prob <- 2 * pt(abs(tval), rdf, lower.tail = FALSE)
  } else{
    XtXinv <- NULL
    se <- rep(NA, length(pnames))
    tval <- rep(NA, length(pnames))
    prob <- rep(NA, length(pnames))

  }
  param <- cbind(param, se, tval, prob)
  dimnames(param) <- list(pnames, c("Estimate", "Std. Error",
                                    "t value", "Pr(>|t|)"))

  ans <- list(formula = object$formula, residuals = r, sigma = sqrt(resvar),
              df = c(p, rdf), cov.unscaled = XtXinv,
              convInfo = object$convInfo,
              coefficients = param, parameters = param)

  return(structure(ans, class = "summary.nlstac"))
}
