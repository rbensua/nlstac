#' @title Predict a nls tac fit.
#' @description Returns the prediction values of a nls tac fit model for a given set of predictors.
#'
#' @param object An object of class \code{"tac"} obtained by the \code{nls_tac} function.
#' @param newdata An optional data frame in which to look for variables with which to predict. It should contain
#' at least the columns for the independent variables with the same names as the ones used in the formula passed to the
#' \code{nls_tac} function. If omitted, the fitted values are used.
#' @param ... Ignored, for compatibility issues.
#'
#'
#' @return A vector with the predicted values for the predictor given in the \code{newdata} input.
#'
#' @importFrom stats coef
#' @examples
#'
#'
#' x <- seq(from = 0, to = 3, length.out = 50)
#' y <- 3*exp(-5*x) + 2*x + 1 + 0.05*rnorm(50)
#' df <- data.frame(x = x, y = y)
#' form <- y ~ a1*exp(-b1*x) + a2*x + a3
#' nlbnds <- list(b1 = c(0.5,10)) # bouds for tac
#' fitmodel <-  nls_tac(formula = form, data = df, nlparam = nlbnds)
#' yhat <- predict(fitmodel) # predict values in the fitted abcisae
#' plot(x,y)
#' lines(x,yhat, col = "red", lwd = 2)
#' # Predicting for other points
#' newdata <- c(0.25,1.5,2.25)
#' yhat2 <- predict(fitmodel, newdata = data.frame(x = newdata))
#' points(newdata, yhat2, pch = 19, col = "blue", cex = 1.2)
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
#' @method predict nlstac
#' @export
#'

predict.nlstac <- function(object, newdata = NULL,  ...){

  if(!is.nlstac(object)){
    stop("Input should be of class nlstac!")
  }

  if(is.null(newdata)){
    dat <- object$data
  }else{
    dat <- newdata
  }
  form <- object$formula
  best_coef <- coef(object)
  for(i in names(best_coef)) assign(i,as.numeric(best_coef[i]))

  yhat <- eval(form[[3]], dat)

  return(yhat)

}
