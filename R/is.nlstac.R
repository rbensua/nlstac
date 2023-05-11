#' @title Is nlsTAC class check
#' @description Checks wether an R object is of tac class or not.
#' @usage is.nlstac(x)
#'
#' @param x Any \bold{R} object.
#' @return Returns \code{TRUE} if its argument is a tac object (that is, has "tac"
#'   amongst its classes) and \code{FALSE} otherwise.
#'
#'
#' @export


is.nlstac <- function(x) {

  inherits(x, "nlstac")

}
