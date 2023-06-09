% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.residual.R
\name{df.residual.nlstac}
\alias{df.residual.nlstac}
\title{Residuals Degree-of-Freedom of a nsltac Fit}
\usage{
\method{df.residual}{nlstac}(object, ...)
}
\arguments{
\item{object}{An object of class \code{"nlstac"} obtained by the \code{nls_tac} function.}

\item{...}{Ignored, for compatibility issues.}
}
\value{
A single numeric value for the deviance of the model
}
\description{
Returns the residuals degrees-of-freedom from a nlstac model fit.
}
\author{
\strong{Mariano Rodríguez-Arias} (\email{arias@unex.es}).
\emph{Deptartment of Mathematics}

\strong{Juan Antonio Fernández Torvisco} (\email{jfernandck@alumnos.unex.es}).
\emph{Department of Mathematics}

University of Extremadura (Spain)

\strong{Rafael Benítez} (\email{rafael.suarez@uv.es}).
\emph{Department of Business Mathematics}

University of Valencia (Spain)
}
