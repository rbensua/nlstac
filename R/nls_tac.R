#' @title Nonlinear fit with the TAC algorithm
#' @description Fits a nonlinear function to data.
#' @param formula A formula given in the form "LHS ~ a1 * F_1(x,p1) + a2 * F_2(x,p2) + ... + an F_n(x,pn)"
#' @param data Data frame with the data points to be fitted.
#' @param functions A string array with the nonlinear functions. If \code{get_functions} fails to properly provide the functions they should be explicitly introduced.
#' @param nlparam A list with the names of the nonlinear parameters and their lower and upper bounds in the form \code{c(lower,upper)}.
#' @param lp_bounds An optional list with the bounding restrictions over the linear parameters.
#' @param N Size of the partition of the nonlinear parameters. Defaults to 10.
#' @param tol Stopping condition. The algorithm stops whenever the maximum difference between two consecutive iterations is less than \code{tol}. Default value is 1e-4
#' @param parallel Logical. If TRUE then multicore parallelization of for loops is done with the parallel package. Defaults to FALSE.
#' @param maxiter Integer. The maximum number of iterations. Defaults to 50.
#' @param quiet Logical. If TRUE, all progress messages are supressed (defaults to FALSE). 
#' @param silent Logical. Parameter to be passed to get_best_parameters function. If TRUE (default) suppresses any warnings regarding the collinearity of the columns of the matrix in the determination of the best linear parameters.
#' @param compute_errors Logical. If TRUE (default value) the function computes the standard error of the estimates.
#' @return  An object of class \code{nlstac}.  A list of
#'
#' \item{\code{coefficients}}{Best coefficients obtained.}
#'
#' \item{\code{stdError}}{Standard errors for the obtained coefficients}
#'
#' \item{\code{convInfo}}{Convergence information: a list with the number of iterations performed (\code{niter}) and the tolerance attained at convergence (\code{tol})}
#'
#' \item{\code{SSR}}{Sum of the squares of the residuals}
#'
#' \item{\code{resid}}{Residuals}
#'
#' \item{\code{data}}{Data frame used. Columns of variables not used in the formula fitted will be removed}
#'
#' \item{\code{formula}}{Formula used}
#'
#' \item{\code{df}}{Degrees of freedom}
#'
#' \item{\code{sigma}}{Standard deviation estimate.}
#'
#' \item{\code{Rmat}}{R matrix in the QR decomposition of the gradient matrix used for the computation of the standard errors of the coefficients}
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
#' @references
#'
#' Fernández Torvisco, J. A.; Rodríguez-Arias Fernández, M.; Cabello Sánchez, J. (2018). “A New Algorithm to Fit Exponential Decays without Initial Guess”, Filomat 32:12, 4233–4248.
#'
#' Bates, D. M. and Watts, D. G. (1988) Nonlinear Regression Analysis and Its Applications, Wiley
#'
#' @examples
#' ### Examples from 'nls' doc ###
#'
#' DNase1 <- subset(DNase, Run == 1)
#' ## using logistic formula
#' fm2DNase1 <- nls_tac(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
#'                    data = DNase1,
#'                    nlparam = list(xmid = c(1e-7,10), scal = c(1e-7,3)))
#' ## some generics are applicable

#' coefficients(fm2DNase1)
#' summary(fm2DNase1)
#' ## obtaining extra information
#' fm2DNase1$resid # residuals
#' fm2DNase1$formula # formula used
#' fm2DNase1$df # degrees of freedom
#' fm2DNase1$convInfo # Convergence information (n. iterations, tolerance attained)
#' fm2DNase1$SSR # SSR
#' fm2DNase1$data$density - fm2DNase1$resid # fitted values
#'
#' ## Synthetic examples
#'
#' ## Double exponential
#' x <- seq(from = 0, to = 20, length.out = 1000)
#' y <- 3*exp(-0.12*x) + 0.6*exp(-3.05*x) +  5 + 0.1*rnorm(length(x))
#' df <- data.frame(time = x, Temp = y)
#' # The nonlinear parameter list (with lower and upper values)
#' nlparam <- list(b1 = c(0,2), b2 = c(0,8))
#' fittac <- nls_tac('Temp ~ a1*exp(-b1*time) +  a2*exp(-b2*time) + a3',
#'                    data = df,
#'                    nlparam = nlparam,
#'                    N = 5)
#' summary(fittac)
#' plot(Temp ~ time, data = df)
#' lines(x, predict(fittac), col = "red", lwd = 2)
#'
#' ##
#' N <- 100
#' x <- seq(from = 0, to = 3, length.out = N)
#' y <- 3*sin(5*x)^2 + 2 + 0.2*rnorm(N)
#' df <- data.frame(x = x, y = y)
#' form <- y ~ a1*sin(b1*x)^2 + a2
#' nlbnds <- list(b1 = c(0.5,10)) # rough bouds for tac
#' tac_model <-  nls_tac(formula = form,
#'                       data = df,
#'                       nlparam = nlbnds,
#'                       N = 10,
#'                       tol = 1e-5)
#' yhat <- predict(tac_model)
#' plot(x,y)
#' lines(x,yhat, col = "blue")
#'
#' @importFrom Deriv Deriv
#' @importFrom stats as.formula qt
#' @export
#'
nls_tac <-
  function(formula,
           data = parent.frame(),
           functions = NULL,
           nlparam,
           lp_bounds = NULL,
           N = 10,
           tol = 1e-4,
           parallel = FALSE,
           maxiter = 50,
           quiet = FALSE,
           silent = TRUE,
           compute_errors = TRUE
  ) {
    dataset <- substitute(data)
    var_names_orig <- colnames(data)
    form <- as.formula(formula)
    # Removing the columns of the original data frame not used in the regression.
    used_var <- which(var_names_orig %in% all.vars(form))
    data <- data[,used_var]
    var_names <- colnames(data)
    
    
    nlp_names <- names(nlparam)
    parameters <- get_parameters(form, var_names)
    if(is.null(functions)){
      functions <- get_functions(form, lp = parameters$linear_parameters)
    }
    lhs_var <- get_lhs(form)
    
    # First step ----
    SOL <- get_best_params(data,
                           form,
                           functions = functions, 
                           nlparam,
                           lp = parameters$linear_parameters,
                           lp_bounds = lp_bounds,
                           lhs_var = lhs_var,
                           N = N,
                           parallel = parallel,
                           silent = silent)
    incr <- tol + 1
    # Rest of steps... ----
    niter <- 1
    while(incr > tol & niter <= maxiter){
      best_coef_old <- SOL$coef
      bestnlp <- best_coef_old[,names(best_coef_old) %in% nlp_names]
      names(bestnlp) <- nlp_names
      if(is.data.frame(bestnlp)){
        idx_bestnlp <- sapply(names(bestnlp), function(x) which(as.numeric(SOL$nonlparam_full[[x]]) == bestnlp[,x]))
      }else{
        idx_bestnlp <- sapply(names(bestnlp), function(x) which(as.numeric(SOL$nonlparam_full[[x]]) == bestnlp[[x]]))
      }
      lower_bound <- pmax(idx_bestnlp-1,1)
      upper_bound <- pmin(idx_bestnlp+1,N)
      nlparam <- lapply(names(bestnlp), function(x) c(SOL$nonlparam_full[[x]][lower_bound[x]], SOL$nonlparam_full[[x]][upper_bound[x]]))
      names(nlparam) <- names(bestnlp)
      SOL <- get_best_params(data,
                             form,
                             functions,
                             nlparam,
                             lp = parameters$linear_parameters,
                             lp_bounds = lp_bounds,
                             lhs_var = lhs_var,
                             N = N,
                             parallel = parallel,
                             silent = silent)
      
      best_coef <- SOL$coef
      incr <- max(abs(best_coef[,1:length(nlparam)] - best_coef_old[,1:length(nlparam)]) / (abs(best_coef_old[,1:length(nlparam)] + 2 * .Machine$double.eps)))
      if(!quiet){
        message(paste0('iteration = ',niter, '/ incr = ',incr))
      }
      niter <- niter + 1
    }
    SSR <- best_coef[,length(nlparam) + length(parameters$linear_parameters) + 1]
    par <- c(parameters$nonlinear_parameters, parameters$linear_parameters)
    names(par) <- par
    
    ## Computing gradients for the error estimates
    ##
    ##
    
    if (compute_errors){
      
      rhs <- get_rhs(formula)
      gradfcn  <- sapply(par, function(p) Deriv(rhs,p))
      
      for (i in names(par)) {
        assign(i, best_coef[[i]])
      }
      
      gradMat <- matrix(0, ncol = length(gradfcn), nrow = nrow(data))
      
      for (j in seq_along(gradfcn)){
        gradMat[,j] <- eval(formula(paste("y~",gradfcn[j]))[[3]], data)
      }
      gradMat[is.nan(gradMat) | is.na(gradMat)] <- 0
      Rmat <- qr.R(qr(gradMat))
      
      
      df <- nrow(data)-length(par)
      
      s <- sqrt(SSR / df)
      
      stdError <- s * sqrt(diag(chol2inv(Rmat)))
      names(stdError) <- names(par)
    } else{
      Rmat = NULL
      stdError = NULL
      s = NULL
    }
    ## Gathering the results
    
    BC <- best_coef[ ,(1:(length(nlparam)+length(parameters$linear_parameters)))]
    nBC <- names(BC)
    BC <- as.numeric(BC)
    names(BC) <- nBC
    tacOutput <- list(
      coefficients = BC,
      stdError = stdError,
      convInfo = list(niter = niter, tolerance = incr),
      SSR = SSR,
      fitted = SOL$fitted,
      resid = SOL$residuals,
      dataset = dataset,
      data = data,
      formula = form,
      df = df,
      sigma = s,
      Rmat = Rmat
    )
    return(structure(tacOutput, class = "nlstac"))
  }
