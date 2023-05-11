#' @title Get best-fit parameters
#' @description Returns the best-fit parameters for a given nonlinear parameter bounds and nonlinear functions.
#' @details This is an internal function called from  \code{nls_tac} function. It is not intended for direct use.
#' @param dat Data frame with the data points to be fitted.
#' @param form A formula given in the form "LHS ~ a1 * F_1(x,p1) + a2 * F_2(x,p2) + ... + an F_n(x,pn)"
#' @param functions A string array with the nonlinear functions as obtained with \code{get_functions} functions.
#' @param nlparam A list with the names of the nonlinear parameters and their lower and upper bounds in the form \code{c(lower,upper)}.
#' @param lp A string array with the names of the linear parameters contained in the formula as obtained with \code{get_parameters} function
#' @param lp_bounds An optional list with the bounding restrictions over the linear parameters.
#' @param lhs_var The name of the left-hand-side of the formula
#' @param N Size of the partition of the nonlinear parameters. Defaults to 10.
#' @param silent Logical. If TRUE (default) supresses any warnings regarding the collinearity of the columns of the matrix in the determination of the best linear parameters.
#' @param parallel Logical. If TRUE then multicore parallelization of for loops is done with the parallel package. Defaults to FALSE.
#' @return  A list containing the strings for the  nonlinear functions of the formula.
#' @importFrom foreach foreach %dopar%
#' @importFrom stats as.formula formula lm coefficients complete.cases
#'
get_best_params <-
  function(dat,
           form,
           functions,
           nlparam,
           lp,
           lp_bounds = NULL,
           lhs_var,
           N = 10,
           silent = TRUE,
           parallel = FALSE) {
    
    nonlparam_full <- lapply(nlparam, function(x){
      seq(from = x[1], to = x[2], length.out = N)
    })
    
    NLP <- expand.grid(nonlparam_full)
    form <- as.formula(form)
    
    if (parallel) {
      
      LP <-
        foreach(kidx = seq_len(nrow(NLP)), .combine = rbind) %dopar% {
          for (i in names(nonlparam_full)) {
            assign(i, NLP[kidx, i])
          }
          nlMatrix <-
            matrix(0, nrow = nrow(dat), ncol = length(functions))
          colnames(nlMatrix) <- lp
          for (j in seq_along(functions)) {
            nlMatrix[, j] <- eval(formula(paste("y~", functions[j]))[[3]], dat)
          }
          if (qr(nlMatrix)$rank < ncol(nlMatrix)) {
            if (!silent) {
              warning("Rank deficient matrix! Try to change the parameters!")
            }
            rep(NA, length(lp))
          } else{
            datdf <-
              cbind(data.frame(lhs = unname(dat[, lhs_var])), data.frame(nlMatrix))
            fitlm <- lm(lhs ~  . - 1 , data = datdf)
            coefficients(fitlm)
            
            
          }
        }
      
    } else{
      LP <-
        matrix(
          0,
          nrow = nrow(NLP),
          ncol = length(lp),
          dimnames = list(NULL, lp)
        )
      
      for (kidx in seq_len(nrow(NLP))) {

        for (i in names(nonlparam_full)) {
          assign(i, NLP[kidx, i])
        }
        nlMatrix <- matrix(0, nrow = nrow(dat), ncol = length(functions))
        colnames(nlMatrix) <- lp
        
        
        for (j in seq_along(functions)) {
          nlMatrix[, j] <- eval(formula(paste("y~", functions[j]))[[3]], dat)
        }
        
        if (qr(nlMatrix)$rank < ncol(nlMatrix)) {
          if (!silent) {
            warning("Rank deficient matrix! Try to change the parameters!")
          }
          LP[kidx, ] <- rep(NA, ncol(LP))
        } else{
          datdf <-
            cbind(data.frame(lhs = unname(dat[, lhs_var])), data.frame(nlMatrix))
          
          fitlm <- lm(lhs ~  . - 1 , data = datdf)
          LP[kidx, ] <- coefficients(fitlm)
          
        }
      }
    }
    
    non_na_idxs <- complete.cases(LP)
    LP <- LP[non_na_idxs,]
    if(!is.matrix(LP)){
      LP <- matrix(LP, ncol = 1, dimnames = list(NULL, lp))
    }
    
    NLP <- NLP[non_na_idxs,]
    # Checking that NLP continues being a matrix with the same colnames
    if (!is.matrix(NLP) & is.numeric(NLP)){
      NLP <- matrix(NLP, ncol = 1)
      colnames(NLP) <- names(nlparam)
    }
    
    if (!is.null(lp_bounds)){
      lpidx <- matrix(NA, nrow = nrow(LP), ncol = length(lp_bounds))
      for(i in seq_along(lp_bounds)){
        lp_bounded_param <- names(lp_bounds[i])
        lpidx[,i] <- LP[,lp_bounded_param] >= lp_bounds[[lp_bounded_param]][1] &
          LP[, lp_bounded_param] <= lp_bounds[[lp_bounded_param]][2]
      }
      lpidx <- apply(lpidx, MARGIN = 1, FUN = function(x) all(x))
      
      if(!any(lpidx)){
        stop("No feasible solution found. Try modifying the linear parameters bounds.")
      } else {
        LP <- LP[lpidx,]
        NLP <- NLP[lpidx,]
      }
    }
    
    
    PAR <- cbind(NLP, LP)
    PAR <- as.data.frame(PAR)
    
    SSR <- apply( PAR, MARGIN = 1, FUN = function(x) {
      if (any(is.na(x))) {
        ssr <- Inf
      } else{
        
        for (i in seq_len(ncol(PAR))) {
          assign(colnames(PAR)[i], x[i])
        }
        yhat <- eval(form[[3]], dat)
        ssr <- sum((yhat - dat[lhs_var]) ^ 2, na.rm = TRUE)
      }
      return(ssr)
    }
    )
    
    PAR$SSR <- SSR
    
    minSSR <- which.min(PAR$SSR)
    
    
    
    coef <- PAR[minSSR, ]
    for (i in seq_len(length(nlparam) + length(lp))) {
      assign(colnames(coef)[i], coef[[i]])
    }
    fitted <- eval(form[[3]], dat)
    residuals <-  fitted - dat[lhs_var]
    
    return(list(
      coef = coef,
      PARAM = PAR,
      nonlparam_full = nonlparam_full,
      residuals = residuals,
      fitted = fitted
    ))
    
  }
