# define model class
Sigmoid <- function(theta=NULL, derivs=FALSE, x, y, init=FALSE, L=100)
{
  
  if (init) {
    
    z <- suppressWarnings(log(( L / y ) - 1))
    
    linear_fit <- lm(z ~ x)
    
    theta1 <- exp(linear_fit$coefficients[[1]])
    theta2 <- linear_fit$coefficients[[2]]
    
    return(c(theta1, theta2))
    
  }
  
  res <- y - (L / (1 + theta[1] * exp(theta[2] * x )))
  
  if (!derivs) {
    return(res)
  }
  
  grad <- matrix(c(-(L * exp(theta[2] * x)) / (1 + theta[1] * exp(theta[2] * x))^2, 
                   -(L * theta[1] * x * exp(theta[2] * x)) / (1 + theta[1] * exp(theta[2] * x))^2), 
                 ncol = 2)
  
  list(residual = res, gradient=grad)
  
}

# nonlinear fitting algorithm
nlsfit <- function(model, theta, x, y, maxiter=25*P, minfactor=1/2^10, tolerance=0.001, verbose=F)
{
  
  resid <- model(theta, derivs = FALSE, x, y)
  newssq <- sum(resid^2)
  P <- length(theta)
  N <- length(resid)
  ndof <- N - P
  mult <- sqrt(ndof / P)
  iteration <- 0
  stepfactor <- 1
  repeat {
    
    iteration <- iteration + 1
    if (iteration > maxiter) stop("Maximum iterations exceeded")
    oldssq <- newssq
    resgrad <- model(theta, derivs = TRUE, x, y)
    qrstr <- qr(resgrad$gradient)
    if(qrstr$rank!=P) stop("Singular matrix")
    qrslv.coef <- qr.coef(qrstr, resgrad$residual)
    qrslv.qty <- qr.qty(qrstr, resgrad$residual)
    incr <- qrslv.coef
    converge <- mult * sqrt(sum(qrslv.qty[1:P]^2) / sum(qrslv.qty[-(1:P)]^2))
    if(verbose) cat(iteration, "<", converge, ">", incr, fill=TRUE)
    if(converge < tolerance) break
    repeat {
      trial <- theta + stepfactor * incr
      if (stepfactor < minfactor) stop("Step factor below minimum")
      newssq <- sum(model(trial, derivs = FALSE, x, y)^2)
      if (verbose) cat(" ", stepfactor, "(", newssq, ")", trial, fill = TRUE)
      if (newssq <= oldssq) break
      stepfactor <- stepfactor / 2
    }
    stepfactor <- min(1, 2 * stepfactor)
    theta <- trial
  }
  
  list(model=model, coef=theta, qr=qrstr, residuals=resgrad$residual, criterion=converge)
  
}


confidence_bands <- function(model, fit, theta, x, alpha=0.05, flag_upper=NULL, L=100)
{
  
  n_params <- length(theta)
  n_degrees <- length(fit$residuals)
  nu <- n_degrees - n_params
  
  s2 <- sum(fit$residuals^2) / nu
  
  y0 <- L / (1 + theta[1] * exp(theta[2] * x ))
  
  v_vec <- model(theta = theta, derivs = TRUE, x, y0)
  
  R1 <- qr.R(fit$qr)
  R1_inv <- solve(R1)
  
  vTR1_inv <- v_vec$gradient %*% R1_inv
  
  e <- sqrt(s2 * sum(vTR1_inv^2) * n_params * qf(p = 1 - alpha, df1 = n_params, df2 = nu))
  
  if (is.null(flag_upper)) {
    
    y0
    
  } else {
  
    if (flag_upper) {
      
      y0 + e
      
    } else {
      
      y0 - e
      
    }
    
  }
  
}