

source("./strdate_aux.R")
require(ggplot2)
require(grid)
require(scales)
require(plyr)
require(lubridate)
require(reshape2)
require(xtable)
require(MASS)
require(lattice)
require(moments)
require(microbenchmark)
require(foreach)
require(doParallel)
require(stringr)
require(truncnorm)
options(stringsAsFactors = F)


#
# Given matrix of index returns, simulate k iterations constrained to multi-day return
#  that compounds to r_0 (if r_ab = NULL), otherwise compounds to r s.t. r_0 <= r <= r_ab
#  i.e. for one sided inequalities, set r_0 = -Inf or r_ab = Inf, or combine for interval
#  and for compliment of interval, or "tails only", swap your r_0 and r_ab values
#
sim_index <- function(R_mat, r_0 = NULL, k = 100000,
                      h_factor = 0.1, nstol = Inf, r.lag = 0,
                      r_ab = NULL)
{
  #R_mat <- do_findatamatroll(index_trs[["SPX"]], 5); r_0 = NULL; k = 1
  #h_factor = 0.001; nstol = 0.01; r.lag = 1
  
  # calculate fundamental quantities
  p <- dim(R_mat)[2]
  n <- dim(R_mat)[1]
  X_mat <- log(1 + R_mat)
  x_0 <- log(1 + r_0)
  rtp <- (r.lag + 1):p
  if (r.lag >= p)
  {
    warning("\n  r.lag >= p.  Proceed as if r.lag = p and nstol = Inf  \n")
    rtp <- 1:p
    nstol <- Inf
  }
  
  # calculate bandwidths
  # use rolling period count
  #N <- n
  # use non-overlapping period count (overestimated or larger h)
  N <- (n - 1 + p) / p
  # use mv normal rule
  #h <- ((4 / (p + 2)) ^ (1 / (p + 4))) * (diag(cov(X_mat)) ^ (1/2)) * (N ^ (-1 / (p + 4)))
  # use mv scott rule
  h <- (diag(cov(X_mat)) ^ (1/2)) * (N ^ (-1 / (p + 4)))
  # use single h for all dim, scale down drastically for reasonableness
  #cat("mean h:", mean(h), "\n")
  h <- rep(mean(h), p) * h_factor 

  # HANDLE INEQUALITIES ON r_0/x_0
  if (is.null(r_ab) == F)
  {
    x <- apply(X_mat[, rtp, drop = F], 1, sum)
    h_0 <- density(x, bw = "nrd", give.Rkern = T)
    if (r_0 < r_ab)
    {
      a <- x_0
      b <- log(1 + r_ab)
      x_0 <- sample(x, k, replace = T, prob = pnorm(b, x, h_0) - pnorm(a, x, h_0))
      x_0 <- x_0 + rtruncnorm(k, a - x_0, b - x_0, 0, h_0)
    } else {
      a <- log(1 + r_ab)
      b <- x_0
      w_a <- pnorm(a, x, h_0)
      w_b <- 1 - pnorm(b, x, h_0)
      x_0 <- sample(x, k, replace = T, prob = w_a + w_b)
      r <- sapply(1:k, function(i) sample(c(0, 1), 1, T, cbind(w_a, w_b)[i,]))
      # CONSIDER REVISING TO AVOID WASTED function calls
      r <- (1 - r) * rtruncnorm(k, -Inf, a - x_0, 0, h_0) + r * rtruncnorm(k, b - x_0, Inf, 0, h_0)
      x_0 <- x_0 + r
    }
  }

  options(warn = -1)
  # Unconditional simulation or Naive Conditional Simulation
  if (length(x_0) == 0 | nstol < Inf | p == 1 | r.lag >= p)
  {
    mu <- X_mat
    sigma <- diag(h, p, p)
    X_sim <- matrix(NA, 0, p)
    N <- 0
    if (length(x_0) == 0)
      x_0 <- 0
    while (N < k)
    {
      X_temp <- mu[sample.int(n, k, replace = T), , drop = F] + mvrnorm(k, rep(0, p), sigma)
      keepers <- abs(apply(X_temp[, rtp, drop = F], 1, sum) - x_0) < nstol
      X_sim <- rbind(X_sim, X_temp[keepers, , drop = F])
      N <- N + sum(keepers)
      cat("hit rate %:", (sum(keepers) / k * 100), "sample count:", N, "\n")
    }
    X_sim <- X_sim[1:k, , drop = F]
  } else {
    
    # Conditional simulation.  Smart!
    sum_Xj <- apply(X_mat[, rtp, drop = F], 1, sum)
    if (length(x_0) == 1)
    {
    mu <- X_mat[,-p] + ((rep(1, n) %*% t(h[-p]^2)) *
                          ((x_0 - sum_Xj) %*% t(rep(1, p-1))) * (1 / sum(h^2)))
    sigma <- diag(h[-p]^2, p - 1, p - 1) - ((1 / sum(h^2)) * (h[-p]^2 %*% t(h[-p]^2)))
    alpha <- (1 / n) * (2 * pi * sum(h[rtp]^2))^(-1 / 2) *
                         exp((-1 / (2 * sum(h[rtp]^2))) * (x_0 - sum_Xj)^2)
    #cat("NA count:", sum(is.na(alpha)), "alpha sum:", sum(alpha), "\n")
    alpha[is.na(alpha)] <- 0.0
    
    if (sum(alpha) <= 0.0)
    {
      X_sim <- matrix(NA, k, p)
    } else {
      
      X_sim <- mu[sample.int(n, k, replace = T, prob = alpha / sum(alpha)), , drop = F] +
                 mvrnorm(k, rep(0, p-1), sigma)
      X_sim <- cbind(X_sim, x_0 - apply(X_sim[, rtp[-length(rtp)], drop = F], 1, sum))
    }
    } else {
    registerDoParallel(cores = 12)
    X_sim <- foreach(i = 1:k, .combine = rbind) %dopar% {
      mu <- X_mat[,-p] + ((rep(1, n) %*% t(h[-p]^2)) *
                          ((x_0[i] - sum_Xj) %*% t(rep(1, p-1))) * (1 / sum(h^2)))
    sigma <- diag(h[-p]^2, p - 1, p - 1) - ((1 / sum(h^2)) * (h[-p]^2 %*% t(h[-p]^2)))
    alpha <- (1 / n) * (2 * pi * sum(h[rtp]^2))^(-1 / 2) *
                         exp((-1 / (2 * sum(h[rtp]^2))) * (x_0[i] - sum_Xj)^2)
    #cat("NA count:", sum(is.na(alpha)), "alpha sum:", sum(alpha), "\n")
    alpha[is.na(alpha)] <- 0.0
    
    if (sum(alpha) <= 0.0)
    {
      X_sim <- matrix(NA, 1, p)
    } else {
      
      X_sim <- mu[sample.int(n, 1, replace = T, prob = alpha / sum(alpha)), , drop = F] +
                 mvrnorm(1, rep(0, p-1), sigma)
      X_sim <- cbind(X_sim, x_0[i] - apply(X_sim[, rtp[-length(rtp)], drop = F], 1, sum))
    }
    }
    registerDoSEQ()
    stopImplicitCluster()
    }
    }
  
  options(warn = 0)
  colnames(X_sim) <- paste(1:p)
  exp(X_sim) - 1
}

