
#
# Given matrix of observed tracking errors and simulated index returns,
#  simulate tracking errors for each simulated index return
#

sim_error <- function(sim_index_mat, error_mat, r.lag = 0, e.lag = 0, h_factor = 0.1,
                      use.index_t = F)
{
#   k.sim = 10; n.sim = 20; d.sim = 5;
#   r.lag = 0; e.lag = 2; h_factor = 0.1;
#   sim_index_mat = matrix(0.01, k.sim, d.sim + r.lag)
#   error_mat = t(matrix(rep(c(rep(0.01, r.lag + 1), rep(0.0005, e.lag + 1)), n.sim),
#                        r.lag + 1 + e.lag + 1, n.sim))
#   m.lag <- max(r.lag, e.lag)
  
  # used in do_sample below.  Needed when we include index return at time t in sampling
  fail.flag <- 0
  t.flag <- -1
  if (use.index_t == T)
    t.flag <- T
  
  # calculate fundamental quantities
  k <- dim(sim_index_mat)[1]
  r.p <- dim(sim_index_mat)[2]
  n <- dim(error_mat)[1]
  m <- dim(error_mat)[2]
  e.p <- r.p - r.lag + e.lag
  X_sim <- matrix(NA, k, e.p)
  si_mat <- log(1 + sim_index_mat)
  er_mat <- log(1 + error_mat)
  
  # calculate bandwidths.  CONSIDER REVISING
  # use non-overlapping period count (overestimated or larger h)
  N <- (n - 1 + m) / m
  # use mv scott rule
  h <- (diag(cov(er_mat)) ^ (1/2)) * (N ^ (-1 / (m + 4)))
  # apply various length h_factor
  if (length(h_factor) == 2) {
    h <- h * c(rep(h_factor[1], r.lag + 1), rep(h_factor[2], e.lag + 1))
  } else if (length(h_factor) == 1 | length(h_factor) == length(h)) {
    h <- h * h_factor
  } else {
    cat("Incorrect length of h_factor:", length(h_factor), "vs. length of h:", length(h), "\n")
  }
  
  options(warn = -1)
  if (r.lag == 0 & e.lag == 0 & use.index_t == F)  # no dependence
  {
    for (j in 1:e.p)
    {
      X_sim[, j] <- er_mat[sample.int(n, k, replace = T), -1, drop = F] +
                      mvrnorm(k + 1, rep(0, 2), diag(h))[-1, -1, drop = F]
    }
  } else {
    
    # pack kernel objects
    pack_theta <- function(cols_1, cols_2)
    {
      theta <- list()
      theta$cols_1 <- cols_1
      theta$cols_2 <- cols_2
      theta$mu_1 <- er_mat[, theta$cols_1, drop = F]
      theta$mu_2 <- er_mat[, theta$cols_2, drop = F]
      theta$Sigma_11 <- diag(h)[theta$cols_1, theta$cols_1, drop = F]
      theta$Sigma_11.inv <- solve(theta$Sigma_11)
      theta$Sigma_21 <- diag(h)[theta$cols_2, theta$cols_1, drop = F]
      theta$Sigma_12 <- diag(h)[theta$cols_1, theta$cols_2, drop = F]
      theta$Sigma_22 <- diag(h)[theta$cols_2, theta$cols_2, drop = F]
      theta$Sigma_22.1 <- theta$Sigma_22 -
                            (theta$Sigma_21 %*% theta$Sigma_11.inv %*% theta$Sigma_12)
      theta$Sigma_21.1 <- theta$Sigma_21 %*% theta$Sigma_11.inv
      theta
    }
    
    # core sampling routine
    do_sample <- function(theta, aux_data, day_j)
    {
      # calculate KDE weights based on conditioned variables
      ones <- rep(1, n)
      mu <- theta$mu_1
      dsig <- diag(theta$Sigma_11.inv)
      alpha <- t(sapply(1:k, function(x) (((ones %*% t(aux_data[x,])) - mu) ^ 2) %*% dsig))
#       aux_diff <- function(x, mu) { t(x %*% t(rep(1, n))) - mu }
#       aux_quad <- function(x, sigma_inv) { apply(x * t(sigma_inv %*% t(x)), 1, sum) }
#       atest <- t(sapply(1:k, function(i)
#                                aux_quad(aux_diff(aux_data[i, ], theta$mu_1), theta$Sigma_11.inv)))

      alpha <- (2 * pi) ^ (-length(theta$cols_1) / 2) *
                 det(theta$Sigma_11) ^ (-1 / 2) * exp((-1 / 2) * (alpha))
      alpha <- t(apply(alpha, 1, function(x) ifelse(is.na(x), 0, x)))

      if (sum(rowSums(alpha) == 0) > 0) {
        #cat("FAIL!!  Day:", day_j, " row checks:", sum(rowSums(alpha) == 0), "\n")
        #print(aux_data[rowSums(alpha) == 0,])
        alpha[rowSums(alpha) == 0, ] <- 1
        fail.flag <- fail.flag + 1
        #cat("fail.flag test 1.0", fail.flag, "\n")
      }
      #cat("fail.flag test 1.1", fail.flag, "\n")
      alpha <- alpha / rowSums(alpha)

      # simulate sample indexes, compute and return final sample
      mu_sim <- sapply(1:k, function(x) sample.int(n, 1, prob = alpha[x,]))
      list(X_sim = theta$mu_2[mu_sim, t.flag, drop = F] +
                     mvrnorm(k + 1, rep(0, length(theta$cols_2)),
                             theta$Sigma_22.1)[-1, t.flag, drop = F],
           fail.flag = fail.flag)
    }
    
    #cat("sim day", 1, "\n")
    # simulate for first day of period
    if (r.lag == 0 & use.index_t == F)
    {
      X_sim[, 1:(e.lag + 1)] <- er_mat[sample.int(n, k, replace = T), -1, drop = F] +
                                  mvrnorm(k + 1, rep(0, m), diag(h))[-1, -1, drop = F]
    } else {
      #theta <- pack_theta(1:r.lag, (r.lag + 1):m); aux_data <- si_mat[, 1:r.lag, drop = F]
      cols_1 <- 1:r.lag
      cols_2 <- (r.lag + 1):m
      index_cols <- 1:r.lag
      if (use.index_t == T)
      {
        cols_1 <- 1:(r.lag + 1)
        cols_2 <- (r.lag + 2):m
        index_cols <- 1:(r.lag + 1)
      }
      do_sample_list <- do_sample(pack_theta(cols_1, cols_2),
                                  si_mat[, index_cols, drop = F], 1)
      
      X_sim[, 1:(e.lag + 1)] <- do_sample_list$X_sim
      fail.flag <- fail.flag + do_sample_list$fail.flag
    }
    # iterate over remaining days in sample.  2, 3, ...
    if ((r.p - r.lag) > 1)
    {
      cols_1 <- c(1:m)[-c(r.lag + 1, m)]
      cols_2 <- c(r.lag + 1, m)
      if (use.index_t == T)
      {
        cols_1 <- 1:(m - 1)
        cols_2 <- c(m)
      }
      theta <- pack_theta(cols_1, cols_2)
      for (j in 2:(e.p - e.lag))
      {
        #j <- 2
        #cat("sim day", j, "\n")
        index_cols <- rev(c((j - 1 + r.lag):1)[0:r.lag])
        error_cols <- rev(c((j - 1 + e.lag):1)[0:e.lag])
        if (use.index_t == T)
        {
          index_cols <- rev(c((j - 1 + r.lag):1)[0:(r.lag + 1)])
        }
        #aux_data <- cbind(si_mat[, index_cols, drop = F], X_sim[, error_cols, drop = F])
        do_sample_list <- do_sample(theta, cbind(si_mat[, index_cols, drop = F],
                                                 X_sim[, error_cols, drop = F]), j)
        X_sim[, j + e.lag] <- do_sample_list$X_sim
        fail.flag <- fail.flag + do_sample_list$fail.flag
      }
    }
  }
  
  options(warn = 0)
  colnames(X_sim) <- paste(1:e.p)
  #cat("fail.flag test 1", fail.flag, "\n")
  list(R_sim = exp(X_sim) - 1, fail.flag = fail.flag)
}


#
# master LETF simulator
#

sim_master <- function(myLETF = NULL, d = 1,
                       r.lag = 0, e.lag = 0,
                       r_0 = NULL, k = 100000, h_factor = 0.1,
                       b = 3, annual.fee = 0.0095, k.eval = 0, roll.sim = T,
                       use.index_t = F)
{
#   myLETF = "SPXL"; d = 21;
#   r.lag = 2; e.lag = 2;
#   r_0 = NULL; k = 0; h_factor = 0.1;
#   b = 3; annual.fee = 0.0095;
#   m.lag <- max(r.lag, e.lag)
#   myLETF = "SPXL"; d = 23;
#   r.lag = 5; e.lag = 5;
#   r_0 = -0.168; k = 100; h_factor = c(0.01, 0.000001);
#   b = 3; annual.fee = 0.0095;
#   k.eval = 0; roll.sim = T; use.index_t = T
                               
  fail.flag <- 0
  fee <- (1 + annual.fee) ^ (1 / 252) - 1
  myIndex <- subset(joindf_tickers,
                    letfl_tickers == myLETF | letfs_tickers == myLETF)$index_tickers
  letf_df <- do_findataframe(list(Index = index_trs[[myIndex]],
                                  LETF = c(letfl_trs, letfs_trs)[[myLETF]]),
                             missing = "Returns")
  error_ret <- (as.matrix(letf_df)[, "LETF"] + fee) - (as.matrix(letf_df)[, "Index"] * b)
  
  if (roll.sim == F & k.eval != 0) {
    roll.sim <- d
    d <- nrow(letf_df) - r.lag
  } else {
    roll.sim <- 0
  }
  
  error_mat_index <- do_findatamatroll(as.matrix(letf_df)[, "Index"], 1 + r.lag)
  error_mat_error <- do_findatamatroll(error_ret, 1 + e.lag)
  error_row_index <- T
  error_row_error <- T
  if (r.lag < e.lag)
    error_row_index <- -c(1:(e.lag - r.lag))
  if (r.lag > e.lag)
    error_row_error <- -c(1:(r.lag - e.lag))
  error_mat <- cbind(error_mat_index[error_row_index, , drop = F],
                     error_mat_error[error_row_error, , drop = F])
  colnames(error_mat) <- c(paste("index", 1:(1 + r.lag), sep = ""),
                           paste("error", 1:(1 + e.lag), sep = ""))
  index_mat <- do_findatamatroll(index_trs[[myIndex]], d + r.lag)
  colnames(index_mat) <- paste("index", 1:(d + r.lag), sep = "")
  
  if (k < 0) {
    sim_index_mat <- do_findatamatroll(as.matrix(letf_df)[, "Index"], d + r.lag)
    colnames(sim_index_mat) <- paste(1:(d + r.lag))
  } else if (k == 0) {
    sim_index_mat <- index_mat
    colnames(sim_index_mat) <- paste(1:(d + r.lag))
  } else {
    #sim_index_mat <- sim_index(index_mat, r_0, k = k, h_factor = h_factor, r.lag = r.lag)
    sim_index_mat <- sim_index(index_mat, r_0, k = k, h_factor = 0.1, r.lag = r.lag)
  }
  
  #cat(str(sim_index_mat))
  #cat(str(error_mat))
  
  if (k.eval == 0) {
    sim_error_list <- sim_error(sim_index_mat, error_mat, r.lag, e.lag, h_factor, use.index_t)
    sim_error_mat <- sim_error_list$R_sim
    fail.flag <- fail.flag + sim_error_list$fail.flag
    #cat("fail.flag test 2", fail.flag, "\n")
    X_sim <- b * sim_index_mat[, c(1:d) + r.lag, drop = F] - fee
    X_sim <- X_sim + sim_error_mat[, c(1:d) + e.lag, drop = F]
    colnames(X_sim) <- paste(1:d)
    rownames(X_sim) <- rownames(sim_index_mat)
    #list(X_sim = X_sim, fail.flag = fail.flag)
    list(X_sim = X_sim, fail.flag = fail.flag, R_sim = sim_index_mat)
  } else if (k.eval > 0) {
    R_sim <- matrix(NA, k.eval, nrow(sim_index_mat))
    if (roll.sim > 0) {
      R_sim <- matrix(NA, k.eval, ncol(sim_index_mat) - roll.sim + 1 - r.lag)
    }
    for (i in 1:k.eval) {
      sim_error_list <- sim_error(sim_index_mat, error_mat, r.lag, e.lag, h_factor, use.index_t)
      sim_error_mat <- sim_error_list$R_sim
      fail.flag <- fail.flag + sim_error_list$fail.flag
      #cat("fail.flag test 2", fail.flag, "\n")
      X_sim <- b * sim_index_mat[, c(1:d) + r.lag, drop = F] - fee
      X_sim <- X_sim + sim_error_mat[, c(1:d) + e.lag, drop = F]
      colnames(X_sim) <- paste(1:d)
      rownames(X_sim) <- rownames(sim_index_mat)
      if (roll.sim > 0) {
        X_sim <- do_findatamatroll(c(rep(0, r.lag), X_sim[1,]), roll.sim + r.lag)
        X_sim <- X_sim[, c(1:roll.sim) + r.lag]
      }
      R_sim[i, ] <- apply(1 + X_sim, 1, prod) - 1
    }
    list(R_sim = R_sim, fail.flag = fail.flag)
  } else if (k.eval < 0) {
    k.eval <- abs(k.eval)
    registerDoParallel(cores = 12)
    R_sim <- foreach(i = 1:k.eval, .combine = rbind) %dopar% {
      sim_error_list <- sim_error(sim_index_mat, error_mat, r.lag, e.lag, h_factor, use.index_t)
      sim_error_mat <- sim_error_list$R_sim
      fail.flag <- fail.flag + sim_error_list$fail.flag
      #cat("fail.flag test 2", fail.flag, "\n")
      X_sim <- b * sim_index_mat[, c(1:d) + r.lag, drop = F] - fee
      X_sim <- X_sim + sim_error_mat[, c(1:d) + e.lag, drop = F]
      colnames(X_sim) <- paste(1:d)
      rownames(X_sim) <- rownames(sim_index_mat)
      if (roll.sim > 0) {
        X_sim <- do_findatamatroll(c(rep(0, r.lag), X_sim[1,]), roll.sim + r.lag)
        X_sim <- X_sim[, c(1:roll.sim) + r.lag]
      }
      bind_vector <- c(fail.flag, apply(1 + X_sim, 1, prod) - 1)
    }
    registerDoSEQ()
    stopImplicitCluster()
    list(R_sim = unname(R_sim[, -1]), fail.flag = sum(R_sim[, 1]))
  }
}


#
# run master sim for a given lag, and run and save KS test p values
#

do_lag_test <- function(myLETF = NULL, d = 1,
                        r.lag = 0, e.lag = 0,
                        r_0 = NULL, k = -1, h_factor = c(0.1, 0.01),
                        b = 3, annual.fee = 0.0095, k.eval = 1, roll.sim = T,
                        use.index_t = F)
{
#   myLETF <- "TECS"; d <- 252; r.lag <- 4; e.lag <- 4
#   r_0 <- NULL; k <- -1; h_factor <- c(0.01, 0.00001)
#   b <- -3; annual.fee <- 0.0095; k.eval <- -1000; roll.sim = F; use.index_t = T

  fee <- (1 + annual.fee) ^ (1 / 252) - 1
  myIndex <- subset(joindf_tickers,
                    letfl_tickers == myLETF | letfs_tickers == myLETF)$index_tickers
  
  letf_df <- do_findataframe(list(Index = index_trs[[myIndex]],
                                  LETF = c(letfl_trs, letfs_trs)[[myLETF]]),
                             missing = "Returns")
  
  index_ret <- do_findatamatroll(as.matrix(letf_df)[, "Index"], d + r.lag)
  letf_ret <- do_findatamatroll(as.matrix(letf_df)[, "LETF"], d + r.lag)
  levdex_ret <- (index_ret * b) - fee
  
  index_ret <- apply(index_ret[, (1 + r.lag):ncol(index_ret), drop = F], 1, rmc, b)
  letf_ret <- apply(1 + letf_ret[, (1 + r.lag):ncol(letf_ret), drop = F], 1, prod) - 1
  levdex_ret <- apply(1 + levdex_ret[, (1 + r.lag):ncol(levdex_ret), drop = F], 1, prod) - 1
    
  sim_master_list <- sim_master(myLETF, d,
                                r.lag = r.lag, e.lag = e.lag,
                                r_0 = r_0, k = k, h_factor = h_factor,
                                b = b, annual.fee = annual.fee, k.eval = k.eval,
                                roll.sim = roll.sim,
                                use.index_t = use.index_t)
  sim_df <- sim_master_list$R_sim
  cat("fail.flag test 3", sim_master_list$fail.flag, "\n")
  
  sim_pval <- sapply(1:nrow(sim_df), function(x) ks.test(letf_ret, sim_df[x, ])$p.value)
  #print(ks.test(letf_ret, levdex_ret))
  #print(summary(sim_pval))
  list(sim_pval = sim_pval,
       sim_case = c(ks.test(letf_ret, levdex_ret)$p.value, sim_master_list$fail.flag))
  
#   levdex_smc <- sapply(1:length(index_ret), function(x)
#                                               (1 + index_ret[x]) / (1 + levdex_ret[x]) - 1)
#   letf_smc <- sapply(1:length(index_ret), function(x)
#                                             (1 + index_ret[x]) / (1 + letf_ret[x]) - 1)
#   sim_smc <- sapply(1:length(index_ret), function(x)
#                                             (1 + index_ret[x]) / (1 + sim_df[, x]) - 1)
  
#   sim_pval <- sapply(1:nrow(sim_smc), function(x) ks.test(letf_smc, sim_smc[x, ])$p.value)
#   #print(ks.test(letf_smc, levdex_smc))
#   #print(summary(sim_pval))
#   list(sim_pval = sim_pval,
#        sim_case = c(ks.test(letf_smc, levdex_smc)$p.value, sim_master_list$fail.flag))
}

