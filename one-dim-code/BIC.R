#Implementing BIC for Threshold Selection

CUSUM <- function(x, t){
  n <- length(x)
  cusum <- ((t*(n-t))/n)^(1/2)*abs(mean(x[1:t]) - mean(x[(t+1):n]))
  if(t==n){
    cusum <- 0
    return(cusum)
  }
  else{
    cusum <- as.numeric(cusum)
    return(cusum)
  }
}

max_cusum_defunct <- function(x, k) {
  cusum_val <- rep(0, length(x) - 1)
  for(t in 0:(length(x))){
    cusum_val[t] <- CUSUM(x, t)
  }
  return(list(max(cusum_val), which.max(cusum_val)+k))
}


max_cusum <- function(x, k) {
  t_values <- 1:length(x)
  cusum_val <- sapply(t_values, function(t_val) CUSUM(x, t_val))
  if(k == 0){
    return(list(max(cusum_val), which.max(cusum_val)+k))
  }
  else{
    return(list(max(cusum_val), which.max(cusum_val)+k-1)) #shifts change point to correct position when start of interval not zero
  }
}

bs_N <- function(x, k, q, N){
  tau_lst <- c(k, q)
  j <- 0
  bool <- c()
  cusum <- c(NA, NA)
  df <- data.frame(tau = tau_lst, cusum = cusum)
  
  intervals <- create_intervals(tau_lst)
  while(j != 1){
    for(i in 1:nrow(intervals)){
      k <- intervals[i, 1]
      q <- intervals[i, 2]
      tau_hat <- max_cusum(x[k:q], k)[[2]]
      if(!(tau_hat %in% c(k, q))){
        # Check for duplicates in df$tau
        duplicate_idx <- which(df$tau == tau_hat)
        if (length(duplicate_idx) > 0) {
          # If duplicate exists, compare CUSUMs and keep the one with the bigger CUSUM
          max_cusum_idx <- which.max(df$cusum[duplicate_idx])
          if (df$cusum[duplicate_idx[max_cusum_idx]] < max_cusum(x[k:q], k)[[1]]) {
            df <- df[-duplicate_idx[max_cusum_idx], ]  # Remove the row with smaller CUSUM
            df <- rbind(df, data.frame(tau = tau_hat, cusum = max_cusum(x[k:q], k)[[1]]))  # Add the new tau and its CUSUM
          }
        } else {
          df <- rbind(df, data.frame(tau = tau_hat, cusum = max_cusum(x[k:q], k)[[1]]))
        }
        # Sort df by tau and remove duplicates
        df <- df[order(df$tau, decreasing = FALSE), ]
        df <- df[!duplicated(df$tau), ]
      }
      bool <-!(tau_hat %in% c(k, q))
    }
    intervals <- create_intervals(df$tau)  # Update intervals using df$tau
    all_false <- all(bool == FALSE)
    if(length(df$tau) >= N+2){
      j <- 1
    }
    bool <- c()
  }
  df <- df[c(-1, -nrow(df)), ]
  df <- df[order(df$cusum, decreasing = TRUE), ]
  df <- df[1:N, ]
  df <- rbind(df, c(1, NA))
  df <- rbind(df, c(length(x), NA))
  df <- df[order(df$tau, decreasing = FALSE), ]
  return(df)
}


normal_log_likelihood <- function(data, mu, sigma) {
  n <- length(data)
  ll <- -(n/2) * log(2*pi) - (n/2) * log(sigma^2) - (1/(2*sigma^2)) * sum((data - mu)^2)
  return(ll)
}

calculate_log_likelihood <- function(x, changepoints, sigma) {
  n <- length(changepoints)
  log_likelihood <- 0
  start_index <- 1
  
  for (i in 1:n) {
    if (i < n) {
      segment <- x[start_index:changepoints[i]]
    } else {
      segment <- x[start_index:length(x)]
    }
    segment_mean <- mean(segment)
    log_likelihood <- log_likelihood + normal_log_likelihood(segment, mu = segment_mean, sigma = sigma)
    start_index <- changepoints[i] + 1
  }
  
  return(log_likelihood)
}

BIC <- function(x, k, q, N){
  changepoints <-bs_N(x, k, q, N)$tau
  result <- -2*calculate_log_likelihood(x, changepoints, 1) + length(changepoints)*log(length(x))
  return(list(result, length(changepoints) -2))
}

AIC <- function(x, k, q, N){
  changepoints <- bs_N(x, k, q, N)$tau
  log_likelihood <- calculate_log_likelihood(x, changepoints, 1)
  result <- -2 * log_likelihood + 2 * (length(changepoints) - 2)
  return(result)
}

set.seed(1)
x <- rep(NA, 1000)
x[1:100] <- rnorm(100, -4, 1)
x[101:1000] <- rnorm(900, 4, 1)

plot(ts(x))

BIC(x, 1, 1000, 3)

set.seed(1)
x <- rep(NA, 1000)
x[1:10] <- rnorm(10, -4, 1)
x[11:1000] <- rnorm(990, 4, 1)


plot(ts(x), col = 'darkgreen', ylab = 'X', main = 'Gaussian Simulation')


t_values = 1:1000
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM with Change Near Boundary")


set.seed(1)
x[1:500] <- rnorm(500, -4, 30)
x[501:1000] <- rnorm(500, 4, 30)
plot(ts(x), col = 'darkgreen', ylab = 'X', main = 'Gaussian Simulation')
t_values = 1:1000
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))
plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM with Change Near Boundary")


plot(ts(x), col = 'darkgreen', ylab = 'X', main = 'Gaussian Simulation')

t_values = 1:1000
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM with High Gaussian Error")

create_intervals <- function(vec){
  
  df <- data.frame(rep(NA, length(vec)-1), rep(NA, length(vec)-1))
  colnames(df) <- list('Start', 'End')
  for(i in 1:length(vec)-1){
    df[i, ] <- c(vec[i], vec[i+1])
  }
  return(df)
}


bs <- function(x, k, q, c){
  tau_lst <- c(k, q)
  j <- 0
  bool <- c()
  intervals <- create_intervals(tau_lst)
  while(j != 1){
    for(i in 1:nrow(intervals)){
      k <- intervals[i, 1]
      q <- intervals[i, 2]
      tau_hat <- max_cusum(x[k:q], k)[[2]]
      if(max_cusum(x[k:q], k)[[1]] > c & !(tau_hat %in% c(k, q))){
        tau_lst <- sort(unique(c(tau_lst, tau_hat)))
      }
      bool <- c(bool, max_cusum(x[k:q], k)[[1]] > c & !(tau_hat %in% c(k, q)))
    }
    intervals <- create_intervals(tau_lst)
    all_false <- all(bool == FALSE)
    if(all_false == TRUE){
      j <- 1
    }
    bool <- c()
  }
  return(tau_lst)
}


set.seed(1)
x <- rep(NA, 1000)
x[1:200] = rnorm(200, 0, 1)
x[201:300] = rnorm(100, -1, 1)
x[301:650] = rnorm(350, 2, 1)
x[651:1000] = rnorm(350, -1.5, 1)
#x[271] <- 12

plot(ts(x), col = 'darkgreen', ylab = 'X', main = 'Gaussian Simulation')

plot(ts(x), col = 'darkgreen', ylab = 'X', main = 'Gaussian Simulation')

bs_N(x, 1, 1000, 7)



#cannot return exactly 10.


#threshold of 6 produces 3 change points

bs(x, 1, 1000, 6)

#threshold of 10 produces 2 change points

bs(x, 1, 1000, 10)

#threshold of 35 produces 1 change point

bs(x, 1, 1000, 35)

#next significant increase is at threshold = 1.5

bs(x, 1, 1000, 1.5)

#log likelihood for 2 changepoints:

l1 <- normal_log_likelihood(x[1:300], mu = mean(x[1:300]), 1)
l2 <- normal_log_likelihood(x[301:650], mu = mean(x[301:650]), 1)
l3 <- normal_log_likelihood(x[651:1000], mu = mean(x[651:1000]), 1)

loglik_2 <- l1+l2+l3


#log likelihood for 3 changepoints: 

# Calculate log likelihood for each segment
l1 <- normal_log_likelihood(x[1:203], mu = mean(x[1:203]), sigma = 1)
l2 <- normal_log_likelihood(x[204:300], mu = mean(x[204:300]), sigma = 1)
l3 <- normal_log_likelihood(x[301:650], mu = mean(x[301:650]), sigma = 1)
l4 <- normal_log_likelihood(x[651:1000], mu = mean(x[651:1000]), sigma = 1)

# Sum up log likelihoods for all segments
loglik_3 <- l1 + l2 + l3 + l4

# log likelihood for 1 changepoint

l1 <- normal_log_likelihood(x[1:650], mu = mean(x[1:650]), sigma = 1)
l2 <- normal_log_likelihood(x[651:1000], mu = mean(x[651:1000]), sigma = 1)

logkile_1 <- l1+l2


BIC(x, 1, 1000, 3)
bs_N(x, 1, 1000, 3)


segment_counts <- 1:30

BIC_values <- sapply(segment_counts, function(N) {
  BIC_result <- BIC(x, 1, 1000, N)
  return(BIC_result)
})

Y <- BIC_values[1, ]
X <- BIC_values[2, ]


plot(X, Y, xlab = "Number of Change Points", ylab = "BIC Value", type = "l", col = "blue", lwd = 2)

X[which.min(Y)]

plot(X, Y, xlab = "Number of Change Points", ylab = "BIC Value", type = "l", col = "blue", lwd = 2)

min_index <- which.min(Y)
min_arg <- X[min_index]

points(min_arg, Y[min_index], col = "red", pch = 19)

text(min_arg, Y[min_index], min_arg, pos = 3, col = 'red')

plot(X, Y, xlab = "Number of Change Points", ylab = "BIC Value", type = "l", col = "blue", lwd = 2)

min_index <- which.min(Y)
min_arg <- X[min_index]

points(min_arg, Y[min_index], col = "red", pch = 19)

text(min_arg, Y[min_index], min_arg, pos = 3, col = 'red')
dev.off()

##################################################################



