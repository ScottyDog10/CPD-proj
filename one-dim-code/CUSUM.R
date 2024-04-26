n <- 1000
x <- rep(0, n)
tau <- floor(n/2)
#tau <- 1000
set.seed(1)
x[1:tau] <- rnorm(tau, -0.5, 1)
x[(tau+1):n] <- rnorm(n-tau, 0.5, 1)
x

plot(ts(x), xlab = 't', ylab = expression(X[t]), main = paste("Randomly Generated Values with Changepoint at t = ", tau))

cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")


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

max_cusum(x)

CUSUM(x, 62)


#t_values <- 2:(n-1)
t_values <- 1:n

x[1:500] = rnorm(500, 0, 1)
x[501:1000] = rnorm(500, 2, 1)
x[271] <- 12

plot(ts(x))
t_values <- 1:1000
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")

#which.max(cusum_results) + 1
which.max(cusum_results)

cusum_results

# Bootstrap algorithm for an input series x
bootstrap <- function(x, num_samples = 1000) {
  n <- length(x)
  set.seed(1)
  bootstrap_samples <- matrix(NA, nrow = num_samples, ncol = n)
  
  for (i in 1:num_samples) {
    # Generate a bootstrap sample by sampling with replacement
    bootstrap_sample <- sample(x, replace = TRUE)
    bootstrap_samples[i, ] <- bootstrap_sample
  }
  
  return(bootstrap_samples)
}


plot(ts(bootstrap(x)[3, ]), ylab = 'Bootstrap Samples')

max_cusum_results <- apply(bootstrap(x), 1, max_cusum)

c <- as.numeric(quantile(max_cusum_results, 0.95))

plot(density(max_cusum_results), main = 'Density of Maximal CUSUMs', xlab = 'Bootstrap Maximal CUSUM Samples')

abline(v = c, col = 'red')


generate_permutations <- function(x, num_permutations = 1000) {
  matrix(replicate(num_permutations, sample(x)), nrow = length(x), ncol = num_permutations)
}


max_cusum_results <- apply(generate_permutations(x), 1, max_cusum)

c <- as.numeric(quantile(max_cusum_results, 0.95))

plot(density(max_cusum_results), main = 'Density of Maximal CUSUMs', xlab = 'Permutated Maximal CUSUM Samples')

abline(v = c, col = 'red')

#will create very conservative results due to inclusion of non-null data
# if there is actually a change point hence we move on to following techniques


#both methods struggle when the change point is not centered, the threshold value
#is far too low and the max CUSUM is too low, #note for centered tau the threshold may be too strict

#Now let us try our method of only permuting and bootstrap sampling the data 
#before or after our change point:

n <- 1000
x <- rep(0, n)
tau <- 500
set.seed(1)
x[1:tau] <- rnorm(tau, -4, 1)
x[(tau+1):n] <- rnorm(n-tau, 4, 1)
x

plot(ts(x), xlab = 't', ylab = expression(X[t]), main = "Randomly Generated Values with Change Point at t = 500",
     col = 'darkgreen')

#we note that the point with max CUSUM is 850 out of the 1000. So we could try
#finding the threshold from the first 850 points where there is likely to be no change
#(rather than the last fifty as the sample size is too small)


max_cusum_results <- apply(x, 1, max_cusum)

t_values <- 1:n
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))
plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")

#which.max(cusum_results) + 1
which.max(cusum_results)

max(cusum_results[-1000])

c <- as.numeric(quantile(max_cusum_results, 0.95))

plot(density(max_cusum_results), main = 'Density of Maximal CUSUMs', xlab = 'Bootstrap Maximal CUSUM Samples')

abline(v = c, col = 'red')

#we note we now exceed the threshold using bootstrap

max_cusum_results <- apply(generate_permutations(x[1:850]), 1, max_cusum)

c <- as.numeric(quantile(max_cusum_results, 0.95))

plot(density(max_cusum_results), main = 'Density of Maximal CUSUMs', xlab = 'Permutated Maximal CUSUM Samples')

abline(v = c, col = 'red')
 #still exceed it using the permuted data

#investigate with centered tau if using LHS or RHS results in similar results to whole

plot(ts(bootstrap(x[1:500])[3, ]))

max_cusum_results <- apply(bootstrap(x[1:500]), 1, max_cusum)

plot(density(max_cusum_results))

quantile(max_cusum_results, 0.95)

#this threshold is a lot lower

#the threshold works well when we simulate the known first half of the data 

n <- 1000
x <- rep(0, n)
tau <- 1000
set.seed(1)
X <- matrix(NA, nrow = 1000, ncol = 1000)
for(i in 1:1000){
  X[i, ] <- rnorm(1000, -4, 1)
}
x

max_cusum_results <- apply(X, 1, max_cusum)

plot(density(max_cusum_results))

quantile(max_cusum_results, 0.95)


#three change points:


n <- 1000
x <- rep(0, n)
tau <- 250
tau2 <- 750
set.seed(1)
x[1:tau] <- rnorm(tau, -4, 1)
x[(tau+1):tau2] <- rnorm(tau2-tau, 4, 1)
x[(tau2+1):n] <- rnorm(n-tau2, 0, 1)

plot(ts(x), xlab = 't', ylab = expression(X[t]), main = paste("Randomly Generated Values with Changepoint at t = ", tau))

max_cusum(x)

CUSUM(x, 62)


#t_values <- 2:(n-1)
t_values <- 1:n
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")

#which.max(cusum_results) + 1
which.max(cusum_results)

cusum_results

# Bootstrap algorithm for an input series x
bootstrap <- function(x, num_samples = 1000) {
  n <- length(x)
  set.seed(1)
  bootstrap_samples <- matrix(NA, nrow = num_samples, ncol = n)
  
  for (i in 1:num_samples) {
    # Generate a bootstrap sample by sampling with replacement
    bootstrap_sample <- sample(x, replace = TRUE)
    bootstrap_samples[i, ] <- bootstrap_sample
  }
  
  return(bootstrap_samples)
}


plot(ts(bootstrap(x)[3, ]), ylab = 'Bootstrap Samples')

t_values <- 1:n
cusum_results <- sapply(t_values, function(t_val) CUSUM(bootstrap(x)[3, ], t_val))


plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")


max_cusum_results <- apply(bootstrap(x), 1, max_cusum)

c <- as.numeric(quantile(max_cusum_results, 0.95))

plot(density(max_cusum_results), main = 'Density of Maximal CUSUMs', xlab = 'Permutated Maximal CUSUM Samples')

abline(v = c, col = 'red')


