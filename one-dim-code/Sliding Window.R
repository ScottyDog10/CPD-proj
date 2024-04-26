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

set.seed(1)
x <- rep(NA, 1000)
x[1:500] = rnorm(500, 0, 1)
x[501:1000] = rnorm(500, 2, 1)
x[271] <- 12

plot(ts(x))
t_values <- 1:1000
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")

#which.max(cusum_results) + 1
which.max(cusum_results)

#these are the CUSUMs for the entire interval

plot(ts(x[200:300]))

cusum_results <- sapply(1:100, function(t_val) CUSUM(x[200:300], t_val))

plot(ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")

#which.max(cusum_results) + 1
which.max(cusum_results)

n <- 100
x <- rep(NA, n)
x[1:(n/2)] = 0
x[(n/2+1):n] = 2
plot(ts(x))
t_values <- 1:n
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))
max(cusum_results)

max_cusum_values <- numeric()

n_values <- seq(100, 1000, by = 1)
for (n in n_values) {
  x <- rep(NA, n)
  x[1:round((n/2))] <- 0
  x[(round(n/2)+1):n] <- 2
  max_cusum_value <- max(sapply(1:n, function(t_val) CUSUM(x, t_val)))
  max_cusum_values <- c(max_cusum_values, max_cusum_value)
  print(n)
}
plot(n_values, max_cusum_values, type = "l", col = "blue", xlab = "Length of Time Series", ylab = "Maximum CUSUM Value", main = "Maximum CUSUM ")


sliding_window_algorithm <- function(x, c, plot = FALSE) {
  n <- length(x)
  a <- 1
  i <- 2
  hat_T <- c()
  total_RSS <- c()
  
  while (i < n) {
    S <- c(a)
    RSS <- sum((x[S] - mean(x[S]))^2)
    
    while (RSS < c && i <= n) {
      m <- mean(x[S])
      S <- c(S, i)
      RSS <- sum((x[S] - m)^2)
      i <- i + 1
      total_RSS <- c(total_RSS, RSS)
    }
    
    hat_T <- c(hat_T, i - 2)
    print(hat_T)
    a <- i - 1
    print(a)
  }
  run_lengths <- 1:hat_T[1]
  for(k in 2:length(hat_T)){
    run_lengths <- c(run_lengths, 1:(hat_T[k]-hat_T[k-1]))
  }
  if (length(hat_T) > 1 && plot) {
    plot(run_lengths, xlab = "Time", ylab = "",cex = 1,
         main = "", xlim = c(1, n), pch = 20)
    lines(1:length(total_RSS), total_RSS, col = "red")
    legend("topleft", legend = c("Total Run Length", "Total RSS"), col = c("black", "red"), lty = 1)
  }
  hat_T <- hat_T[-length(hat_T)]
  return(hat_T)
}


set.seed(123)
x <- rep(NA, 1000)
x[1:200] = rnorm(200, 0, 0.5)
x[201:300] = rnorm(100, -1, 0.5)
x[301:650] = rnorm(350, 2, 0.5)
x[651:1000] = rnorm(350, -2, 0.5)
#x[271] <- 12

plot(ts(x))

threshold <- 30

x <- libor_1m$X1M_LIBOR
result <- sliding_window_algorithm(x, threshold, TRUE)

print(result)
abline(h = threshold, col = 'blue', lty = 2)

lines(7*ts(x), col = 'darkgreen')
dev.off()

libor_1m$date[result]

threshold <- 150

x1 <- rep(2, 50)
x2 <- rep(4, 50)

x <- c(x1, x2)

median_absolute_deviation <- function(x) {
  med <- median(x)
  mad <- median(abs(x - med))
  return(mad)
}

median_absolute_deviation(x)

sd(scaled_data)
median_data <- median(x)
mad_data <- median_absolute_deviation(x)
centered_data <- x - median_data
scaled_data <- centered_data / mad_data

plot(ts(scaled_data))
result <- sliding_window_algorithm(x, 2, TRUE)
print(result)

n <- length(x)
a <- 1
i <- 2
hat_T <- c()
total_RSS <- c()

while (i < n) {
  S <- c(a)
  RSS <- sum((x[S] - mean(x[S]))^2)
  
  while (RSS < c && i <= n) {
    m <- mean(x[S])
    S <- c(S, i)
    RSS <- sum((x[S] - m)^2)
    print(paste('iteration', i))
    print(paste('mean', m))
    print(paste('x values', x[S]))
    print(RSS)
    i <- i + 1
    total_RSS <- c(total_RSS, RSS)
  }
  
  hat_T <- c(hat_T, i - 2)
  print(hat_T)
  a <- i - 1
  print(a)
}
run_lengths <- 1:hat_T[1]
for(k in 2:length(hat_T)){
  run_lengths <- c(run_lengths, 1:(hat_T[k]-hat_T[k-1]))
}


