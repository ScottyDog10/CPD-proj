#Simulated Data Set-up:

#to test our different algorithms, we will need to
#generate several different types of time series data.

#1. One CP
#2. CP close to one another
#3. A CP in a bigger CP
#4. CP that cancel each other out
#5. CP near the tails of the time series

gen_ts <- function(tau, mean, sd = 1){
  set.seed(1)
  x <- c()
  for(i in 1:(length(tau)-1)){
    m <- mean[i]
    len <-tau[i+1] - tau[i] 
    val <- rnorm(len, m, sd)
    x <- c(x, val)
  }
  return(x)
}


plot_with_segments <- function(x, tau, mean, title = 'Simulated Time Series') {
  plot(x, type = 'l', main = title, xlab = 't', ylab = expression(x[t]), col = 'purple')
  abline(v = tau, col = 'black', lwd = 2)
  
  for (i in 1:(length(x)-1)) {
    tau_start <- tau[i]
    tau_end <- tau[i+1]
    h_value <- mean[i]
    segments(tau_start, h_value, tau_end, h_value, col = 'black', lwd = 2, lty = 2)
  }
}

plot_without_segments <- function(x, tau, mean, title = 'Simulated Time Series', cex.main = 1) {
  plot(x, type = 'l', main = title, xlab = 't', ylab = expression(x[t]), cex.main = 1)
}


#1. one change point

tau <- c(0, 500, 1000)
mean <- c(-4, 4)

x <- gen_ts(tau, mean)

plot_with_segments(x, tau, mean)

plot(density(x))


tau <- seq(from = 0, to = 1000, by = 100)

mean_pool <- seq(-10, 10, by = 2)

set.seed(1)
mean <- sample(mean_pool, 10, replace = TRUE)

x <- gen_ts(tau, mean)

plot_with_segments(x, tau, mean)

#jumps too large, too easy

plot(density(x))

#2. lots of change points close to one another

#large number of change points close to each other following a markov chain 

gen_mc <- function(m, n, d) {
  i <- 1
  lst <- c(m)
  set.seed(1)
  while (i < n+1) {
    prob <- runif(1)
    
    if (prob > 0.5) {
      m <- m + d
    } else {
      m <- m - d
    }
    
    lst <- c(lst, m)
    i <- i + 1
  }
  
  return(lst)
}

tau <- seq(from = 0, to = 1000, by = 100)

mean <- gen_mc(2, 10, 1)

x <- gen_ts(tau, mean, sd = 1)

plot_with_segments(x, tau, mean)


#3. A CP in a bigger CP

tau <- c(0, 250, 750, 1000)

mean <- c(0, 4, 0)

x <- gen_ts(tau, mean, sd = 1)

plot_with_segments(x, tau, mean)

#4. CPs that cancel each other out:

tau <- c(0, 500, 550, 600, 1000)
mean <- c(0, 2, -2, 0)

x <- gen_ts(tau, mean, sd = 1)

plot_with_segments(x, tau, mean)

#5.CP near the tails of the time series

tau <- c(0, 50, 950, 1000)
mean <- c(0, 2, 4)

x <- gen_ts(tau, mean, sd = 1)

plot_with_segments(x, tau, mean)

#6. For poster

tau <- c(0, 400, 650, 800, 1000)
mean <- c(0, 1, 0.5, -1)

x <- gen_ts(tau, mean, sd = 0.25)

plot(x, type = 'l', main = 'Simulated Time Series', xlab = 't', ylab = expression(x[t]))


tau <- c(0, 500, 1000)
mean <- c(-4, 4)

x <- gen_ts(tau, mean)

plot_with_segments(x, tau, mean)

?t.test
t.test()

t.values <- c()
p.values <- c()
n <- length(x)
for(t in 1:(n-1)){
  t.values[t] <- t.test(x = x[1:t],
                        y = x[t+1:n],
                        alternative = 'two.sided',
                        var.equal = TRUE,
                        conf.level = 0.95)$statistic^2
  p.values[t] <- t.test(x = x[1:t],
                        y = x[t+1:n],
                        alternative = 'two.sided',
                        var.equal = TRUE,
                        conf.level = 0.95)$p.value
}

t_test <- function(x) {
  t.values <- numeric(length(x) - 1)
  p.values <- numeric(length(x) - 1)
  
  n <- length(x)
  
  for (t in 1:(n - 1)) {
    t_result <- t.test(x = x[1:t],
                       y = x[(t + 1):n],
                       alternative = 'two.sided',
                       var.equal = TRUE,
                       conf.level = 0.95)
    t.values[t] <- t_result$statistic^2
    p.values[t] <- t_result$p.value
  }
  
  return(t.values)
}

plot(ts(t.values))

that <- which.max(t.values)

max.t <- c()
for(i in 1:500){
  permutation <- sample(x)
  max.t[i] <- which.max(t_test(permutation))
}

quantile(max.t, 0.95)

max(t_test(x[1:570]))


tau <- c(0, 250, 750, 1000)
mean <- c(-4, 4, 2)

x <- gen_ts(tau, mean)

plot_with_segments(x, tau, mean)

t.values <- c()
p.values <- c()
df.values <- c()
n <- length(x)
for(t in 1:(n-1)){
  t.values[t] <- t.test(x = x[1:t],
                        y = x[t+1:n],
                        alternative = 'two.sided',
                        var.equal = TRUE,
                        conf.level = 0.95)$statistic
  p.values[t] <- t.test(x = x[1:t],
                        y = x[t+1:n],
                        alternative = 'two.sided',
                        var.equal = TRUE,
                        conf.level = 0.95)$p.value
  df.values[t] <- t.test(x = x[1:t],
                         y = x[t+1:n],
                         alternative = 'two.sided',
                         var.equal = TRUE,
                         conf.level = 0.95)$parameter
}

plot(ts(t.values))
abline(h = 998*qt(0.05, df.values[1]))


df.values
abline(qt(0.95, df.values[1]))


t.values <- c()
p.values <- c()
n <- length(x)
for(t in 250:(n-1)){
  t.values[t] <- (t.test(x = x[250:t],
                         y = x[t+1:n],
                         alternative = 'two.sided',
                         var.equal = TRUE,
                         conf.level = 0.95)$statistic)^2
  p.values[t] <- t.test(x = x[1:t],
                        y = x[t+1:n],
                        alternative = 'two.sided',
                        var.equal = TRUE,
                        conf.level = 0.95)$p.value
}

plot(ts(t.values))

which.max(t.values)

install.packages("extRemes")
library(extRemes)

fit <- fevd(t.values)

?fevd
