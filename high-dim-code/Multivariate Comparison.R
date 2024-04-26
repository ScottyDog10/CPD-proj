set.seed(42)

#change in distribution:
gamma_variables <- rgamma(500, 2, 1)
exponential_variables <- rexp(500, rate = 1/2)
head(normal_variables)
head(exponential_variables)


plot(ts(c(gamma_variables, exponential_variables)))

#change in distribution:

plot(ts(c(rchisq(500, 11), rchisq(500, 15))))


plot(ts(c(rt(500, df = 4), rnorm(500, 0, sd=1))))

generate_mixed_distribution <- function(n, changepoint, seed) {
  set.seed(seed)
  t_variables <- rt(n, df = 4)
  normal_variables <- rnorm(n, mean = 0, sd = 1)
  mixed_distribution <- c(t_variables[1:changepoint], normal_variables[(changepoint + 1):n])
  
  return(mixed_distribution)
}


plot(ts(generate_mixed_distribution(1000, 750, 1)))

generate_mixed_distribution <- function(n, changepoint, seed) {
  set.seed(seed)
  t_variables <- rt(n, df = 4)
  normal_variables <- rnorm(n, mean = 0, sd = 1)
  mixed_distribution <- c(t_variables[1:changepoint], normal_variables[(changepoint + 1):n])
  
  return(mixed_distribution)
}

num_distributions <- 10
n <- 1000
changepoint <- 750 

time_series <- matrix(0, nrow = n, ncol = num_distributions)

for(i in 1:num_distributions){
  time_series[, i] <- generate_mixed_distribution(n, changepoint, i)
}

par(mfrow = c(5, 2))
plot(ts(time_series[, 1]), col = 1)
for (i in 2:num_distributions) {
  plot(time_series[, i], type = 'l', col = i)
}

install.packages('ecp')
library(ecp)
e.divisive(time_series, sig.lvl = 0.05, R = 199, min.size = 200, alpha = 1)
 
#change in mean:

