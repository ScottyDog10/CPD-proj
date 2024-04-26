library(ggthemes)
library(ggplot2)
library(MASS)

gen_timeseries <- function(N, n, m, s, seed){
  set.seed(seed)
  change_points <- sort(sample(2:(n-1), N))
  change_points <- c(change_points, c(1, n))
  change_points <- sort(change_points)
  mus <- rnorm(N+1, m, s)
  x <- rep(NA, n)
  x[1:change_points[2]] <- rnorm(change_points[2], mus[1], 1)
  for(i in 2:(length(change_points)-1)){
    x[change_points[i]:change_points[i+1]] <- rnorm(change_points[i+1]-(change_points[i]-1), mus[i], 1)
  }
  return(list(data = x, change_points=change_points, mus = mus))
}

result <- gen_timeseries(N = 30, n = 1000, m = 3, s = 2, seed = 1)

plot(ts(result$data))

#SPARSITY CHANGE

#independent change points:

num_iterations <- 5

results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  result <- gen_timeseries(N = 30, n = 1000, m = 3, s = 2, seed = i)
  results[[i]] <- result$data
  taus[[i]] <- result$change_points
  mus[[i]] <- result$mus
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

results_df$time <- 1:nrow(results_df)
results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)
ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

#4 changepoints in common:

gen_seriessparsity <- function(N, n, m, s, seed, changes){
  set.seed(seed)
  mus <- rnorm(N+1, m, s)
  N <- N - length(changes)
  change_points <- sort(sample(2:(n-1), N))
  change_points <- c(change_points, c(1, n), changes)
  change_points <- sort(change_points)
  x <- rep(NA, n)
  x[1:change_points[2]] <- rnorm(change_points[2], mus[1], 1)
  for(i in 2:(length(change_points)-1)){
    x[change_points[i]:change_points[i+1]] <- rnorm(change_points[i+1]-(change_points[i]-1), mus[i], 1)
  }
  return(list(data = x, change_points=change_points, mus = mus))
}

result <- gen_seriessparsity(N = 30, n = 1000, m = 3, s = 2, seed = 1, four_fixed)

result$change_points
plot(ts(result$data))

num_iterations <- 5

set.seed(1)
four_fixed <- sample(2:(1000-1), 4)

results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  result <- gen_seriessparsity (N = 30, n = 1000, m = 3, s = 2, seed = i, four_fixed)
  results[[i]] <- result$data
  taus[[i]] <- result$change_points
  mus[[i]] <- result$mus
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

common_changepoints <- taus_df[[1]]

for (i in 2:ncol(taus_df)) {
  common_changepoints <- intersect(common_changepoints, taus_df[[i]])
}

common_changepoints

results_df$time <- 1:nrow(results_df)
results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)

ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()


#8 changes in common:

num_iterations <- 5

set.seed(1)
eight_fixed <- sample(2:(1000-1), 8)

results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  result <- gen_seriessparsity (N = 30, n = 1000, m = 3, s = 2, seed = i, eight_fixed )
  results[[i]] <- result$data
  taus[[i]] <- result$change_points
  mus[[i]] <- result$mus
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

common_changepoints <- taus_df[[1]]
for (i in 2:ncol(taus_df)) {
  common_changepoints <- intersect(common_changepoints, taus_df[[i]])
}

common_changepoints
results_df$time <- 1:nrow(results_df)
results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)
ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()


#16 changes in common:

num_iterations <- 5

set.seed(1)
sixteen_fixed <- sample(2:(1000-1), 16)

sort(sixteen_fixed)
results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  result <- gen_seriessparsity (N = 30, n = 1000, m = 3, s = 2, seed = i, sixteen_fixed)
  results[[i]] <- result$data
  taus[[i]] <- result$change_points
  mus[[i]] <- result$mus
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

common_changepoints <- taus_df[[1]]

for (i in 2:ncol(taus_df)) {
  common_changepoints <- intersect(common_changepoints, taus_df[[i]])
}
common_changepoints
results_df$time <- 1:nrow(results_df)
results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)
ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

####################################################################

#Change in Distribution:


generate_mixed_distribution <- function(n, changepoint, seed) {
  set.seed(seed)
  
  gamma_variables <- rgamma(n, 2, 1)
  
  exponential_variables <- rexp(n, rate = 1/2)
  mixed_distribution <- c(gamma_variables[1:changepoint], exponential_variables[(changepoint + 1):n])
  
  return(mixed_distribution)
}

num_distributions <- 5
n <- 1000
changepoint <- 420

time_series <- matrix(0, nrow = n, ncol = num_distributions)

for(i in 1:num_distributions){
  time_series[, i] <- generate_mixed_distribution(n, changepoint, i)
}

par(mfrow = c(5, 1))
plot(ts(time_series[, 1]), col = 1)
for (i in 2:num_distributions) {
  plot(time_series[, i], type = 'l', col = i)
}
time_series_df <- data.frame(time = rep(1:n, num_distributions),
                             value = as.vector(time_series),
                             distribution = factor(rep(1:num_distributions, each = n)))

ggplot(time_series_df, aes(x = time, y = value, color = distribution)) +
  geom_line() +
  scale_color_viridis_d() + 
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

ggplot(time_series_df, aes(x = time, y = value, color = distribution)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  facet_wrap(~distribution, scales = "free_y", nrow = 5) +
  theme_gdocs()

#########################################################################

#Aggregation method mean failure (outlier): one very large changepoint in one level:


gen_seriesagg <- function(N, n, m, s, seed, changes){
  set.seed(seed)
  mus <- rnorm(N+1, m, s)
  N_Og <- N
  N <- N - length(changes)
  change_points <- sort(sample(2:(n-1), N))
  change_points[round(N_Og/2)] <- sample(2:(n-1), 1)
  mus[round(N_Og/2)+1] <- rnorm(1, 5*m, s)
  change_points <- c(change_points, c(1, n), changes)
  change_points <- sort(change_points)
  x <- rep(NA, n)
  x[1:change_points[2]] <- rnorm(change_points[2], mus[1], 1)
  for(i in 2:(length(change_points)-1)){
    x[change_points[i]:change_points[i+1]] <- rnorm(change_points[i+1]-(change_points[i]-1), mus[i], 1)
  }
  return(list(data = x, change_points=change_points, mus = mus))
}

num_iterations <- 5

set.seed(1)
four_fixed <- sample(2:(1000-1), 4)
results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  if(i != 3){
    result <- gen_seriessparsity (N = 10, n = 1000, m = 3, s = 2, seed = i, four_fixed)
    results[[i]] <- result$data
    taus[[i]] <- result$change_points
    mus[[i]] <- result$mus
  }
  else{
    result <- gen_seriesagg (N = 10, n = 1000, m = 3, s = 2, seed = i, four_fixed)
    results[[i]] <- result$data
    taus[[i]] <- result$change_points
    mus[[i]] <- result$mus
  }
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

common_changepoints <- taus_df[[1]]

for (i in 2:ncol(taus_df)) {
  common_changepoints <- intersect(common_changepoints, taus_df[[i]])
}
common_changepoints
results_df$time <- 1:nrow(results_df)
results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)
ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() + 
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

#####################################################

#accuracy as p increases:

#p=5
num_iterations <- 5

set.seed(1)
eight_fixed <- sample(2:(1000-1), 8)
results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  result <- gen_seriessparsity (N = 30, n = 1000, m = 3, s = 2, seed = i, eight_fixed )
  results[[i]] <- result$data
  taus[[i]] <- result$change_points
  mus[[i]] <- result$mus
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

common_changepoints <- taus_df[[1]]
for (i in 2:ncol(taus_df)) {
  common_changepoints <- intersect(common_changepoints, taus_df[[i]])
}
common_changepoints
results_df$time <- 1:nrow(results_df)

results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)

ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

#p = 10

num_iterations <- 10

set.seed(1)
eight_fixed <- sample(2:(1000-1), 8)

results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  result <- gen_seriessparsity (N = 30, n = 1000, m = 3, s = 2, seed = i, eight_fixed )
  results[[i]] <- result$data
  taus[[i]] <- result$change_points
  mus[[i]] <- result$mus
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

common_changepoints <- taus_df[[1]]

for (i in 2:ncol(taus_df)) {
  common_changepoints <- intersect(common_changepoints, taus_df[[i]])
}

common_changepoints

results_df$time <- 1:nrow(results_df)

results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)
ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() + 
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

#p = 20
num_iterations <- 20

set.seed(1)
eight_fixed <- sample(2:(1000-1), 8)
results <- list()
taus <- list()
mus <- list()

for (i in 1:num_iterations) {
  result <- gen_seriessparsity (N = 30, n = 1000, m = 3, s = 2, seed = i, eight_fixed )
  results[[i]] <- result$data
  taus[[i]] <- result$change_points
  mus[[i]] <- result$mus
}

results_df <- as.data.frame(do.call(cbind, results))
taus_df <- as.data.frame(do.call(cbind, taus))
mus_df <- as.data.frame(do.call(cbind, mus))

common_changepoints <- taus_df[[1]]
for (i in 2:ncol(taus_df)) {
  common_changepoints <- intersect(common_changepoints, taus_df[[i]])
}

common_changepoints

results_df$time <- 1:nrow(results_df)

results_melted <- reshape2::melt(results_df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)
ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

#interesting to compare treating time series individually then locating common
#change points.
#################################################################

#Simulated time correlated time series (between levels)

library(corrplot)

#8 changes in common of 10:

#works for five variables

gen_corrts <- function(n, m, Sigma, sparsity_index, total_changes, seed){
  # 
  # n <- 1000
  # m <- c(-2, 4, 1, 3, 2)
  # Sigma <- cov_matrix
  # sparsity_index <- 8
  # total_changes<- 10
  # seed <- 1
  
  set.seed(seed)
  N <- sparsity_index 
  means <- MASS::mvrnorm(n = N+1, mu = m, Sigma )
  change_points <- sort(sample(2:(n-1), N))
  change_points <- c(change_points, c(1, n))
  change_points <- sort(change_points)
  cps <- change_points
  means <- rbind(means, tail(means, 1))
  change_points_df <- data.frame(change_points)
  means_change_points <- cbind(change_points_df, means)
  
  rownames(means_change_points) <- NULL
  
  colnames(means_change_points) <- c("change_points", "X1", "X2", "X3", "X4", "X5")
  
  dif <- total_changes-sparsity_index
  v1_randcp <- sort(sample(2:(n-1), dif))
  v2_randcp <- sort(sample(2:(n-1), dif))
  v3_randcp <- sort(sample(2:(n-1), dif))
  v4_randcp <- sort(sample(2:(n-1), dif))
  v5_randcp <- sort(sample(2:(n-1), dif))
  
  means_vector <- m
  v1_extmeans <- rnorm(dif, means_vector[1], 2)
  v2_extmeans <- rnorm(dif, means_vector[2], 2)
  v3_extmeans <- rnorm(dif, means_vector[3], 2)
  v4_extmeans <- rnorm(dif, means_vector[4], 2)
  v5_extmeans <- rnorm(dif, means_vector[5], 2)
  
  v1_df <- data.frame("change_points" = v1_randcp, "X1" = v1_extmeans , "X2" = rep(NA, 2), "X3" = rep(NA, 2), 
                      "X4" = rep(NA, 2), "X5" = rep(NA, 2))
  v2_df <- data.frame("change_points" = v2_randcp, "X1" = rep(NA, 2), "X2" = v2_extmeans , "X3" = rep(NA, 2), 
                      "X4" = rep(NA, 2), "X5" = rep(NA, 2))
  v3_df <- data.frame("change_points" = v3_randcp, "X1" = rep(NA, 2), "X2" = rep(NA, 2), "X3" = v3_extmeans , 
                      "X4" = rep(NA, 2), "X5" = rep(NA, 2))
  v4_df <- data.frame("change_points" = v4_randcp, "X1" = rep(NA, 2), "X2" = rep(NA, 2), "X3" = rep(NA, 2), 
                      "X4" = v4_extmeans , "X5" = rep(NA, 2))
  v5_df <- data.frame("change_points" = v5_randcp, "X1" = rep(NA, 2), "X2" = rep(NA, 2), "X3" = rep(NA, 2), 
                      "X4" = rep(NA, 2), "X5" = v5_extmeans)
  
  means_change_points <- rbind(means_change_points, v1_df, v2_df, v3_df, v4_df, v5_df)
  
  means_change_points <- means_change_points[order(means_change_points$change_points), ]
  
  library(zoo)
  
  for (i in 2:ncol(means_change_points)) {
    means_change_points[, i] <- na.locf(means_change_points[, i])
  }
  
  rownames(means_change_points) <- NULL
  
  means <- means_change_points[-nrow(means_change_points), -1]
  
  #dont care about last row
  
  means <- as.matrix(means)
  x <- matrix(NA, nrow = n, ncol = ncol(means))
  change_points <- means_change_points[, 1]
  for(i in 1:(length(change_points)-1)) {
    mvn_data <- MASS::mvrnorm(n = (change_points[i+1] - change_points[i])+1, mu = means[i, ], Sigma)
    x[change_points[i]:change_points[i+1], ] <- mvn_data
  }
  
  return(list('mu' = means, 'change_points' = means_change_points[, 1], 'data' = x, common_cp = cps))
}

cor_matrix <- matrix(c(1.0, 0.9, 0.8, 0.7, 0.6,
                       0.9, 1.0, 0.9, 0.8, 0.7,
                       0.8, 0.9, 1.0, 0.9, 0.8,
                       0.7, 0.8, 0.9, 1.0, 0.9,
                       0.6, 0.7, 0.8, 0.9, 1.0), nrow = 5, byrow = TRUE)

std_dev_vector <- c(2, 2, 2, 2, 2)

cov_matrix <- cor_matrix * (std_dev_vector %*% t(std_dev_vector))


gen_corrts(1000, c(-2, 4, 1, 3, 2), cov_matrix, 8, 10, 2)

result_df <- data.frame(gen_corrts(1000, c(-4, 5, 7, 1, 11), cov_matrix, 8, 10, 2)$data)
names(result_df) <- paste0("Variable_", 1:ncol(result_df))
result_df$time <- 1:n

df <- result_df
df$time <- 1:nrow(df)
results_melted <- reshape2::melt(df, id.vars = "time")
results_melted$variable <- as.factor(results_melted$variable)
ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

##########################################

#Sparsity:

#Change only occurs in 2 of the 10 series:


gen_sparsets <- function(n, m, s, sparsity_index, seed, p, cp, jump_size){
  set.seed(seed)
  data <- data.frame(matrix(0, nrow = n, ncol = 0)) 
  index <- sample(1:p, sparsity_index)
  if (sparsity_index < p) {
    for(i in 1:(p-sparsity_index)){
      set.seed(i)
      x <- rnorm(n, m[i], s)  
      data <- cbind(data, x)  
    }
  }
  change_points <- cp
  for(i in (p-sparsity_index+1):p){
    set.seed(i)
    sgn <- sample(c(-1, 1), 1)
    mus <- c(m[i], jump_size+sgn*m[i])
    x <- rep(NA, n)
    x[1:change_points[2]] <- rnorm(change_points[2], mus[1], 1)
    for(j in 2:(length(change_points)-1)){
      x[change_points[j]:change_points[j+1]] <- rnorm(change_points[j+1]-(change_points[j]-1), mus[2], 1)
    }
    data <- cbind(data, x) 
  }
  colnames(data) <- paste0("X", 1:ncol(data)) 
  return(data)
}

plot(ts(gen_sparsets(1000, c(-2, 3, 1, 4, 2, 6, -4, 2, 8, 2.7), 1, 10, 1, 10, c(1, 500, 1000), 30)))

#####################################################

#Taking mean of CUSUM (only do for one change point):

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

max_cusum <- function(x, k) {
  t_values <- 1:length(x)
  cusum_val <- sapply(t_values, function(t_val) CUSUM(x, t_val))
  if(k == 0){
    return(list(max(cusum_val), which.max(cusum_val)+k))
  }
  else{
    return(list(max(cusum_val), which.max(cusum_val)+k-1)) 
  }
}

x <- gen_sparsets(1000, c(-2, 3, 1, 4, 2, 6, -4, 2, 8, 2.7), 1, 10, 1, 10, c(1, 500, 1000), 1.2)
plot(ts(x))

cusum_df <- function(x){
  n <- nrow(x)
  p <- ncol(x)
  t_values <- 1:n 
  cs_df <- data.frame(matrix(0, ncol = p, nrow = n))
  for(j in 1:p){
    cs_df[, j] <- sapply(t_values, function(t_val) CUSUM(x[, j], t_val))
  }
  return(cs_df)
}

df <- rowMeans(cusum_df(x))

plot(ts(df))

which.max(df)


find_max_cusum_cp <- function(x) {
  cusum_df <- function(x) {
    n <- nrow(x)
    p <- ncol(x)
    t_values <- 1:n 
    cs_df <- data.frame(matrix(0, ncol = p, nrow = n))
    for(j in 1:p) {
      cs_df[, j] <- sapply(t_values, function(t_val) CUSUM(x[, j], t_val))
    }
    return(cs_df)
  }
  
  df <- rowMeans(cusum_df(x))
  plot(ts(df))
  return(which.max(df))
}

max_cusum_cp <- find_max_cusum_cp(x)
max_cusum_cp

#####################################################

library(ecp)
library(InspectChangepoint)

#Applying to simulation:

find_closest_element <- function(value, vector) {
  closest_element <- vector[which.min(abs(vector - value))]
  abs_distance <- abs(value - closest_element)
  
  result <- list(closest_element = closest_element, abs_distance = abs_distance)
  return(result)
}

calculate_true_positive_ratio <- function(true_change_points, detected_change_points, epsilon) {
  true_change_points <- true_change_points[-c(1, length(true_change_points))]
  detected_change_points <- detected_change_points[-c(1, length(detected_change_points))]
  
  true_positives <- 0
  for (i in 1:length(true_change_points)) {
    abs_distance <- find_closest_element(true_change_points[i], detected_change_points)$abs_distance
    
    if (abs_distance <= epsilon && length(detected_change_points) != 0) {
      closest_element <- find_closest_element(true_change_points[i], detected_change_points)$closest_element
      detected_change_points <- detected_change_points[detected_change_points != closest_element]
      true_positives <- true_positives + 1
    }
  }
  
  true_positive_ratio <- true_positives / length(true_change_points)
  return(true_positive_ratio)
}

epsilon_fun <- function(N, n) {
  epsilon <- 10 * exp(-0.01 * N * (1000 / n))
  return(round(epsilon))
}

##################################################

par(mfrow = c(1, 1))

#Dense Change Point:
set.seed(2) 
twenty_means <- runif(20, min = -5, max = 5)
twenty_means

par(mfrow = c(10, 2)) 
par(mar = c(2, 2, 1, 1))

x <- gen_sparsets(1000, twenty_means, 1, 20, 1, 20, c(1, 500, 1000), 1.2)
for (i in 1:ncol(x)) {
  plot(ts(x[, i]), main = paste("Time Series", i), type = "l")
}

par(mfrow = c(1, 1))

rotated_df <- as.data.frame(t(as.matrix(x)))
ret <- inspect(rotated_df)
cp <- ret$changepoints[1] + 1
summary(ret)
plot(ret)

#inspect: 500

res <- e.divisive(x)

#ecp: 500

max_cusum_cp <- find_max_cusum_cp(x)

#aggregated mean: 500

#p/2 changes:

par(mfrow = c(10, 2)) 
par(mar = c(2, 2, 1, 1)) 

x <- gen_sparsets(1000, twenty_means, 1, 10, 1, 20, c(1, 500, 1000), 1.2)
for (i in 1:ncol(x)) {
  plot(ts(x[, i]), main = paste("Time Series", i), type = "l")
}

par(mfrow = c(1, 1))

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

#inspect 499

res <- e.divisive(x)

#ecp 499

max_cusum_cp <- find_max_cusum_cp(x)

#agg 497

#p/5 changes:

par(mfrow = c(10, 2))  
par(mar = c(2, 2, 1, 1))  

x <- gen_sparsets(1000, twenty_means, 1, 5, 1, 20, c(1, 500, 1000), 1.2)
for (i in 1:ncol(x)) {
  plot(ts(x[, i]), main = paste("Time Series", i), type = "l")
}

par(mfrow = c(1, 1))
rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

#inspect 499

res <- e.divisive(x)

#ecp 499

max_cusum_cp <- find_max_cusum_cp(x)

#agg 497

#p/10 changes:

par(mfrow = c(10, 2)) 
par(mar = c(2, 2, 1, 1))

x <- gen_sparsets(1000, twenty_means, 1, 2, 1, 20, c(1, 500, 1000), 1.2)
for (i in 1:ncol(x)) {
  plot(ts(x[, i]), main = paste("Time Series", i), type = "l")
}

par(mfrow = c(1, 1))

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

#495

res <- e.divisive(x)

#495


max_cusum_cp <- find_max_cusum_cp(x)

#478

#1 change:

par(mfrow = c(10, 2))  
par(mar = c(2, 2, 1, 1))  

x <- gen_sparsets(1000, twenty_means, 1, 1, 1, 20, c(1, 500, 1000), 1.2)
for (i in 1:ncol(x)) {
  plot(ts(x[, i]), main = paste("Time Series", i), type = "l")
}

par(mfrow = c(1, 1))

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

#495

res <- e.divisive(x)

#495

max_cusum_cp <- find_max_cusum_cp(x)


#478
###########################################################

#Changepoints in 10, but p increasing:

x <- gen_sparsets(1000, twenty_means, 1, 10, 1, 20, c(1, 500, 1000), 1.2)

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

res <- e.divisive(x)

max_cusum_cp <- find_max_cusum_cp(x)

set.seed(1) 
fifty_means <- runif(50, min = -5, max = 5)
fifty_means

x <- gen_sparsets(1000, fifty_means, 1, 10, 1, 50, c(1, 500, 1000), 1.2)

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

res <- e.divisive(x)

max_cusum_cp <- find_max_cusum_cp(x)

set.seed(1) 
hun_means <- runif(100, min = -5, max = 5)
hun_means

x <- gen_sparsets(1000, hun_means, 1, 10, 1, 100, c(1, 500, 1000), 1.2)

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

res <- e.divisive(x)

max_cusum_cp <- find_max_cusum_cp(x)



set.seed(1) 
hun_means <- runif(1000, min = -5, max = 5)
hun_means

x <- gen_sparsets(1000, hun_means, 1, 10, 1, 1000, c(1, 500, 1000), 1.2)

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

res <- e.divisive(x)

max_cusum_cp <- find_max_cusum_cp(x)


####################testing correlation


cor_matrix <- matrix(c(1.0, 0.9, 0.8, 0.7, 0.6,
                       0.9, 1.0, 0.9, 0.8, 0.7,
                       0.8, 0.9, 1.0, 0.9, 0.8,
                       0.7, 0.8, 0.9, 1.0, 0.9,
                       0.6, 0.7, 0.8, 0.9, 1.0), nrow = 5, byrow = TRUE)

std_dev_vector <- c(2, 2, 2, 2, 2)

cov_matrix <- cor_matrix * (std_dev_vector %*% t(std_dev_vector))



x <- gen_corrts(1000, c(-2, 4, 1, 3, 2), cov_matrix, 2, 10, 2)

result_df <- data.frame(x$data)
names(result_df) <- paste0("Variable_", 1:ncol(result_df))
result_df$time <- 1:n

df <- result_df

df$time <- 1:nrow(df)

results_melted <- reshape2::melt(df, id.vars = "time")

results_melted$variable <- as.factor(results_melted$variable)

ggplot(results_melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  theme_gdocs()

common <- x$common_cp
all <- x$change_points
x <- x$data

rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df)
ret
summary(ret)
plot(ret)

calculate_true_positive_ratio(all, ret$changepoints[, 1], 1)

res <- e.divisive(x)


calculate_true_positive_ratio(all, res$estimates, 1)

max_cusum_cp <- find_max_cusum_cp(x)

###########################################


num_distributions <- 10
n <- 1000  
changepoint <- 500 

time_series <- matrix(0, nrow = n, ncol = num_distributions)

for(i in 1:num_distributions){
  time_series[, i] <- generate_mixed_distribution(n, changepoint, i)
}

par(mfrow = c(5, 2))
plot(ts(time_series[, 1]), col = 1)
for (i in 2:num_distributions) {
  plot(time_series[, i], type = 'l', col = i)
}

time_series_df <- data.frame(time = rep(1:n, num_distributions),
                             value = as.vector(time_series),
                             distribution = factor(rep(1:num_distributions, each = n)))

ggplot(time_series_df, aes(x = time, y = value, color = distribution)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Time", y = "Value", color = "Variable") +
  facet_wrap(~distribution, scales = "free_y", nrow = 5) +
  theme_gdocs()

plot <- ggplot(time_series_df, aes(x = time, y = value, color = distribution)) +
  geom_line() +
  scale_color_viridis_d() + 
  labs(x = "Time", y = "Value", color = "Variable") +
  facet_wrap(~distribution, scales = "free_y", nrow = 5) +
  theme_gdocs()

x <- time_series

par(mfrow = c(1, 1))
rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df, threshold = 3.7)
ret
summary(ret)
plot(ret)

res <- e.divisive(x)

max_cusum_cp <- find_max_cusum_cp(x)

########################################################

#testing inspect for different values of hyperparameters
?inspect
set.seed(1)
twenty_means <- runif(10, min = -5, max = 5) 
n <- 1000
p <- 10
lambda <- log(log(n)*p/2)

x <- gen_sparsets(n, twenty_means, 1, 3, 1, p, c(1, 500, 1000), 1.2)



plot(ts(x))
rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df, lambda = 0.00001)
ret
summary(ret)
plot(ret)

res <- e.divisive(x)

max_cusum_cp <- find_max_cusum_cp(x)



x <- gen_sparsets(n, twenty_means, 1, 2, 1, p, c(1, 500, 1000), 1.2)



plot(ts(x))
rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df, lambda = 200)
ret
summary(ret)
plot(ret)

res <- e.divisive(x)

max_cusum_cp <- find_max_cusum_cp(x)

