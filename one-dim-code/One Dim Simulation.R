#Simulation Application in One Dimension:

#Use three different simulations, all 1000 long, one with 4 changepoints, one with 16, and one with 32.
#Use sd of 1.

gen_timeseries <- function(N, n, m, s){
  set.seed(1)
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


# Example usage:
result <- gen_timeseries(N = 30, n = 1000, m = 3, s = 2)
print(result$change_points)
print(result$mus)
plot(result$data, type = "l", col = "darkgreen", ylab = "Value", xlab = "Time", main = "Time Series with Mean Changes")

four_changes <- gen_timeseries(N = 4, n = 1000, m = 3, s = 2)
plot(four_changes$data, type = "l", col = "darkgreen", ylab = "Value", xlab = "Time", main = "Time Series with Mean Changes")


sixteen_changes <- gen_timeseries(N = 16, n = 1000, m = 3, s = 2)
plot(sixteen_changes$data, type = "l", col = "darkgreen", ylab = "Value", xlab = "Time", main = "Time Series with Mean Changes")


thirtytwo_changes <- gen_timeseries(N = 32, n = 1000, m = 3, s = 2)
plot(thirtytwo_changes$data, type = "l", col = "darkgreen", ylab = "Value", xlab = "Time", main = "Time Series with Mean Changes")


#required functions: 

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

create_intervals <- function(vec){
  
  df <- data.frame(rep(NA, length(vec)-1), rep(NA, length(vec)-1))
  colnames(df) <- list('Start', 'End')
  for(i in 1:length(vec)-1){
    df[i, ] <- c(vec[i], vec[i+1])
  }
  return(df)
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

bs_N(four_changes$data, 1, 1000, 4)

four_changes$change_points
four_changes$mus

four_changes <- gen_timeseries(N = 4, n = 1000, m = 3, s = 2)
plot(four_changes$data, type = "l", col = "darkgreen", ylab = "Value", xlab = "Time", main = "Time Series with Mean Changes")


bs_N(sixteen_changes$data, 1, 1000, 16)
sixteen_changes$change_points

sixteen_changes <- gen_timeseries(N = 16, n = 1000, m = 3, s = 2)
plot(sixteen_changes$data, type = "l", col = "darkgreen", ylab = "Value", xlab = "Time", main = "Time Series with Mean Changes")

bs_N(thirtytwo_changes$data, 1, 1000, 32)
thirtytwo_changes$change_points
thirtytwo_changes <- gen_timeseries(N = 32, n = 1000, m = 3, s = 2)
plot(thirtytwo_changes$data, type = "l", col = "darkgreen", ylab = "Value", xlab = "Time", main = "Time Series with Mean Changes")



install.packages("fossil")
library(fossil)
true_change_points <- c(1, 130, 188, 271, 278, 300, 308, 472, 495, 510, 598, 680, 837, 875, 931, 951, 979, 1000)
detected_change_points <- bs_N(sixteen_changes$data, 1, 1000, 16)$tau

compare_change_points <- function(true_change_points, detected_change_points, epsilon) {
  true_positives <- 0
  true_change_points <- true_change_points[c(-1, -length(true_change_points))]
  detected_change_points <- detected_change_points[c(-1, -length(detected_change_points))]
  for (true_point in true_change_points) {
    for (detected_point in detected_change_points) {
      if (abs(true_point - detected_point) <= epsilon) {
        true_positives <- true_positives + 1
        break 
      }
    }
  }
  
  return(true_positives)
}

true_positives <- 0
true_change_points <- true_change_points[c(-1, -length(true_change_points))]
detected_change_points <- detected_change_points[c(-1, -length(detected_change_points))]
for (true_point in true_change_points) {
  for (detected_point in detected_change_points) {
    
  }
}


true_change_points <- c(1, 130, 188, 271, 278, 300, 308, 472, 495, 510, 598, 680, 837, 875, 931, 951, 979, 1000)
detected_change_points <- bs_N(sixteen_changes$data, 1, 1000, 16)$tau





find_closest_element <- function(value, vector) {
  closest_element <- vector[which.min(abs(vector - value))]
  abs_distance <- abs(value - closest_element)
  
  result <- list(closest_element = closest_element, abs_distance = abs_distance)
  return(result)
}
value <- 5
vector <- c(2, 4, 6, 8, 10)
result <- find_closest_element(value, vector)
print(result)


true_change_points <- c(1, 130, 188, 271, 278, 300, 308, 472, 495, 510, 598, 680, 837, 875, 931, 951, 979, 1000)
detected_change_points <- bs_N(sixteen_changes$data, 1, 1000, 16)$tau


true_change_points <- true_change_points[c(-1, -length(true_change_points))]
detected_change_points <- detected_change_points [c(-1, -length(detected_change_points ))]

true_positives <- 0
for (i in 1:length(true_change_points)) {
  abs_distance <- find_closest_element(true_change_points[i], detected_change_points)$abs_distance
  
  if (abs_distance <= 10) {
    closest_element <- find_closest_element(true_change_points[i], detected_change_points)$closest_element
    print(closest_element)
    detected_change_points <- detected_change_points[detected_change_points != closest_element]  # Remove closest element
    true_positives <- true_positives + 1
  }
}
print(detected_change_points)

true_positives/length(true_change_points)

calculate_true_positive_ratio <- function(true_change_points, detected_change_points, epsilon) {
  true_change_points <- true_change_points[-c(1, length(true_change_points))]
  detected_change_points <- detected_change_points[-c(1, length(detected_change_points))]
  
  true_positives <- 0
  for (i in 1:length(true_change_points)) {
    abs_distance <- find_closest_element(true_change_points[i], detected_change_points)$abs_distance
    
    if (abs_distance <= epsilon && length(detected_change_points) != 0) {
      closest_element <- find_closest_element(true_change_points[i], detected_change_points)$closest_element
      detected_change_points <- detected_change_points[detected_change_points != closest_element]  # Remove closest element
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

calculate_interval_means <- function(change_points, x) {
  interval_means <- sapply(1:(length(change_points) - 1), function(i) {
    start_idx <- change_points[i]
    end_idx <- change_points[i + 1]
    mean(x[start_idx:end_idx])
  })
  y <- rep(NA, length(x))
  for (i in 1:(length(change_points) - 1)) {
    start_idx <- change_points[i]
    end_idx <- change_points[i + 1]
    y[start_idx:end_idx] <- interval_means[i]
  }
  
  return(y)
}

#Binary Segmentation:

true_change_points <- four_changes$change_points
detected_change_points <- bs_N(four_changes$data, 1, 1000, 4)$tau
epsilon <- epsilon_fun(4, 1000) 

ratio1 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio)

true_change_points <- c(1, 130, 188, 271, 278, 300, 308, 472, 495, 510, 598, 680, 837, 875, 931, 951, 979, 1000)
detected_change_points <- bs_N(sixteen_changes$data, 1, 1000, 16)$tau
epsilon <- epsilon_fun(16, 1000) 

ratio2 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio)

true_change_points <- thirtytwo_changes$change_points
detected_change_points <- bs_N(thirtytwo_changes$data, 1, 1000, 32)$tau
epsilon <- epsilon_fun(32, 1000) 

ratio3 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio)

#we need epsilon to approach zero as N approaches n from the left. Choice of epsilon will also depend
#on the importance of detecting a change point, if for example we are looking at the FTSE, a large epsilon
#would be important as we would like to detect anything likely to be a change, whereas for one stock
#we would want a small epsilon as we don't really mind if we don't estimate a change point.



#test with different values of N and n
N_values <- c(10, 50, 100)
n_values <- c(1000, 5000, 10000) 

for (N in N_values) {
  for (n in n_values) {
    epsilon <- epsilon_fun(N, n)
    cat("For N =", N, "and n =", n, ", epsilon is:", epsilon, "\n")
  }
}

#WBS:

library(wbs)


#an example in which standard Binary Segmentation fails to detect change points

set.seed(1)
x <- rep(NA, 1000)
x[1:250] <- rnorm(250, -4, 1)
x[251:300] <- rnorm(50, 4, 1)
x[301:700] <- rnorm(400, -2, 1)
x[701:1000] <- rnorm(300, 1, 1)

s <- sbs(x)
w <- wbs(x)
s.cpt <- changepoints(s)
s.cpt
w.cpt <- changepoints(w)
w.cpt

plot(ts(x))

?changepoints

w.cpt$cpt.ic[[2]]






#4

true_change_points <- four_changes$change_points
w <- wbs(four_changes$data)
w.cpt <- sort(c(changepoints(w, Kmax = 4)$cpt.ic[[2]], c(1, 1000)))

detected_change_points <- w.cpt
epsilon <- epsilon_fun(4, 1000) 

ratio4 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio)

#16

true_change_points <- sixteen_changes$change_points
w <- wbs(sixteen_changes$data, M = 5000)
w.cpt <- changepoints(w, th = -6021851) 
detected_change_points <- sort(c(w.cpt$cpt.th[[1]][1:16], c(1, 1000)))

ratio5 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio)



true_change_points <- thirtytwo_changes$change_points
w <- wbs(thirtytwo_changes$data, M = 5000)
w.cpt <- changepoints(w, th = -6021851)
detected_change_points <- sort(c(w.cpt$cpt.th[[1]][1:32], c(1, 1000)))


ratio6 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio)

#seeded:

plot(ts(four_changes$data))

true_change_points <- four_changes$change_points
w <- seedBS(four_changes$data, decay = 1.5)
#larger decay
w.cpt <- sort(c(changepoints(w, Kmax = 4)$cpt.ic[[2]], c(1, 1000)))

detected_change_points <- w.cpt
epsilon <- epsilon_fun(4, 1000)

ratio7 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio7)


#16

true_change_points <- sixteen_changes$change_points
w <- seedBS(sixteen_changes$data)
w.cpt <- changepoints(w, th = -6021851) 
detected_change_points <- sort(c(w.cpt$cpt.th[[1]][1:16], c(1, 1000)))
epsilon <- epsilon_fun(16, 1000) 

ratio8 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio)

#32

true_change_points <- thirtytwo_changes$change_points
w <- seedBS(thirtytwo_changes$data, decay = sqrt(2))
w.cpt <- changepoints(w, th = -6021851) 
detected_change_points <- sort(c(w.cpt$cpt.th[[1]][1:32], c(1, 1000)))


ratio9 <- calculate_true_positive_ratio(true_change_points, detected_change_points, epsilon)
print(ratio9)


#table:

results <- matrix(c(ratio1, ratio2, ratio3,
                    ratio4, ratio5, ratio6,
                    ratio7, ratio8, ratio9), nrow = 3)
  
results <-  data.frame(results)
colnames(results) <- c('BS', 'WBS', 'SBS')
rownames(results) <- c('N = 4', 'N = 16', 'N = 32')

results
######################################################################################################

#4:

#BS

true_change_points <- four_changes$change_points
detected_change_points1 <- bs_N(four_changes$data, 1, 1000, 4)$tau
epsilon <- epsilon_fun(4, 1000)

ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points1, epsilon)
print(ratio)

#WBS

true_change_points <- four_changes$change_points
w <- wbs(four_changes$data)
w.cpt <- sort(c(changepoints(w, Kmax = 4)$cpt.ic[[2]], c(1, 1000)))

detected_change_points2 <- w.cpt
epsilon <- epsilon_fun(4, 1000)

ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points2, epsilon)
print(ratio)

#SBS

plot(ts(four_changes$data))

true_change_points <- four_changes$change_points
w <- seedBS(four_changes$data, decay= 1.5)
w.cpt <- sort(c(changepoints(w, Kmax = 4)$cpt.ic[[2]], c(1, 1000)))

detected_change_points3 <- w.cpt
epsilon <- epsilon_fun(4, 1000) 

ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points3, epsilon)
print(ratio)

plot(ts(four_changes$data), col = 'darkgreen', ylab = 'X')
 
axis_pos <- par("usr")[3] 

y1 <- calculate_interval_means(detected_change_points1, four_changes$data)
y2 <- calculate_interval_means(detected_change_points2, four_changes$data)
y3 <- calculate_interval_means(detected_change_points3, four_changes$data)

lines(y1, col = 'red')
lines(y2, col = 'blue')
lines(y3, col = 'orange')

offset <- 0.2
points(x = true_change_points, y = rep(axis_pos + offset, length(true_change_points)), pch = 18, col = 'darkgreen')

offset <- 0.3

points(x = detected_change_points1, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'red')

offset <- 0.4

points(x = detected_change_points2, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'blue')

offset <- 0.5

points(x = detected_change_points3, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'orange')
legend("topright", legend = c("True Change Points", "BS Change Points", "WBS Change Points", "SBS Change Points"), pch = c(18, 4, 4, 4), col = c("darkgreen", "red",
                                                                                                                                                 "blue", "orange"))

#16:

#BS

true_change_points <- sixteen_changes$change_points
detected_change_points1 <- bs_N(sixteen_changes$data, 1, 1000, 16)$tau
epsilon <- epsilon_fun(16, 1000)

ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points1, epsilon)
print(ratio)

#WBS

true_change_points <- sixteen_changes$change_points
w <- wbs(sixteen_changes$data, M = 5000)
w.cpt <- changepoints(w, th = -6021851)  
detected_change_points2 <- sort(c(w.cpt$cpt.th[[1]][1:16], c(1, 1000)))


ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points2, epsilon)
print(ratio)


#SBS

true_change_points <- sixteen_changes$change_points
w <- seedBS(sixteen_changes$data)
w.cpt <- changepoints(w, th = -6021851)  
detected_change_points3 <- sort(c(w.cpt$cpt.th[[1]][1:16], c(1, 1000)))
epsilon <- epsilon_fun(16, 1000)

ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points3, epsilon)
print(ratio)

plot(ts(sixteen_changes$data), col = 'darkgreen', ylab = 'X')

axis_pos <- par("usr")[3] 

y1 <- calculate_interval_means(detected_change_points1, sixteen_changes$data)
y2 <- calculate_interval_means(detected_change_points2, sixteen_changes$data)
y3 <- calculate_interval_means(detected_change_points3, sixteen_changes$data)

lines(y1, col = 'red')
lines(y2, col = 'blue')
lines(y3, col = 'orange')

offset <- 0.2

points(x = true_change_points, y = rep(axis_pos + offset, length(true_change_points)), pch = 18, col = 'darkgreen')

offset <- 0.3

points(x = detected_change_points1, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'red')

offset <- 0.4

points(x = detected_change_points2, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'blue')

offset <- 0.5

points(x = detected_change_points3, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'orange')


legend("topright", legend = c("True Change Points", "BS Change Points", "WBS Change Points", "SBS Change Points"), pch = c(18, 4, 4, 4), col = c("darkgreen", "red",
                                                                                                                                                 "blue", "orange"))

#18:

#BS

true_change_points <- thirtytwo_changes$change_points
detected_change_points1 <- bs_N(thirtytwo_changes$data, 1, 1000, 32)$tau
epsilon <- epsilon_fun(32, 1000)

ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points1, epsilon)
print(ratio)

#WBS

true_change_points <- thirtytwo_changes$change_points
w <- wbs(thirtytwo_changes$data, M = 5000)
w.cpt <- changepoints(w, th = -6021851) 
detected_change_points2 <- sort(c(w.cpt$cpt.th[[1]][1:32], c(1, 1000)))


ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points2, epsilon)
print(ratio)


#SBS

true_change_points <- thirtytwo_changes$change_points
w <- seedBS(thirtytwo_changes$data)
w.cpt <- changepoints(w, th = -6021851)
detected_change_points3 <- sort(c(w.cpt$cpt.th[[1]][1:32], c(1, 1000)))
epsilon <- epsilon_fun(16, 1000)

ratio <- calculate_true_positive_ratio(true_change_points, detected_change_points3, epsilon)
print(ratio)

plot(ts(thirtytwo_changes$data), col = 'darkgreen', ylab = 'X')

axis_pos <- par("usr")[3] 

y1 <- calculate_interval_means(detected_change_points1, thirtytwo_changes$data)
y2 <- calculate_interval_means(detected_change_points2, thirtytwo_changes$data)
y3 <- calculate_interval_means(detected_change_points3, thirtytwo_changes$data)

lines(y1, col = 'red')
lines(y2, col = 'blue')
lines(y3, col = 'orange')

offset <- 0.2
points(x = true_change_points, y = rep(axis_pos + offset, length(true_change_points)), pch = 18, col = 'darkgreen')

offset <- 0.3

points(x = detected_change_points1, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'red')

offset <- 0.4

points(x = detected_change_points2, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'blue')

offset <- 0.5

points(x = detected_change_points3, y = rep(axis_pos + offset, length(true_change_points)), pch = 4, col = 'orange')


legend("topright", legend = c("True Change Points", "BS Change Points", "WBS Change Points", "SBS Change Points"), pch = c(18, 4, 4, 4), col = c("darkgreen", "red",
                                                                                                                                                 "blue", "orange"))

#############################################################

