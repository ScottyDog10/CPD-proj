#1D Application: LIBOR
library(wbs)

#historical-libor-rates-chart.csv
libor_1m$X1M_LIBOR

#use libor_1m fro Interest Rate.R

med <- median(libor_1m$X1M_LIBOR)
mad <- mad(libor_1m$X1M_LIBOR)

?mad
libor_1m$X1M_LIBOR_standardized <- (libor_1m$X1M_LIBOR - med) / mad

plot(ts(libor_1m$X1M_LIBOR_standardized))

?median

libor_1m$date[which.max(libor_1m$X1M_LIBOR)]


data <- libor_1m$X1M_LIBOR








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
        duplicate_idx <- which(df$tau == tau_hat)
        if (length(duplicate_idx) > 0) {
          max_cusum_idx <- which.max(df$cusum[duplicate_idx])
          if (df$cusum[duplicate_idx[max_cusum_idx]] < max_cusum(x[k:q], k)[[1]]) {
            df <- df[-duplicate_idx[max_cusum_idx], ]  # Remove the row with smaller CUSUM
            df <- rbind(df, data.frame(tau = tau_hat, cusum = max_cusum(x[k:q], k)[[1]]))  # Add the new tau and its CUSUM
          }
        } else {
          df <- rbind(df, data.frame(tau = tau_hat, cusum = max_cusum(x[k:q], k)[[1]]))
        }
        df <- df[order(df$tau, decreasing = FALSE), ]
        df <- df[!duplicated(df$tau), ]
      }
      bool <-!(tau_hat %in% c(k, q))
    }
    intervals <- create_intervals(df$tau) 
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



#BS:

segment_counts <- 1:30
BIC_values <- sapply(segment_counts, function(N) {
  cat("Iteration:", N, "\n")
  BIC_result <- BIC(libor_1m$X1M_LIBOR, 1, length(libor_1m$X1M_LIBOR), N)
  print(BIC_result)
  return(BIC_result)
})

Y <- BIC_values[1, ]
X <- BIC_values[2, ]

min_bs <- min(unlist(Y))
min_index <- which.min(Y)
min_arg <- X[min_index]

detected_change_points1 <- bs_N(libor_1m$X1M_LIBOR, 1, length(libor_1m$X1M_LIBOR), 15)


#WBS:

w <- wbs(libor_1m$X1M_LIBOR, M = 5000)
w.cpt <- changepoints(w, th = -6021851) 
detected_change_points2 <- sort(c(w.cpt$cpt.th[[1]][1:15], c(1, length(libor_1m$X1M_LIBOR))))

BIC_wbs <- function(x, N){
  w <- wbs(x)
  w.cpt <- changepoints(w, th = -6021851)
  changepoints <- sort(c(w.cpt$cpt.th[[1]][1:N], c(1, length(x))))
  result <- -2*calculate_log_likelihood(x, changepoints, 1) + length(changepoints)*log(length(x))
  return(list(result, length(changepoints) -2))
}

BIC_values <- sapply(segment_counts, function(N) {
  cat("Iteration:", N, "\n")
  BIC_result <- BIC_wbs(libor_1m$X1M_LIBOR, N)
  print(BIC_result)
  return(BIC_result)
})

Y <- BIC_values[1, ]
X <- BIC_values[2, ]

min_wbs <- min(unlist(Y))
min_index <- which.min(Y)
min_arg <- X[min_index]

w <- wbs(libor_1m$X1M_LIBOR, M = 5000)
w.cpt <- changepoints(w, th = -6021851)
detected_change_points2 <- sort(c(w.cpt$cpt.th[[1]][1:16], c(1, length(libor_1m$X1M_LIBOR))))

#SBS:

BIC_sbs <- function(x, N){
  w <- seedBS(x)
  w.cpt <- changepoints(w, th = -6021851)
  changepoints <- sort(c(w.cpt$cpt.th[[1]][1:N], c(1, length(x))))
  result <- -2*calculate_log_likelihood(x, changepoints, 1) + length(changepoints)*log(length(x))
  return(list(result, length(changepoints) -2))
}


BIC_values <- sapply(segment_counts, function(N) {
  cat("Iteration:", N, "\n")
  BIC_result <- BIC_sbs(libor_1m$X1M_LIBOR, N)
  print(BIC_result)
  return(BIC_result)
})

Y <- BIC_values[1, ]
X <- BIC_values[2, ]

min_sbs <- min(unlist(Y))
min_index <- which.min(Y)
min_arg <- X[min_index]

w <- seedBS(libor_1m$X1M_LIBOR)
w.cpt <- changepoints(w, th = -6021851)  
detected_change_points3 <- sort(c(w.cpt$cpt.th[[1]][1:20], c(1, length(libor_1m$X1M_LIBOR))))


min_bs
min_wbs
min_sbs

plot(ts(libor_1m$X1M_LIBOR), col = 'darkgreen', ylab = 'X')

axis_pos <- par("usr")[3] 

y1 <- calculate_interval_means(detected_change_points1$tau, libor_1m$X1M_LIBOR)
y2 <- calculate_interval_means(detected_change_points2, libor_1m$X1M_LIBOR)
y3 <- calculate_interval_means(detected_change_points3, libor_1m$X1M_LIBOR)

lines(y1, col = 'red')
lines(y2, col = 'blue')
lines(y3, col = 'orange')

offset <- 0.3

points(x = detected_change_points1$tau, y = rep(axis_pos + offset, length(detected_change_points1$tau)), pch = 4, col = 'red')

offset <- 0.4

points(x = detected_change_points2, y = rep(axis_pos + offset, length(detected_change_points2)), pch = 4, col = 'blue')

offset <- 0.5

points(x = detected_change_points3, y = rep(axis_pos + offset, length(detected_change_points3)), pch = 4, col = 'orange')

legend("topright", legend = c("BS Change Points", "WBS Change Points", "SBS Change Points"), pch = c(4, 4, 4), col = c("red","blue", "orange"))
                               

library(ggplot2)

change_points_data <- data.frame('date' = libor_1m$date, 'X1M_LIBOR' = libor_1m$X1M_LIBOR, 'BS' = y1, 'WBS' = y2, 
                                 'SBS' = y3)

plot <- ggplot(change_points_data, aes(x = date)) +
  geom_line(aes(y = X1M_LIBOR, color = "X1M_LIBOR")) +
  geom_line(aes(y = BS, color = "BS")) +
  geom_line(aes(y = WBS, color = "WBS")) +
  geom_line(aes(y = SBS, color = "SBS")) +
  labs(x = "Date", y = "Interest Rate (%)", title = "One-Month Interbank Borrowing Rates") +
  scale_colour_manual("", 
                      breaks = c("X1M_LIBOR", "BS", "WBS", "SBS"),
                      values = c("darkgreen", "red", "blue", "orange")) +
  theme_gdocs() +
  theme(legend.position = c(0.8, 0.85),
        legend.text = element_text(size = 10.5), 
        legend.title = element_text(size = 10.5), 
        legend.key.size = unit(0.8, "lines")) 


length(detected_change_points3)
segment_list <- list()

for (i in 2:length(detected_change_points3)) {
    segment_list[[i]] <- data[(detected_change_points3[i - 1] + 1):detected_change_points3[i]]
}

segment_list <- segment_list[-1]
total_observations <- sum(lengths(segment_list))
segment_list[[1]] <- c(data[1], segment_list[[1]])
total_observations <- sum(lengths(segment_list))
combined_data <- unlist(segment_list)
identical(combined_data, data)
options(repr.plot.width=10, repr.plot.height=30)
par(mfrow=c(7, 3), mar=c(4, 4, 2, 1)) 

for (i in 1:length(segment_list)) {
  qqnorm(segment_list[[i]], main = paste("QQ Plot for Segment", i), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(segment_list[[i]], col = 2)
}
#dev.off()

plot(segment_list[[6]], type = 'l')

par(mfrow=c(1, 1))

qqnorm(data, main = "QQ Plot for X1M_LIBOR", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(data, col = 2)

#dev.off()
par(mfrow=c(7, 3))
for (i in 1:length(segment_list)) {
  pacf(segment_list[[i]], main = '')
}

for (i in 1:length(segment_list)) {
  acf(segment_list[[i]], main = '', xlab = paste("Lag for segment", i), ylab = "Autocorrelation")
}

#dev.off()

acf(data, main = "Autocorrelation Plot for X1M_LIBOR", xlab = "Lag", ylab = "Autocorrelation")
