#Short Term IR Application:

library(ggplot2)
library(zoo)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggthemes)

#set transformed_data_long_filled <- ShortTermRates.csv

cluster_1 <- transformed_data_long_filled %>% filter(Cluster == 1)
cluster_2 <- transformed_data_long_filled %>% filter(Cluster == 2)
cluster_3 <- transformed_data_long_filled %>% filter(Cluster == 3)
cluster_4 <- transformed_data_long_filled %>% filter(Cluster == 4)

cluster_1_pivot <- pivot_wider(cluster_1, names_from = Country, values_from = Interest_Rate)

cluster_2_pivot <- pivot_wider(cluster_2, names_from = Country, values_from = Interest_Rate)

cluster_3_pivot <- pivot_wider(cluster_3, names_from = Country, values_from = Interest_Rate)

cluster_4_pivot <- pivot_wider(cluster_4, names_from = Country, values_from = Interest_Rate)



library(reshape2)

library(ggplot2)

transformed_data_long_filled$TIME <- as.Date(transformed_data_long_filled$TIME)

plot_cluster <- function(data, cluster_id) {
  ggplot(data = data, aes(x = TIME, y = Interest_Rate, color = Country)) +
    geom_line() +
    labs(x = "Time", y = "Interest Rate (%)", title = paste("Cluster", cluster_id)) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") + 
    scale_color_viridis_d() +
    theme_gdocs() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.key.size = unit(0.5, "lines")) 
}

plot_cluster_1 <- plot_cluster(transformed_data_long_filled[transformed_data_long_filled$Cluster == 1, ], 1)
plot_cluster_2 <- plot_cluster(transformed_data_long_filled[transformed_data_long_filled$Cluster == 2, ], 2)
plot_cluster_3 <- plot_cluster(transformed_data_long_filled[transformed_data_long_filled$Cluster == 3, ], 3)
plot_cluster_4 <- plot_cluster(transformed_data_long_filled[transformed_data_long_filled$Cluster == 4, ], 4)

print(plot_cluster_1)
print(plot_cluster_2)
print(plot_cluster_3)
print(plot_cluster_4)


#cluster 1:

cluster_1_pivot <- pivot_wider(cluster_1, names_from = Country, values_from = Interest_Rate)

x <- cluster_1_pivot[, c(-1, -2)]

par(mfrow = c(1, 1))
rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df, threshold = 270)
length(ret$changepoints[,1])
inspect <- ret$changepoints[,1]

summary(ret)

res <- e.divisive(x, sig.lvl = 0.05)

ediv <- res$estimates[c(-1, -length(res$estimates))]

library(viridis)

ediv_time <- unique(cluster_1_data$TIME)[ediv]
inspect_time <- unique(cluster_1_data$TIME)[inspect]

?e.agglo

x <- as.matrix(x)

?inspect
edivpen <- e.agglo(x, member=1:nrow(x), alpha=1, penalty=function(cp,Xts) 0)

edivpentimes <- (unique(cluster_1_data$TIME)[edivpen$estimates])[c(-1, -length((unique(cluster_1_data$TIME)[edivpen$estimates])))]

cluster_1_data <- subset(transformed_data_long_filled, Cluster == 1)
plot(cluster_1_data$TIME, cluster_1_data$Interest_Rate, type = "l",
     xlab = "Time", ylab = "Interest Rate (%)",
     main = "Cluster 1: Interest Rate")

par(mfrow = c(1, 1))
countries <- unique(cluster_1_data$Country)
min_time <- min(cluster_1_data$TIME)
max_time <- max(cluster_1_data$TIME)
min_interest_rate <- min(cluster_1_data$Interest_Rate)
max_interest_rate <- max(cluster_1_data$Interest_Rate)

plot(Interest_Rate ~ TIME, data = cluster_1_data, type = "n",
     xlab = "Time", ylab = "Interest Rate (%)",
     main = "Cluster 1: Short-Term Rates")

viridis_palette <- viridis(length(countries))

for (i in 1:length(countries)) {
  country <- countries[i]
  country_data <- subset(cluster_1_data, Country == country)
  lines(country_data$TIME, country_data$Interest_Rate, col = viridis_palette[i])
}

abline(v = ediv_time, col = rgb(1, 0, 0, alpha = 0.3), lwd = 2, lty = 2)
abline(v = inspect_time, col = rgb(0, 0, 1, alpha = 0.3), lwd = 2, lty = 2)
abline(v = edivpentimes, col = rgb(0, 1, 0, alpha = 0.3), lwd = 2, lty = 2)
legend("topright", legend = countries, col = viridis_palette, lty = 1, cex = 0.5)

legend("topleft", legend = c("E-Divisive", "Inspect", "GOF E-Divisive"), 
       col = c(rgb(1, 0, 0, alpha = 0.3), rgb(0, 0, 1, alpha = 0.3), rgb(0, 1, 0, alpha = 0.3)),
       lty = 2, cex = 0.8)


plot(ret)

ret$changepoints[, 1]

par(mfrow = c(1, 1))


#return the changepoint times

length(cluster_1_data$TIME)

############################################

#cluster 3


cluster_1_pivot <- pivot_wider(cluster_3, names_from = Country, values_from = Interest_Rate)

x <- cluster_1_pivot[, c(-1, -2)]

par(mfrow = c(1, 1))
rotated_df <- as.data.frame(t(as.matrix(x)))

ret <- inspect(rotated_df, threshold = 250)
length(ret$changepoints[,1])
inspect <- ret$changepoints[,1]
summary(ret)

nrow(x)
res <- e.divisive(x, sig.lvl = 0.05)

ediv <- res$estimates[c(-1, -length(res$estimates))]

library(viridis)

ediv_time <- unique(cluster_1_data$TIME)[ediv]

length(unique(cluster_1_data$TIME))

inspect_time <- unique(cluster_1_data$TIME)[inspect]

?e.agglo

x <- as.matrix(x)

edivpen <- e.agglo(x, member=1:nrow(x), alpha=1, penalty=function(cp,Xts) 0)

edivpentimes <- (unique(cluster_1_data$TIME)[edivpen$estimates])[c(-1, -length((unique(cluster_1_data$TIME)[edivpen$estimates])))]

cluster_1_data <- subset(transformed_data_long_filled, Cluster == 3)
plot(cluster_1_data$TIME, cluster_1_data$Interest_Rate, type = "l",
     xlab = "Time", ylab = "Interest Rate (%)",
     main = "Cluster 1: Interest Rate")
par(mfrow = c(1, 1))

countries <- unique(cluster_1_data$Country)

min_time <- min(cluster_1_data$TIME)
max_time <- max(cluster_1_data$TIME)
min_interest_rate <- min(cluster_1_data$Interest_Rate)
max_interest_rate <- max(cluster_1_data$Interest_Rate)
plot(Interest_Rate ~ TIME, data = cluster_1_data, type = "n",
     xlab = "Time", ylab = "Interest Rate (%)",
     main = "Cluster 3: Short-Term Rates")

viridis_palette <- viridis(length(countries))

for (i in 1:length(countries)) {
  country <- countries[i]
  country_data <- subset(cluster_1_data, Country == country)
  lines(country_data$TIME, country_data$Interest_Rate, col = viridis_palette[i])
}

abline(v = ediv_time, col = rgb(1, 0, 0, alpha = 0.3), lwd = 2, lty = 2)
abline(v = inspect_time, col = rgb(0, 0, 1, alpha = 0.3), lwd = 2, lty = 2)
abline(v = edivpentimes, col = rgb(0, 1, 0, alpha = 0.3), lwd = 2, lty = 2)
legend("topright", legend = countries, col = viridis_palette, lty = 1, cex = 0.5)
legend("topleft", legend = c("E-Divisive", "Inspect", "GOF E-Divisive"), col = c(rgb(1, 0, 0, alpha = 0.3), rgb(0, 0, 1, alpha = 0.3),
                                                                                 rgb(0, 1, 0, alpha = 0.3)), lty = 2, lwd = 2, cex = 0.5)

plot(ret)

ret$changepoints[, 1]

par(mfrow = c(1, 1))

length(cluster_1_data$TIME)

####################################

#cluster 4:

cluster_1_pivot <- pivot_wider(cluster_4, names_from = Country, values_from = Interest_Rate)

x <- cluster_1_pivot[, c(-1, -2)]

par(mfrow = c(1, 1))
rotated_df <- as.data.frame(t(as.matrix(x)))
ret <- inspect(rotated_df, threshold = 450)
length(ret$changepoints[,1])
inspect <- ret$changepoints[,1]
summary(ret)

nrow(x)
res <- e.divisive(x, sig.lvl = 0.05)

ediv <- res$estimates[c(-1, -length(res$estimates))]

library(viridis)
ediv_time <- unique(cluster_1_data$TIME)[ediv]

length(unique(cluster_1_data$TIME))

inspect_time <- unique(cluster_1_data$TIME)[inspect]

?e.agglo

x <- as.matrix(x)

edivpen <- e.agglo(x, member=1:nrow(x), alpha=1, penalty=function(cp,Xts) 0)

edivpentimes <- (unique(cluster_1_data$TIME)[edivpen$estimates])
cluster_1_data <- subset(transformed_data_long_filled, Cluster == 4)
plot(cluster_1_data$TIME, cluster_1_data$Interest_Rate, type = "l",
     xlab = "Time", ylab = "Interest Rate (%)",
     main = "Cluster 1: Interest Rate")

par(mfrow = c(1, 1))
countries <- unique(cluster_1_data$Country)
min_time <- min(cluster_1_data$TIME)
max_time <- max(cluster_1_data$TIME)
min_interest_rate <- min(cluster_1_data$Interest_Rate)
max_interest_rate <- max(cluster_1_data$Interest_Rate)
plot(Interest_Rate ~ TIME, data = cluster_1_data, type = "n",
     xlab = "Time", ylab = "Interest Rate (%)",
     main = "Cluster 4: Short-Term Rates")

viridis_palette <- viridis(length(countries))
for (i in 1:length(countries)) {
  country <- countries[i]
  country_data <- subset(cluster_1_data, Country == country)
  lines(country_data$TIME, country_data$Interest_Rate, col = viridis_palette[i])
}
abline(v = ediv_time, col = rgb(1, 0, 0, alpha = 0.3), lwd = 2, lty = 2)
abline(v = inspect_time, col = rgb(0, 0, 1, alpha = 0.3), lwd = 2, lty = 2)
abline(v = edivpentimes, col = rgb(0, 1, 0, alpha = 0.3), lwd = 2, lty = 2)

legend("topright", legend = countries, col = viridis_palette, lty = 1, cex = 0.5)
legend("topleft", legend = c("E-Divisive", "Inspect", "GOF E-Divisive"), col = c(rgb(1, 0, 0, alpha = 0.3), rgb(0, 0, 1, alpha = 0.3),
                                                                                 rgb(0, 1, 0, alpha = 0.3)), lty = 2, lwd = 2, cex = 0.5)
plot(ret)

ret$changepoints[, 1]
par(mfrow = c(1, 1))

length(cluster_1_data$TIME)

#we will conduct assumption verification on Switzerland for Cluster 4

swiss <- cluster_1_data[cluster_1_data$Country=='Switzerland', ]

plot(swiss$TIME, swiss$Interest_Rate, type = 'l', col = 'darkgreen', xlab = 'Date', ylab = 'Interest Rate')

abline(v = ediv_time, col = rgb(1, 0, 0, alpha = 0.3), lwd = 2, lty = 2)

#we now check the independence assumptions

##normal normal plots for each regime:

first_time <- min(cluster_1_data$TIME)
last_time <- max(cluster_1_data$TIME)

t1 <- sort(c(unique(cluster_1_data$TIME)[ediv], first_time, last_time))

last_observation <- swiss[nrow(swiss), ]
segments <- lapply(1:(length(t1) - 1), function(i) {
  start_time <- t1[i]
  end_time <- t1[i + 1]
  subset(swiss, TIME >= start_time & TIME < end_time)
})

segments[[length(segments)]] <- rbind(segments[[length(segments)]], last_observation)
for (i in seq_along(segments)) {
  cat("Segment", i, ":\n")
  print(segments[[i]])
  cat("\n")
}
qqplots <- list()
for (i in 1:length(segments)) {
  segment <- segments[[i]]

  qqplot <- ggplot(segment, aes(sample = Interest_Rate)) +
    geom_qq() +
    geom_qq_line(color = "red") +
    labs(title = paste("QQ Plot for Group", i))
  
  qqplots[[i]] <- qqplot
}

combined_plot <- cowplot::plot_grid(plotlist = qqplots, ncol = 2)
print(combined_plot)
par(mfrow = c(4, 2))
for (i in 1:length(segments)) {
  acf(segments[[i]]$Interest_Rate, main = paste("ACF for Group", i))
}
par(mfrow = c(1, 1))

acf(swiss$Interest_Rate, main = "ACF for Entire Time Series")


t2 <- sort(c(unique(cluster_1_data$TIME)[inspect], first_time, last_time))

swiss[nrow(swiss), ]

