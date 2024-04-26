#MEI_FIN_02032024154817214.csv is the name of the originial short-term rates dataset

stir <- MEI_FIN_02032024154817214

plot(ts(stir[which(stir$Country == 'United States'), ]$Value))

subset_data <- stir[which(stir$Country == 'United States'), ]

stir$Country

min(stir$TIME)
library(ggplot2)
library(zoo)

str(stir)
stir$TIME <- as.Date(paste(stir$TIME, "-01", sep = ""), format = "%Y-%m-%d")
ggplot(data = stir[which(stir$Country == 'United States'), ], aes(x = TIME, y = Value)) +
  geom_line() +
  labs(x = "Time", y = "Percentage Interest Per Annum") +
  ggtitle("Short-Term Interest Rates in the United States")


ggplot(data = stir, aes(x = TIME, y = Value, color = Country)) +
  geom_line() +
  labs(x = "Time", y = "Percentage Interest Per Annum") +
  ggtitle("Short-Term Interest Rates by Country")

max(stir[which(stir$Country == 'Romania'), ]$Value)

date_of_max <- subset(stir, Country == 'Romania' & Value == max(stir[stir$Country == 'Romania', ]$Value))$TIME

library(tidyr)

stir_red <- data.frame('Country' = stir$Country, 'TIME' = stir$TIME, 'Value' = stir$Value)
transformed_data <- pivot_wider(data = stir_red , names_from = Country, values_from = Value)

jan_1980 <- as.Date("1980-01-01")
index_jan_1980 <- which(transformed_data$TIME == jan_1980)
usa_na_indices <- which(is.na(transformed_data$'United States'))
print(transformed_data[usa_na_indices, ]$TIME)

transformed_data_red <- transformed_data |>
  dplyr::filter(TIME > as.Date("2000-01-01"))

has_consecutive_nas <- function(x) {
  consecutive_nas <- 0
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      consecutive_nas <- consecutive_nas + 1
      if (consecutive_nas >= 2) {
        return(TRUE)
      }
    } else {
      consecutive_nas <- 0
    }
  }
  return(FALSE)
}



col_number_romania <- which(colnames(transformed_data_red) == "Romania")

#has_consecutive_nas(as.vector(transformed_data_red[47]))

has_consecutive_nas(as.vector(transformed_data_red[[47]]))

cols_rem <- c()
for(j in 2:ncol(transformed_data_red)){
  if(has_consecutive_nas(as.vector(transformed_data_red[[j]])))
  {
    cols_rem <- c(cols_rem, j) 
  }
}

df <- transformed_data_red[, -cols_rem]

transformed_data_long <- df %>%
  pivot_longer(cols = -TIME, names_to = "Country", values_to = "Interest_Rate")

library(zoo)

fill_missing_values <- function(x) {
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      if (i == 1) {
        x[i] <- x[i + 1]
      } else if (i == length(x)) {
        x[i] <- x[i - 1]
      } else {
        x[i] <- mean(x[i - 1], x[i + 1])
      }
    }
  }
  return(x)
}


transformed_data_long_filled <- transformed_data_long
transformed_data_long_filled$Interest_Rate <- 
  fill_missing_values(transformed_data_long$Interest_Rate)

#imputing the mean when there is a missing value 

ggplot(data = transformed_data_long_filled, aes(x = TIME, y = Interest_Rate, color = Country)) +
  geom_line() +
  labs(x = "Time", y = "Interest Rate") +
  ggtitle("Interest Rates by Country")

reversed_df <- transformed_data_long_filled %>%
  pivot_wider(names_from = Country, values_from = Interest_Rate)

library(dplyr)

ncol(reversed_df)-1

interest_rates <- select(reversed_df, -TIME)

set.seed(123)  
kmeans_result <- kmeans(t(interest_rates), centers = 4)

?kmeans
clustering_vector <- kmeans_result$cluster
cluster_labels <- setNames(clustering_vector, names(clustering_vector))
transformed_data_long_filled$Cluster <- cluster_labels[transformed_data_long_filled$Country]

transformed_data_long_filled$Country <- as.factor(transformed_data_long_filled$Country)
par(mfrow = c(2, 2))
for (cluster_id in unique(transformed_data_long_filled$Cluster)) {
  cluster_data <- transformed_data_long_filled %>% filter(Cluster == cluster_id)
  plot(NULL, xlim = range(transformed_data_long_filled$TIME), 
       ylim = range(transformed_data_long_filled$Interest_Rate),
       xlab = "Time", ylab = "Interest Rate",
       main = paste("Cluster", cluster_id))
  i = 1
  for (country in unique(cluster_data$Country)) {
    country_data <- cluster_data %>% filter(Country == country)
    lines(country_data$TIME, country_data$Interest_Rate, 
          col = i,
          type = "l")
    i = i + 1
  }
  legend("topright", legend = unique(cluster_data$Country), 
         col = unique(as.numeric(as.factor(cluster_data$Country))), 
         cex = 0.8)
}

par(mfrow = c(1, 1))

cluster_id = 1
cluster_data <- transformed_data_long_filled %>% filter(Cluster == cluster_id)

plot(NULL, xlim = range(transformed_data_long_filled$TIME), 
     ylim = range(cluster_data$Interest_Rate),
     xlab = "Time", ylab = "Interest Rate",
     main = paste("Cluster", cluster_id))
i = 1
for (country in unique(cluster_data$Country)) {
  country_data <- cluster_data %>% filter(Country == country)
  lines(country_data$TIME, country_data$Interest_Rate, 
        col = i,
        type = "l")
  i = i + 1
}

library(gridExtra)

plot_list <- list()

for (cluster_id in c(1, 2, 3, 4)) {
  cluster_data <- transformed_data_long_filled %>% filter(Cluster == cluster_id)
  p <- ggplot(cluster_data, aes(x = TIME, y = Interest_Rate, color = Country)) +
    geom_line() +
    labs(x = "Time", y = "Interest Rate", title = paste("Cluster", cluster_id)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot_list[[cluster_id]] <- p
}

grid.arrange(grobs = plot_list, ncol = 2)

plot_list <- list()

transformed_data_long_filled

transformed_data_long_filled$Country <- gsub("China \\(People's Republic of\\)", "China", transformed_data_long_filled$Country)
transformed_data_long_filled$Country <- gsub("Euro area \\(19 countries\\)", "Euro area", transformed_data_long_filled$Country)

length(unique(transformed_data_long_filled$Country))

nrow(reversed_df)
nrow(transformed_data_long_filled)

min(reversed_df$TIME)
max(reversed_df$TIME)
for (cluster_id in c(1, 2, 3, 4)) {
  cluster_data <- transformed_data_long_filled %>% filter(Cluster == cluster_id)
  p <- ggplot(cluster_data, aes(x = TIME, y = Interest_Rate, color = Country)) +
    geom_line() +
    labs(x = "Time", y = "Interest Rate (%)", title = paste("Cluster", cluster_id)) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +  
    scale_color_viridis_d() + 
    theme_gdocs() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.key.size = unit(0.5, "lines")) 
  
  plot_list[[cluster_id]] <- p
}

grid.arrange(grobs = plot_list, ncol = 2)
grid_arranged <- grid.arrange(grobs = plot_list, ncol = 2)

library(tidyr)

#load historical-libor-rates-chart.csv

colnames(libor)
libor$date <- as.Date(libor$date)
libor_long <- pivot_longer(libor, cols = -date, names_to = "Variable", values_to = "Value")

ggplot(libor_long, aes(x = date, y = Value, color = Variable)) +
  geom_line() +
  scale_color_viridis_d() +  
  labs(x = "Date", y = "Interest Rate (%)", title = "LIBOR Values") +
  theme_minimal()

#we will look at the one month LIBOR:
install.packages('ggthemes')
library(ggthemes)

libor_1m <- libor[, 1:2]

ggplot(libor_1m, aes(x = date, y = X1M_LIBOR, color = X1M_LIBOR)) +
  geom_line() + 
  scale_color_viridis_c() +  
  labs(x = "Date", y = "Interest Rate (%)", title = "1 Month LIBOR") +
  theme_minimal()

ggplot(libor_1m, aes(x = date, y = X1M_LIBOR, color = X1M_LIBOR)) +
  geom_line(col = 'darkgreen') + 
  #scale_color_gradient(low = "darkgreen", high = "darkgreen") +  
  labs(x = "Date", y = "Interest Rate (%)", title = "One-Month Interbank Borrowing Rates") +
  theme_gdocs()

nrow(libor)
min(libor$date)
max(libor$date)

plot <- ggplot(libor_1m, aes(x = date, y = X1M_LIBOR, color = X1M_LIBOR)) +
  geom_line(col = 'darkgreen') + 
  labs(x = "Date", y = "Interest Rate (%)", title = "One-Month Interbank Borrowing Rates") +
  theme_gdocs()
