#Standard Binary Segmentation

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


x[1]

tau <- c(0, 500, 1000)

mean <- c(-5, 0)

x <- gen_ts(tau, mean, sd = 0)

plot_with_segments(x, tau, mean)

max_cusum(x, 0)

t_values <- 100:250
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(x=t_values, y=ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")

which.max(cusum_results)

max_cusum(x[0:300], 0)

t_values <- 0:300
cusum_results <- sapply(t_values, function(t_val) CUSUM(x, t_val))

plot(x=t_values, y=ts(cusum_results), xlab = 't', ylab = expression(C[t]), main = "CUSUM over Time")

which.max(cusum_results)

create_intervals <- function(vec){
  
  df <- data.frame(rep(NA, length(vec)-1), rep(NA, length(vec)-1))
  colnames(df) <- list('Start', 'End')
  for(i in 1:length(vec)-1){
    df[i, ] <- c(vec[i], vec[i+1])
  }
  return(df)
}


tau <- c(0, 250, 750, 1000)

mean <- c(0, 4, 0)

x <- gen_ts(tau, mean, sd = 1)

plot_with_segments(x, tau, mean)


c <- 30
k <- 0
q <- 1000
tau_lst <- c(k, q)
max_cusum_result <- max_cusum(x[k:q])
if(max_cusum_result[[1]] > c){
  tau_hat <- max_cusum_result[[2]]
}

tau_lst <- sort(c(tau_lst, tau_hat))

# create intervals 

intervals <- create_intervals(tau_lst)

#run thru first row 

k <- intervals[1, 1]
q <- intervals[1, 2]

max_cusum(x[k:q])[[1]] > c

#TRUE so add to

tau_lst <- sort(c(tau_lst, max_cusum(x[k:q])[[2]]))

#run thru second row

k <- intervals[2, 1]
q <- intervals[2, 2]

max_cusum(x[k:q])[[1]] > c

#FALSE so do not add to the set

#still at least one TRUE so set j <- 1

#create new list of intervals

#create boolean vector
bool <- c()

intervals <- create_intervals(tau_lst)

k <- intervals[1, 1]
q <- intervals[1, 2]

max_cusum(x[k:q])[[1]] > c

bool <- c(bool, max_cusum(x[k:q])[[1]] > c)

#false so dont add

k <- intervals[2, 1]
q <- intervals[2, 2]

max_cusum(x[k:q])[[1]] > c

bool <- c(bool, max_cusum(x[k:q])[[1]] > c)

#false so dont add

k <- intervals[3, 1]
q <- intervals[3, 2]

max_cusum(x[k:q])[[1]] > c

bool <- c(bool, max_cusum(x[k:q])[[1]] > c)

#false so dont add

#all now FALSE so need indicator to return TRUE

all_false <- all(bool == FALSE)

bs_defunct <- function(x, k, q, c){
  tau_lst <- c(k, q)
  j <- 0
  j2 <- 0
  bool <- c()
  max_cusum_result <- max_cusum(x[k:q], k)
  if(max_cusum_result[[1]] > c){
    tau_hat <- max_cusum_result[[2]]
    j2 <- 1
  }
  tau_lst <- sort(c(tau_lst, tau_hat))
  intervals <- create_intervals(tau_lst)
  while(j != 1 & j2 == 1){
    for(i in 1:nrow(intervals)){
      k <- intervals[i, 1]
      q <- intervals[i, 2]
      if(max_cusum(x[k:q], k)[[1]] > c){
        tau_lst <- sort(c(tau_lst, max_cusum(x[k:q], k)[[2]]))
      }
      bool <- c(bool, max_cusum(x[k:q], k)[[1]] > c)
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



#note that the standard deviation needs to be reasonable, the threshold will depend on it


# tau <- seq(from = 0, to = 1000, by = 100)
# 
# mean <- gen_mc(2, 10, 1)
# 
# x <- gen_ts(tau, mean, sd = 1)
# 
# plot_with_segments(x, tau, mean)
# 
# bs(x, 0, 1000, 25)
# 

tau <- c(0, 200, 800, 1000)

mean <- c(0, 4, -2)

x <- gen_ts(tau, mean, sd = 2)

plot_with_segments(x, tau, mean)

# tau <- seq(from = 0, to = 1000, by = 100)
# 
# mean <- gen_mc(2, 10, 1)
# 
# x <- gen_ts(tau, mean, sd = 1)

bs(x, 0, 1000, 10)

#performed well within twenty of the actual change

#note that the higher the number of change points the lower threshold value we need
#a long with a lower threshold value when there is higher variance

#max_cusum(x)

#####################################

max_cusum(x[400:800], 400)

plot(x[598:604])

k <- 0
q <- 1000
c <- 10
tau_lst <- c(k, q)
j <- 0
j2 <- 0
bool <- c()
max_cusum_result <- max_cusum(x[k:q], k)
if(max_cusum_result[[1]] > c){
  tau_hat <- max_cusum_result[[2]]
  j2 <- 1
}
tau_lst <- sort(c(tau_lst, tau_hat))
intervals <- create_intervals(tau_lst)
historical_intervals <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(historical_intervals) <- c('Start', 'End')
print(c(k, q))
print(max_cusum(x[k:q], k)[[1]] )
print(max_cusum(x[k:q], k)[[1]] > c)
print(tau_hat)
while(j != 1 & j2 == 1){
  print('Loop')
  for(i in 1:nrow(intervals)){
    k <- intervals[i, 1]
    q <- intervals[i, 2]
    print(c(k, q))
    print(max_cusum(x[k:q], k)[[1]] )
    print(max_cusum(x[k:q], k)[[1]] > c)
    tau_hat <- max_cusum(x[k:q], k)[[2]]
    if(max_cusum(x[k:q], k)[[1]] > c & !(tau_hat %in% c(k, q))){
      print(tau_hat)
      tau_lst <- sort(unique(c(tau_lst, tau_hat)))
    }
    #else{
      #historical_intervals <- rbind(historical_intervals, c(k, q)) # i created this so that the function does not have to run over the same interval more than once, not worth it
      #colnames(historical_intervals) <- c('Start', 'End')
    #}
    bool <- c(bool, max_cusum(x[k:q], k)[[1]] > c & !(tau_hat %in% c(k, q)))
  }
  #index <- intersect(historical_intervals, intervals)
  intervals <- create_intervals(tau_lst)
  all_false <- all(bool == FALSE)
  if(all_false == TRUE){
    j <- 1
  }
  bool <- c()
}

index <- intersect(historical_intervals, intervals)
