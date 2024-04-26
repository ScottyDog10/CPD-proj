#load gilts

library(ggplot2)
library(ggthemes)
ggplot(gilts, aes(x = factor(Year), y = Gross_issuance_billion_GBP)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Year", y = "Billions (USD)", title = 'Gross Issuance of Gilts in Billions USD') +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
df <- gilts[1:30, ]

#[14:27, ]

ggplot(df, aes(x = factor(Year), y = Gross_issuance_billion_GBP)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Year", y = "Billions (USD)", title = 'Gross Issuance of Gilts in Billions USD') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gg <- ggplot(df, aes(x = factor(Year), y = Gross_issuance_billion_GBP)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Year", y = "Billions (USD)", title = 'Gross Issuance of Gilts in Billions USD') +
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


x <- df$Gross_issuance_billion_GBP

length(x)

gibbs = function(N,data) 
{
  set.seed(123)
  n = length(data)
  theta = 1; lambda = 1; k = 20  #initial param values
  a1 = 1/9000; a2 = 1/100000; b1 = 1/300; b2 = 1/1000 #prior hyper-params
  probVec = rep(0,n)             #condiional pmf for k
  out = matrix(0,nrow=N,ncol=3)  #store samples here
  colnames(out) <- c("theta", "lambda", "tau")
  out[1,] = c(theta,lambda,k) #store initial sample
  
  for(i in 2:N)
  {
    #Update theta
    theta = rgamma(1,a1+k,b1+sum(data[1:k]))
    #Update lambda
    if(k==n)
    {
      #FCD is the prior
      lambda = rgamma(a2,b2)
    }else{
      lambda = rgamma(1,a2+n-k,b2+sum(data[(k+1):n]))
    }
    #calculate pmf for k
    for(j in 1:(n-1))
    {
      if(j!=n){
        probVec[j] = theta^j*exp(-theta*sum(data[1:j]))*lambda^(n-j)*exp(-lambda*sum(data[(j+1):n]))
      }
      else{
        probVec[j] = theta^j*exp(-theta*sum(data[1:j]))
      }
    }
    #Update k
    k = sample(n,size=1,prob=probVec)
    
    out[i,] = c(theta,lambda,k)
  }
  return(out)
}

out=gibbs(10000,x)

plot(ts(out))
plot(density(out[10:10000, 1]), ylim = c(0, 210), main = 'Posterior Density Plots Without Burn-In', col = "darkgreen")
legend("topright", legend = c("Theta", "Lambda"), col = c("darkgreen", "darkred"), lty = 1)
lines(density(out[10:10000, 2]), col = 'darkred')

#theta
quantile(out[10:10000,1],c(0.025,0.975))
#lambda
quantile(out[10:10000,2],c(0.025,0.975))

mean(out[10:10000,1])
mean(out[10:10000,2])

library(MASS)
dens=kde2d(out[10:10000,1],out[10:10000,3])
contour(dens,xlab="theta",ylab="lambda", xlim = c(0, 0.1), ylim = c(0, 0.015))

table(out[10:10000,3])

df$Year[which.max(table(out[10:10000,3]))]

hist_data <- hist(out[10:10000,3], main = '', xlab = 'Tau')

length(table(out[10:10000,3]))

plot(hist_data, main = 'Histogram of Tau', xlab = 'Tau', col = 'darkgreen')
