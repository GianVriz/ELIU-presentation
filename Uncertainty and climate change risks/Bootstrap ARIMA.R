###Bootstrap confidence intervals###
library(TSA)
library(stats)
library(dplyr)
library(forecast)
library(ggplot2)

GDPC1 <- read.csv("C:/Users/gian-/Desktop/Machine Learnign and GDP/GDPC1.csv", header=FALSE)
View(GDPC1)
diff<-diff(log(as.numeric(ret)))
diff[1]=0
plot(diff, type='l')
cusum<-cumsum(diff[1:296])

plot(cusum, type='l')
cusum_ts <- ts(cusum,start = c(1947,1),frequency = 4)

model<-Arima(cusum_ts, order = c(2,2,1))
checkresiduals(model)

#Simulation function

#Simulation function
library(purrr)
library(forecast)

sim_forecast <-  function(data, nsim=100, h, level=95){
  
  sim <- bld.mbb.bootstrap(data, nsim, block_size = 20)
  
  h <- as.integer(h)
  future <- matrix(0, nrow=nsim, ncol=h)
  
  future <- sim %>% map(function(x){simulate(Arima(x,order = c(2,2,1)),nsim=h)}) %>% 
    unlist() %>% matrix(ncol = h, nrow = nsim, byrow = TRUE)
  
  start <- tsp(data)[2]+1/4
  
  simfc <- structure(list(
    
    mean = future %>% colMeans() %>% ts(start = start, frequency = 4),
    
    lower = future %>% as.data.frame() %>% 
      map_dbl(quantile, prob = (1-level/100)/2) %>% 
      ts(start = start,frequency = 4),
    
    upper = future %>% as.data.frame() %>% 
      map_dbl(quantile, prob = (1-level/100)/2+level/100) %>% 
      ts(start = start,frequency = 4),
    
    level=level),
    class="forecast")
  
  assign("simfc",simfc,envir = .GlobalEnv)
  
  simfc
  
}


arima_bagg<-sim_forecast(cusum_ts,h=50)
#arima_b<-forecast(model, 8, bootstrap = TRUE, npaths = 1000)
arima<-forecast(model, 50)

cusum_ts2 = ts(cumsum(diff[2:296])[291:296],start = c(2019,4),frequency = 4)
autoplot(cusum_ts2) +
  ggtitle("GDP") +
  xlab("Year") + ylab("Price") +
  autolayer(arima, series="ARIMA",PI=TRUE)+
  autolayer(arima_bagg, series="Bagged ARIMA",PI=TRUE) 
  theme_light()


