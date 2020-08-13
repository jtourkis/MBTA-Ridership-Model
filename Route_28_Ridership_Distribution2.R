library(dplyr)
library(tidyverse)
library(cowplot)
library(ggplot2)


Route_28<-read_csv("../data/MBTA_Fall_Route_28_Bus_Ridership_2016_2019.csv")


plot_28 <- ggplot(Route_28, aes(x=average_ons)) + 
  geom_density()
print(plot_28)

Route_28_Peak_AM<-filter(Route_28, time_period_name=="AM_PEAK" )
plot_28_Peak_AM <- ggplot(Route_28_Peak_AM, aes(x=average_ons)) + 
  geom_density()
print(plot_28_Peak_AM)


#####Find Route 28 Mode###


density1<-density(Route_28_Peak_AM$average_ons)

ggplot(data.frame(x = density1$x, y = density1$y), aes(x, y)) + 
  geom_density(stat = "identity", fill = 'burlywood', alpha = 0.3) 

density1$x[which.max(density1$y)]

mean(Route_28_Peak_AM$average_ons)




####Testing For Negative Binomial Boarding####

####AM Peak####

route_28_boarding<-data.frame(table(round(Route_28_Peak_AM$average_ons)))
route_28_boarding$Var1<-as.numeric(route_28_boarding$Var1)
print(route_28_boarding)

#####Estimated Mean#####
riders_x_frequency<-route_28_boarding$Freq*route_28_boarding$Var1
riders_squared<-route_28_boarding$Var1^2


m<- sum(sum_riders_x_frequency)/sum(route_28_boarding$Freq)

print(sum_riders_x_frequency)
print(m)


#####Simulate Predicted Values####


N <- sum(route_28_boarding$Freq)   

####size is r#####
####N is sample size
par(mfrow=c(2,1))

# Set seed for reproducibility
set.seed(13535)  
y_geom <- rnbinom(N, size = 1, prob = p_MLE) 


hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")
hist(y_geom,                                          # Plot of randomly drawn nbinom density
     breaks = 100,
     main = "Geometric Simulation (Using pMLE)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

# Set seed for reproducibility
set.seed(23533)  
y_pois<-rpois(n = N, lambda = mean(Route_28_Peak_AM$average_ons)  )

hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

hist(y_pois,                                          # Plot of randomly drawn nbinom density
     breaks = 100,
     main = "Poisson Simulation (Using pMLE)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")


#### Gamma Distribution ######
#E(X)=k*theta
#V(X)=k*theta^2
#k_hat= Xn^2/S_2
#theta=S_2/Xn

####Method of Moments Estimators

Xn<-mean(Route_28_Peak_AM$average_ons)
var_gam<- var(Route_28_Peak_AM$average_ons)
theta_gam<-var_gam/Xn
k_gam<-Xn^2/var_gam

# Set seed for reproducibility
set.seed(33525)  
y_gamma<-rgamma(n = N, shape = k_gam , scale = theta_gam)

hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

hist(y_gamma,                                          # Plot of randomly drawn nbinom density
     breaks = 11,
     main = "Gamma Simulation (Using Method of Moments)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

####Negative Binomial####

var_nb<- var(Route_28_Peak_AM$average_ons)

p_MM<- 1-(Xn/var_nb)

r_MM<- (Xn*(1-p_MM))/p_MM

print(r_MM)

# Set seed for reproducibility
set.seed(423535)  
y_nbinom <- rnbinom(N, size = r_MM, prob = p_MM) 


hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")
hist(y_nbinom ,                                          # Plot of randomly drawn nbinom density
     breaks = 100,
     main = "Neg Binom Simulation (Using Method of Moments)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")