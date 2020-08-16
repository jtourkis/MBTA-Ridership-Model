library(dplyr)
library(tidyverse)
library(cowplot)
library(ggplot2)


Route_28<-read_csv("https://raw.githubusercontent.com/jtourkis/MBTA-Ridership-Model/master/data/MBTA_Fall_Route_28_Bus_Ridership_2016_2019.csv")

####Goal: The goal of this notebook is to begin to explore which probability distribution best simulates the process of riders boarding at each stop of a MBTA bus.####

#### Review Density Plots of Data###

plot_28 <- ggplot(Route_28, aes(x=average_ons)) + 
  geom_density()
print(plot_28)

Route_28_Peak_AM<-filter(Route_28, time_period_name=="AM_PEAK" )
plot_28_Peak_AM <- ggplot(Route_28_Peak_AM, aes(x=average_ons)) + 
  geom_density()
print(plot_28_Peak_AM)


#####Explore characteristics of Peak AM Density Plot###


density1<-density(Route_28_Peak_AM$average_ons)

ggplot(data.frame(x = density1$x, y = density1$y), aes(x, y)) + 
  geom_density(stat = "identity", fill = 'burlywood', alpha = 0.3) 

density1$x[which.max(density1$y)]

mean(Route_28_Peak_AM$average_ons)




####FINDING DISTRIBUTION OF DATA####

####Review Approx. Range of Values and Counts#####

####AM Peak####

route_28_boarding<-data.frame(table(round(Route_28_Peak_AM$average_ons)))
route_28_boarding$Var1<-as.numeric(route_28_boarding$Var1)
print(route_28_boarding)


#####SIMULATE PREDICTED VALUES AND COMPARE TO ACTUAL BOARDING VALUES####


###SIMULATE GEOMETRIC DISTRIBUTION AND COMPARE###
# Set seed for reproducibility

####Find MLE Parameter Estimate for Geometric Distribution:

N <- sum(route_28_boarding$Freq)   
Xn<-mean(Route_28_Peak_AM$average_ons)
p_MLE<- 1/(Xn+1)

###Simulate###
# Set seed for reproducibility
set.seed(13535)  
y_geom <- rnbinom(N, size = 1, prob = p_MLE) 

par(mfrow=c(2,1))

##Histogram Comparison###
hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")
hist(y_geom,                                          # Plot of randomly drawn geom histogram
     breaks = 100,
     main = "Geometric Simulation (Using pMLE)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")


###SIMULATE POISSON DISTRIBUTION AND COMPARE###
###Note: Mean is MLE Estimate for Lambda###

###Simulate###
# Set seed for reproducibility
set.seed(23533)  
y_pois<-rpois(n = N, lambda = mean(Route_28_Peak_AM$average_ons)  )

##Histogram Comparison###
hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

hist(y_pois,                                          # Plot of randomly drawn poisson histogram
     breaks = 100,
     main = "Poisson Simulation (Using pMLE)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")


###SIMULATE GAMMA DISTRIBUTION AND COMPARE###
##Use Method of Moments Estimators for Parameters### 

#E(X)=k*theta
#V(X)=k*theta^2
#k_hat= Xn^2/S_2
#theta=S_2/Xn

Xn<-mean(Route_28_Peak_AM$average_ons)
var_gam<- var(Route_28_Peak_AM$average_ons)
theta_gam<-var_gam/Xn
k_gam<-Xn^2/var_gam

###Simulate###
# Set seed for reproducibility
set.seed(33525)  
y_gamma<-rgamma(n = N, shape = k_gam , scale = theta_gam)

##Histogram Comparison###
hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

hist(y_gamma,                                          # Plot of randomly drawn gamma histogram
     breaks = 11,
     main = "Gamma Simulation (Using Method of Moments)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

###SIMULATE NEGATIVE BINOMIAL DISTRIBUTION AND COMPARE###

##Use Method of Moments Estimators for Parameters###

var_nb<- var(Route_28_Peak_AM$average_ons)

p_MM<- 1-(Xn/var_nb)

r_MM<- (Xn*(1-p_MM))/p_MM

print(r_MM)

###Simulate###

# Set seed for reproducibility
set.seed(423535)  
y_nbinom <- rnbinom(N, size = r_MM, prob = p_MM) 


##Histogram Comparison###

hist(Route_28_Peak_AM$average_ons, breaks = 11, main="Average Riders at Stops Histogram", xlab="Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

hist(y_nbinom ,                                          # Plot of randomly drawn nbinom histogram
     breaks = 100,
     main = "Neg Binom Simulation (Using Method of Moments)", xlab="Predicted Riders Boarding at Stop",
     ylab="Frequency", sub="MBTA Route 28 Peak AM")

#####GAMMA KOLMOGOROV SMIRNOV TEST####

gamma_sim<-y_gamma
actual<- Route_28_Peak_AM$average_ons


ks.test(gamma_sim, actual)

###H0 is the two distributions are equal.
## P-Value < 0.05 (significance level), reject that they are drawn from same distribution.
## D greater than critical value reject null. 

###n=292
#Significance level:  α = 0.001
#Over 50 Critical value:  1.94947   
#Critical region:  Reject H0 if D > 1.94947

####MIT CHI SQUARED BINNING TEST METHOD#####

actual_round<-data.frame(table(round(Route_28_Peak_AM$average_ons)))
binom<-data.frame(table(y_nbinom))
pois<-data.frame(table(y_pois))
geom<-data.frame(table(y_geom))

####Find P Negative Binomial for Bins using cdf####

#Bins: 0 to .5, .5 to 1.5, 1.5 to 2.5, 2.5 to 3.5, 3.5 to 4.5, 4.5 to 5.5, 5.5 to 6.5, 6.5 to 7.5, 7.5 to 8.5, 8.5 to 9.5, 9.5 to Inf
nbBin0<-pnbinom(.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)
nbBin1<-pnbinom(1.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0
nbBin2<-pnbinom(2.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1
nbBin3<-pnbinom(3.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1-nbBin2
nbBin4<-pnbinom(4.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1-nbBin2-nbBin3
nbBin5<-pnbinom(5.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1-nbBin2-nbBin3-nbBin4
nbBin6<-pnbinom(6.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1-nbBin2-nbBin3-nbBin4-nbBin5
nbBin7<-pnbinom(7.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1-nbBin2-nbBin3-nbBin4-nbBin5-nbBin6
nbBin8<-pnbinom(8.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1-nbBin2-nbBin3-nbBin4-nbBin5-nbBin6-nbBin7
nbBin9<-pnbinom(9.5, size = r_MM, prob = p_MM, lower.tail = TRUE, log.p = FALSE)-nbBin0-nbBin1-nbBin2-nbBin3-nbBin4-nbBin5-nbBin6-nbBin7-nbBin8
nbBin10<-pnbinom(9.5, size = r_MM, prob = p_MM, lower.tail = FALSE, log.p = FALSE)

nbPs<-c(nbBin0,nbBin1,nbBin2,nbBin3,nbBin4,nbBin5,nbBin6,nbBin7,nbBin8,nbBin9,nbBin10)

merged_binom2<-merge(binom,actual_round, by.x="y_nbinom", by.y="Var1", all=TRUE)
merged_binom2$Freq.y[is.na(merged_binom2$Freq.y)] <- 0
merged_binom2$Freq.x[is.na(merged_binom2$Freq.x)] <- 0
merged_binom2$total<-N
merged_binom2$Nj_Rate<-merged_binom2$Freq.y/merged_binom2$total
merged_binom2$p_Bins<-nbPs

#####Combine Bins Less than 5####

merged_binom2[8,]<-merged_binom2[8,]+merged_binom2[9,]+merged_binom2[10,]+merged_binom2[11,]
merged_binom2<-merged_binom2[-9:-11,]
merged_binom2$y_nbinom<-as.numeric(merged_binom2$y_nbinom)
merged_binom2$y_nbinom[is.na(merged_binom2$y_nbinom)] <- "Bin 7-10"
  
print(merged_binom2)

chisq.test(x= merged_binom2$Freq.y, p = merged_binom2$p_Bins)
#chisq.test(x= merged_binom2$Freq.y, p = merged_binom2$p_Bins, simulate.p.value=TRUE, B=1e6)


print("Nbinom MIT Test Stat")

N*sum((merged_binom2$Nj_Rate-merged_binom2$p_Bins)^2/merged_binom2$p_Bins)


#####Chi Square Test Geometric Distribution####

####Find P Geometric Distribution for Bins using cdf####

#Bins: 0 to .5, .5 to 1.5, 1.5 to 2.5, 2.5 to 3.5, 3.5 to 4.5, 4.5 to 5.5, 5.5 to 6.5, 6.5 to 7.5, 7.5 to 8.5, 8.5 to 9.5, 9.5 to Inf
GeomBin0<-pnbinom(.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)
GeomBin1<-pnbinom(1.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0
GeomBin2<-pnbinom(2.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-nbBin1
GeomBin3<-pnbinom(3.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2
GeomBin4<-pnbinom(4.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2-GeomBin3
GeomBin5<-pnbinom(5.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2-GeomBin3-GeomBin4
GeomBin6<-pnbinom(6.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2-GeomBin3-GeomBin4-GeomBin5
GeomBin7<-pnbinom(7.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2-GeomBin3-GeomBin4-GeomBin5-GeomBin6
GeomBin8<-pnbinom(8.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2-GeomBin3-GeomBin4-GeomBin5-GeomBin6-GeomBin7
GeomBin9<-pnbinom(9.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2-GeomBin3-GeomBin4-GeomBin5-GeomBin6-GeomBin7-GeomBin8
GeomBin10<-pnbinom(10.5, size = 1, prob = p_MLE, lower.tail = TRUE, log.p = FALSE)-GeomBin0-GeomBin1-GeomBin2-GeomBin3-GeomBin4-GeomBin5-GeomBin6-GeomBin7-GeomBin8-GeomBin9
GeomBin11<-pnbinom(10.5, size = 1, prob = p_MLE, lower.tail = FALSE, log.p = FALSE)

geomPs<-c(GeomBin0,GeomBin1,GeomBin2,GeomBin3,GeomBin4,GeomBin5,GeomBin6,GeomBin7,GeomBin8,GeomBin9,GeomBin10,GeomBin11)

sum(geomPs)

merged_geom2<-merge(geom,actual_round, by.x="y_geom", by.y="Var1", all=TRUE)
merged_geom2$Freq.y[is.na(merged_geom2$Freq.y)] <- 0
merged_geom2$Freq.x[is.na(merged_geom2$Freq.x)] <- 0
merged_geom2$total<-N
merged_geom2$Nj_Rate<-merged_geom2$Freq.y/merged_geom2$total
merged_geom2$p_Bins<-geomPs


#####Combine Bins Less than 5####

merged_geom2[7,]<-merged_geom2[7,]+merged_geom2[8,]+merged_geom2[9,]+merged_geom2[10,]+merged_geom2[11,]+merged_geom2[12,]
merged_geom2<-merged_geom2[-8:-12,]
merged_geom2$y_geom<-as.numeric(merged_geom2$y_geom)
merged_geom2$y_geom[is.na(merged_geom2$y_geom)] <- "Bin 6-11"

print(merged_geom2)


chisq.test(x= merged_geom2$Freq.y, p = merged_geom2$p_Bins)

#chisq.test(x= merged_geom2$Freq.y, p = merged_geom2$p_Bins, simulate.p.value=TRUE, B=1e6)
           
print("Geom MIT Test Stat")


N*sum((merged_geom2$Nj_Rate-merged_geom2$p_Bins)^2/merged_geom2$p_Bins)

#####Find Expected Categories for the Actual Data Repeated 1000 Times

actual_round_1000<-data.frame(table(round(Route_28_Peak_AM$average_ons)))
actual_round_1000$ActualFreqx1000<-actual_round_1000$Freq*1000
print(actual_round_1000)


#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The Gamma Distribution######

set.seed(13)
gamma_x<-replicate(n = 1000, 
          round(rgamma(n = N, shape = k_gam , scale = theta_gam) ))
predicted_1000<-data.frame(table(gamma_x))

Accuracy_df<-merge(predicted_1000,actual_round_1000, by.x="gamma_x", by.y="Var1", all=TRUE)
Accuracy_df$Freq.y[is.na(Accuracy_df$Freq.y)] <- 0
Accuracy_df$Freq.x[is.na(Accuracy_df$Freq.x)] <- 0
Accuracy_df$ActualFreqx1000[is.na(Accuracy_df$ActualFreqx1000)] <- 0
Accuracy_df$gamma_x<-as.numeric(Accuracy_df$gamma_x)
print(Accuracy_df)

Accuracy_df$correct<-pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000)

num_correct<-sum(pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000))

num_incorrect<-sum(abs(Accuracy_df$ActualFreqx1000-Accuracy_df$Freq.x))

gamma_accuracy<-num_correct/(num_correct+num_incorrect)
  
  
print(gamma_accuracy)



####Calculate the number of people off:

Actual_Total<-sum(Accuracy_df$ActualFreqx1000*Accuracy_df$gamma_x)

gamma_Pred_Total<-sum(Accuracy_df$Freq.x*Accuracy_df$gamma_x)

abs(Actual_Total-gamma_Pred_Total)

#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The Poisson Distribution######

set.seed(14)
pois_x<-replicate(n = 1000, 
          rpois(n = N, lambda = mean(Route_28_Peak_AM$average_ons)))
pois_predicted_1000<-data.frame(table(pois_x))

Accuracy_df<-merge(pois_predicted_1000,actual_round_1000, by.x="pois_x", by.y="Var1", all=TRUE)
Accuracy_df$Freq.y[is.na(Accuracy_df$Freq.y)] <- 0
Accuracy_df$Freq.x[is.na(Accuracy_df$Freq.x)] <- 0
Accuracy_df$ActualFreqx1000[is.na(Accuracy_df$ActualFreqx1000)] <- 0
Accuracy_df$pois_x<-as.numeric(Accuracy_df$pois_x)
print(Accuracy_df)

Accuracy_df$correct<-pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000)

num_correct<-sum(pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000))

num_incorrect<-sum(abs(Accuracy_df$ActualFreqx1000-Accuracy_df$Freq.x))

pois_accuracy<-num_correct/(num_correct+num_incorrect)
  
  
print(pois_accuracy)

####Calculate the number of people off:

pois_Pred_Total<-sum(Accuracy_df$Freq.x*Accuracy_df$pois_x)

abs(Actual_Total-pois_Pred_Total)

#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The NBinom Distribution######

set.seed(15)
nbin_x<-replicate(n = 1000, 
          rnbinom(N, size = r_MM, prob = p_MM))
nbin_predicted_1000<-data.frame(table(nbin_x))

Accuracy_df<-merge(nbin_predicted_1000,actual_round_1000, by.x="nbin_x", by.y="Var1", all=TRUE)
Accuracy_df$Freq.y[is.na(Accuracy_df$Freq.y)] <- 0
Accuracy_df$Freq.x[is.na(Accuracy_df$Freq.x)] <- 0
Accuracy_df$ActualFreqx1000[is.na(Accuracy_df$ActualFreqx1000)] <- 0
Accuracy_df$nbin_x<-as.numeric(Accuracy_df$nbin_x)
print(Accuracy_df)

Accuracy_df$correct<-pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000)

num_correct<-sum(pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000))

num_incorrect<-sum(abs(Accuracy_df$ActualFreqx1000-Accuracy_df$Freq.x))

nbin_accuracy<-num_correct/(num_correct+num_incorrect)
  
  
print(nbin_accuracy)

####Calculate the number of people off:

nbin_Pred_Total<-sum(Accuracy_df$Freq.x*Accuracy_df$nbin_x)
abs(Actual_Total-nbin_Pred_Total)
#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The Geom Distribution######

set.seed(17)
geom_x<-replicate(n = 1000, 
          rnbinom(N, size = 1, prob = p_MLE))

geom_predicted_1000<-data.frame(table(geom_x))

Accuracy_df<-merge(geom_predicted_1000,actual_round_1000, by.x="geom_x", by.y="Var1", all=TRUE)

Accuracy_df$Freq.y[is.na(Accuracy_df$Freq.y)] <- 0
Accuracy_df$Freq.x[is.na(Accuracy_df$Freq.x)] <- 0
Accuracy_df$ActualFreqx1000[is.na(Accuracy_df$ActualFreqx1000)] <- 0
Accuracy_df$geom_x<-as.numeric(Accuracy_df$geom_x)
print(Accuracy_df)

Accuracy_df$correct<-pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000)

num_correct<-sum(pmin(Accuracy_df$Freq.x,Accuracy_df$ActualFreqx1000))

num_incorrect<-sum(abs(Accuracy_df$ActualFreqx1000-Accuracy_df$Freq.x))

geom_accuracy<-num_correct/(num_correct+num_incorrect)
print(geom_accuracy)


####Calculate the number of people off:

geom_Pred_Total<-sum(Accuracy_df$Freq.x*Accuracy_df$geom_x)

abs(Actual_Total-geom_Pred_Total)

######Accuracy Results####

print("Gamma Distribution Accuracy in 1000 Simulations:")

print("Accurate Category Prediction:")
print(gamma_accuracy)

print("Total Riders Deviation:")
Actual_Total-gamma_Pred_Total

print("Poisson Distribution Accuracy in 1000 Simulations:")

print("Accurate Category Prediction:")
print(pois_accuracy)

print("Total Riders Deviation:")
Actual_Total-pois_Pred_Total

print("Negative Binomial Distribution Accuracy in 1000 Simulations:")

print("Accurate Category Prediction:")
print(nbin_accuracy)

print("Total Riders Deviation:")
Actual_Total-nbin_Pred_Total

print("Geometric Distribution Accuracy in 1000 Simulations:")

print("Accurate Category Prediction:")
print(geom_accuracy)

print("Total Riders Deviation:")
Actual_Total-geom_Pred_Total

# "Gamma Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
#0.7738467
# "Total Riders Deviation:"
# 1164

#  "Poisson Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
# 0.7702441
# "Total Riders Deviation:"
# -7835

# "Negative Binomial Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
# 0.7452268
# "Total Riders Deviation:"
# -43385

# "Geometric Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
# 0.6846381
# "Total Riders Deviation:"
# -9101
