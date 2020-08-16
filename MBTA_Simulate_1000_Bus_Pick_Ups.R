library(dplyr)
library(tidyverse)
library(cowplot)
library(ggplot2)


Route_28<-read_csv("https://raw.githubusercontent.com/jtourkis/MBTA-Ridership-Model/master/data/MBTA_Fall_Route_28_Bus_Ridership_2016_2019.csv")
Route_28_Peak_AM<-filter(Route_28, time_period_name=="AM_PEAK" )

####Goal: The goal of this notebook is to begin to perform 1000 simulations from four distributions and explore how they perform against actual MBTA ridership data.####
####The four distributions are Gamma, Poission, Geometric, and Negative Binomial. 

#####Find Expected Categories for the Actual Data Repeated 1000 Times

actual_round_1000<-data.frame(table(round(Route_28_Peak_AM$average_ons)))
actual_round_1000$ActualFreqx1000<-actual_round_1000$Freq*1000
print(actual_round_1000)

####GAMMA DISTRIBUTION######
##Use Method of Moments Estimators for Parameters### 

#E(X)=k*theta
#V(X)=k*theta^2
#k_hat= Xn^2/S_2
#theta=S_2/Xn

N<-NROW(Route_28_Peak_AM$average_ons)
Xn<-mean(Route_28_Peak_AM$average_ons)
var_gam<- var(Route_28_Peak_AM$average_ons)
theta_gam<-var_gam/Xn
k_gam<-Xn^2/var_gam

#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The Gamma Distribution######

avg_boarding<- data.frame(round(Route_28_Peak_AM$average_ons))

set.seed(13)
gamma_x<-replicate(n = 1000, 
                   round(rgamma(n = N, shape = k_gam , scale = theta_gam) ))

gamma_x_df2<-data.frame(gamma_x)

deviation<-gamma_x_df2-t(avg_boarding)

missed_riders_gamma<-as.matrix(colSums(deviation))
#sum(deviation$X1)

####Plot Distribution of Estimated Per Sumulation Bias (Number of Riders Missed)#####

#hist(missed_riders_gamma, main = "", xlab = "Gamma Missed Riders Per Simulation", prob = T, col = "darkred")
#lines(density(missed_riders_gamma), col = "darkblue", lwd = 2)

##########CREATE TABLE OF COUNTS FOR EACH CATEGORY######

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

######POISSON DISTRIBUTION#####

avg_boarding<- data.frame(Route_28_Peak_AM$average_ons)

#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The Poisson Distribution######

set.seed(14)
pois_x<-replicate(n = 1000, 
                  rpois(n = N, lambda = mean(Route_28_Peak_AM$average_ons)))

####Missed Riders

pois_x_df2<-data.frame(pois_x)

deviation<-pois_x_df2-t(avg_boarding)

missed_riders_pois<-as.matrix(colSums(deviation))
#sum(deviation$X1)

####Plot Distribution of Estimated Per Sumulation Bias (Number of Riders Missed)#####
#hist(missed_riders_pois, main = "", xlab = "Poisson Missed Riders Per Simulation", prob = T, col = "darkred")
#lines(density(missed_riders_pois), col = "darkblue", lwd = 2)

##########CREATE TABLE OF COUNTS FOR EACH CATEGORY######

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


##### NEGATIVE BINOMIAL DISTRIBUTION####

##Use Method of Moments Estimators for Parameters###

var_nb<- var(Route_28_Peak_AM$average_ons)

p_MM<- 1-(Xn/var_nb)

r_MM<- (Xn*(1-p_MM))/p_MM

#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The NBinom Distribution######

set.seed(15)
nbin_x<-replicate(n = 1000, 
                  rnbinom(N, size = r_MM, prob = p_MM))

####Missed Riders

nbin_x_df2<-data.frame(nbin_x)

deviation<-nbin_x_df2-t(avg_boarding)

missed_riders_nbin<-as.matrix(colSums(deviation))
#sum(deviation$X1)

####Plot Distribution of Estimated Per Sumulation Bias (Number of Riders Missed)#####
#hist(missed_riders_nbin, main = "", xlab = "Negative Binomial Missed Riders Per Simulation", prob = T, col = "darkred")
#lines(density(missed_riders_nbin), col = "darkblue", lwd = 2)

##########CREATE TABLE OF COUNTS FOR EACH CATEGORY######

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

######GEOMETRIC DISTRIBUTION#####

####Find MLE Parameter Estimate for Geometric Distribution:

N <- sum(route_28_boarding$Freq)   
Xn<-mean(Route_28_Peak_AM$average_ons)
p_MLE<- 1/(Xn+1)

#####Get a Matrix of Estimates###
####1000 Columns with N Simulations From The Geom Distribution######

set.seed(17)
geom_x<-replicate(n = 1000, 
                  rnbinom(N, size = 1, prob = p_MLE))

####Missed Riders

geom_x_df2<-data.frame(geom_x)

deviation<-geom_x_df2-t(avg_boarding)

missed_riders_geom<-as.matrix(colSums(deviation))
#sum(deviation$X1)

####Plot Distribution of Estimated Per Sumulation Bias (Number of Riders Missed)#####
#hist(missed_riders_geom, main = "", xlab = "Geometric Missed Riders Per Simulation", prob = T, col = "darkred")
#lines(density(missed_riders_geom), col = "darkblue", lwd = 2)

##########CREATE TABLE OF COUNTS FOR EACH CATEGORY######

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

print("Estimated Total Missed Riders in 1000 Simulations (Rounded Avg Bording/Rounded Gamma):")
Actual_Total-gamma_Pred_Total

print("Model Bias:")
print(mean(missed_riders_gamma))

print("Estimated Total Missed Riders in 1000 Simulations of N (Rounded Avg Bording/Rounded Gamma)")
sum(missed_riders_gamma)

print("Poisson Distribution Accuracy in 1000 Simulations:")

print("Accurate Category Prediction:")
print(pois_accuracy)

print("Estimated Total Missed Riders in 1000 Simulations (Rounded Avg Bording):")
Actual_Total-pois_Pred_Total

print("Model Bias:")
print(mean(missed_riders_pois))

print("Estimated Total Missed Riders in 1000 Simulations of N (Avg Bording)")
sum(missed_riders_pois)

print("Negative Binomial Distribution Accuracy in 1000 Simulations:")

print("Accurate Category Prediction:")
print(nbin_accuracy)

print("Estimated Total Missed Riders in 1000 Simulations (Rounded Avg Bording):")
Actual_Total-nbin_Pred_Total

print("Model Bias:")

print(mean(missed_riders_nbin))

print("Estimated Total Missed Riders in 1000 Simulations of N")
sum(missed_riders_nbin)

print("Geometric Distribution Accuracy in 1000 Simulations:")

print("Accurate Category Prediction:")
print(geom_accuracy)

print("Estimated Total Missed Riders in 1000 Simulations (Rounded Avg Bording):")
Actual_Total-geom_Pred_Total

print("Model Bias:")

print(mean(missed_riders_geom))

print("Estimated Total Missed Riders in 1000 Simulations of N (Avg Boarding)")
sum(missed_riders_geom)

####Plot Distribution of Estimated Per Sumulation Bias (Number of Riders Missed)#####
hist(missed_riders_gamma, main = "", xlab = "Gamma Missed Riders Per Simulation", prob = T, col = "darkred")
lines(density(missed_riders_gamma), col = "darkblue", lwd = 2)

hist(missed_riders_pois, main = "", xlab = "Poisson Missed Riders Per Simulation", prob = T, col = "darkred")
lines(density(missed_riders_pois), col = "darkblue", lwd = 2)

hist(missed_riders_nbin, main = "", xlab = "Negative Binomial Missed Riders Per Simulation", prob = T, col = "darkred")
lines(density(missed_riders_nbin), col = "darkblue", lwd = 2)

hist(missed_riders_geom, main = "", xlab = "Geometric Missed Riders Per Simulation", prob = T, col = "darkred")
lines(density(missed_riders_geom), col = "darkblue", lwd = 2)

# "Gamma Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
#0.7738467
# "Total Riders Deviation:"
# 1164
# "Est. Model Bias:"
# -1.164
# "Estimated Total Missed Riders in 1000 Simulations of N"
# -1164

#  "Poisson Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
# 0.7702441
# "Total Riders Deviation:"
# -7835
# "Est. Model Bias:"
# -1.015
# "Estimated Total Missed Riders in 1000 Simulations of N"
# -1015

# "Negative Binomial Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
# 0.7452268
# "Total Riders Deviation:"
# -43385
# "Est. Model Bias:"
# 34.535
#"Estimated Total Missed Riders in 1000 Simulations of N"
#34535

# "Geometric Distribution Accuracy in 1000 Simulations:"
# "Accurate Category Prediction:"
# 0.6846381
# "Total Riders Deviation:"
# -9101
# "Est. Model Bias:"
# 0.251
# "Estimated Total Missed Riders in 1000 Simulations of N"
# 251
