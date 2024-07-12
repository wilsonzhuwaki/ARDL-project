# Load the packages
# Core libraries
library(tidyverse)
library(vars)
library(timetk)
library(tidyquant)
library(lubridate) #convert the date format
library(ggplot2)
# Visualization
library(plotly)
library(ggthemes)
library(forecast)
library(TTR)        # Time series
library(tseries)    # Time series
library(tidyverse)
library(pastecs)
library(strucchange)
library(states)
library(ggplot2)
library(readxl)
library(ggpubr)
library(tseries)
library(ARDL)
library(tsbox)
library(imputeTS)
library(astsa)
library(dplyr)
library(car)
library(lmtest)
library(modelr)
suppressWarnings(library(RODBC))
data<- read_csv("DATA_2V1.csv")
data_ts <- ts(data, start = c(1960, 1), end = c(2022, 1), frequency = 1)
head(data)
spec(data)
###############################################################################
summary(data)
hist(data$GDPG,main = "Histogram of GDP ",freq=FALSE,col="blue")
hist(data$INF,main = "Histogram of inflation",freq=FALSE,col="blue")
hist(data$EXP,main = "Histogram of exports ",freq=FALSE,col="yellow")
hist(data$FDI,main = "Histogram of FDI ",freq=FALSE,col="blue")
hist(data$UNE,main = "Histogram of unemployment",freq=FALSE,col="red")
#############################################
# Assuming 'data' is your data frame and you want to convert all columns to numeric
ggqqplot(data$GDPG)
ggqqplot(data$INF)
ggqqplot(data$EXP)
ggqqplot(data$FDI)
ggqqplot(data$UNE)

shapiro.test(data$GDPG)
shapiro.test(data$INF)
shapiro.test(data$EXP)
shapiro.test(data$FDI)
shapiro.test(data$UNF)
###############################################
#####################################################
ts_plot(ts_xts(data_ts[,"INF"]),title = 'inflation line chart')
plot(data_ts[,'INF'],main='inflation as % of gdp')
ts_plot(data_ts[,'GDPG'],data_ts[,'INF'],title='GDP growth vs inflation %')
ts_plot(ts_trend(data_ts[,'GDPG']),ts_trend(data_ts[,'INF']),title = 'GDP growth vs inflation')
##################################################################

ts_plot(ts_xts(data_ts[,"UNE"]),title = 'Unemployment line chart')
plot(data_ts[,'UNE'],main='unemployment as a % of gdp')
ts_plot(data_ts[,'GDPG'],data_ts[,'UNE'],title='GDP growth vs unemployment %')
ts_plot(ts_trend(data_ts[,'GDPG']),ts_trend(data_ts[,'UNE']),title = 'GDP growth vs unemployment')
ts_plot(ts_scale(data_ts[,'GDPG']),ts_scale(data_ts[,'UNE']),title = 'GDP growth vs Unemployment Normalised')
##############################################################
ts_plot(ts_xts(data_ts[,"FDI"]),title = 'FDI line chart')
plot(data_ts[,'FDI'],main='FDI % of gdp')
ts_plot(data_ts[,'GDPG'],data_ts[,'FDI'],title='GDP growth vs FDI %')
ts_plot(ts_scale(data_ts[,'GDPG']),ts_scale(data_ts[,'FDI']),title = 'GDP growth vs FDI Normalised')

###################################################################
ts_plot(ts_xts(data_ts[,"EXP"]),title = 'exports line chart')
plot(data_ts[,'EXP'],main='exports % of gdp')
ts_plot(data_ts[,'GDPG'],data_ts[,'EXP'],title='GDP growth vs export %')
ts_plot(ts_trend(data_ts['GDPG']),ts_trend(data_ts[,'EXP']),title = 'GDP growth vs Exports')
ts_plot(ts_scale(data_ts[,'GDPG']),ts_scale(data_ts[,'EXP']),title = 'GDP growth vs Exports Normalised')
####################################################################
#Dickey-Fuller test 
summary(data)
adf.test(data$INF, alternative=c('stationary'))
adf.test(data$UNE, alternative=c('stationary'))

###
adf.test(data$GDPG, alternative=c('stationary'))
adf.test(data$UNE, alternative=c('stationary'))
adf.test(data$EXP, alternative=c('stationary'))
adf.test(data$FDI, alternative=c('stationary'))
adf.test(data$FDI, alternative=c('stationary'))
##########
#checking for number of diff
#ndiffs(data$GDPG)
###GDPG

GDPG <- diff(data$GDPG, differences=1)
plot.ts(GDPG, col = "red")


### EXPGDP

EXP <- diff(data$EXP, differences=1)
plot.ts(EXP, col = "red")


adf.test(EXP, alternative=c('stationary'))

EXP
### FDIGDP

EXP<- diff(data$EXP, differences=1)
plot.ts(FDI, col = "red")
adf.test(FDI, alternative=c('stationary'))
FDI
FDIGDP<- diff(data$FDI, differences=1)
plot.ts(INF, col = "red")
adf.test(FDI, alternative=c('stationary'))
summary(data)
###############################################################
ardl<-ardl(GDPG~UNE+INF+EXP+FDI,data=data,order=c(1,2,2,2,2))
summary(ardl)
#choosing the best model lags through BIC
model1<-auto_ardl(GDPG~UNE+INF+EXP+FDI,data=data,max_order = c(1,2,2,2,2),selection = 'BIC')
summary(model1)
model1$top_orders
######################################################
#best model
ardl2<-ardl(GDPG~UNE+INF+EXP+FDI,data=data,order=c(1,1,1,1,1))
summary(ardl2)
#Contigration Test
bounds_t_test(ardl2,case=3)
bounds_f_test(ardl2,case=3)
ce2_ardl<-coint_eq(ardl2,case=3)
ce2_ardl<-uecm(ardl2)
summary(ce2_ardl)
uecm<-uecm(ardl2)
summary(uecm)
#UNRESTRICTED MODEL AND RESTRICTED MODEL
ce2_uecm<-coint_eq(uecm,case=3)
summary(ce2_uecm)
ce2_ardl<-uecm(ardl2)
recm<-recm(uecm,case = 3)
summary(recm)
ce2_recm<-coint_eq(recm)
identical(ce2_ardl,ce2_uecm,ce2_recm)

#####################################################
#long run results 
mult_ardl2<-multipliers(ardl2)
mult_ardl2
mult_uecm<-multipliers(uecm)
mult_uecm
plot(ardl2$residuals ~ fitted(ardl2), 
     xlab = "Fitted Values", 
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
qqnorm(ardl2$residuals, 
       main = "Normal Q-Q Plot of Residuals")
qqline(ardl2$residuals)
acf(ardl2$residuals, 
    main = "Autocorrelation Function of Residuals")
pacf(ardl2$residuals, 
     main = "Partial Autocorrelation Function of Residuals")
####
# Assuming cointegrating vectors are stored in ce2_ardl$cointegratingvectors
plot(data$GDPG, type = "l", xlab = "Time", ylab = "GDPG", main = "Cointegration Graph")
lines(ce2_ardl$cointegratingvectors[, 1], col = "red") # Adjust according to your data and model
#############
#significance test 
bgtest(uecm)
bgtest(uecm,order=2)
bptest(uecm)
resettest(uecm)
resettest(uecm,type = ('fitted'))
resettest(uecm,type = ('regressor'))
resettest(uecm,power = 2)

#normality check
jarque.bera.test(residuals(uecm))
jarque.bera.test(residuals(recm))
uecm_names<-to_lm(uecm,fix_names = TRUE)
flactuation<-efp(uecm_names$full_formula,data = uecm_names$model)
sctest(flactuation)
plot(flactuation)

bgtest(ardl2)
bgtest(ardl2,order=2)
bptest(ardl2)
resettest(ardl2)
resettest(ardl2,type = ('fitted'))
resettest(ardl2,type = ('regressor'))
resettest(ardl2,power = 2)

#normality check
jarque.bera.test(residuals(ardl2))
jarque.bera.test(residuals(ardl2))


ardl2_names<-to_lm(ardl2,fix_names = TRUE)
flactuation1<-efp(ardl2_names$full_formula,data = ardl2_names$model)
sctest(flactuation1)
plot(flactuation1)



