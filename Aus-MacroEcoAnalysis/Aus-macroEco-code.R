#############################################################################################################################
#                          TIME SERIES PROJECT - PART 1
#***************************************************************************************************************************#
# Program = QM2_albertini_2021_01_05.r
# Programmer = Luca Albertini
# Date first logger: 2020 12 12
#
#            Description: This program is an analysis of time series data. Especially, it 
#                         deals with variables such as NGDP_R, NGDRP_D, PCPI and LUR
#                         in Austria from 1980 to 2020. The variables will be analyzed one at
#                         time. First of all, a representation of variable's log and growth rate
#                         will be made. Moreover, for every variable will be computed a correlogram
#                         of the logged level and the growth rate. Lastly, for the one at time
#                         analysis a augmented Dickey_fuller test will be run to check stationarity
#                         in each timeseries. 
#                         In conclusion, an estimation a Philip curve where the first differences
#                         of the logged inflation are explained by their past history and by the past
#                         levels of the unemployment rate.
#
#            Input files: /in_data/WEOApr2019all.csv
#            Temp files: none
#            Output files: ~/Desktop/albertini/out_data
#
#**************************************************************************************************************************#


#install packages you might need (if not done yet)
#install.packages("systemfit")
#install.packages("moments")
#install.packages("MASS")
#install.packages("sandwich")
#install.packages("gridExtra")
#install.packages("fBasics")
#install.packages("ivpack")
#install.packages("plm")
#install.packages ("olsrr")
#install.packages ("readxl)
#install.packages ("reshape")
#install.packages ("Hmisc")
#install.packages("expss")
#install.packages("WDI")
#install.packages("reshape2")
#install.packages("normtest")
#install.packages("tseries")
#install.packages("pastecs")
#install.packages("psych")
#install.packages("moments")
#install.packages("estimatr")
#install.packages("Jmisc")
#install.packages("rccdates")
#install.packages("xts")
#install.packages("tis")
#install.packages("sos")
#install.packages("gridExtra")
#install.packages("corrgram")
#install.packages("dynlm")

#set the working directory
#Luca's Macbook
setwd("~/Personal/CaseStudies/Aus-MacroEcoAnalysis")
# set working directory
# setwd("")

#load needed packages
library("expss")
library("WDI")
library("reshape2")
require("normtest")
library("readxl")
library("reshape")
library("Hmisc")
library("tseries")
library("pastecs")
library("psych")
library("moments")
library("AER") 
library("systemfit")
library("moments") # kurtosis and skewness calculations
library("stargazer") # table format for regression results
library("MASS")
library("lmtest") # required for coefci and coeftest
library("sandwich") # required for heteroskedastic robust standard errors calculation
library("plm")
library("gridExtra") # arrange graphs
library("olsrr") # Breusch-Pagan Test
library("fBasics") # dagoTest (approximates sktest)
#library("ivpack")
library("ggplot2")
library("haven") 
library("dplyr")
library("foreign")
library("broom")
library("estimatr")
library("Jmisc")
#library("rccdates")
library("xts")
library("tis")
library("sos")
library("gridExtra")
library("corrgram")
library("urca")
library("dynlm")
library("dplyr")


###### this command might be useful to find to what package a function belongs
# findFn(function name)
#############################

# clear workspace
rm(list = ls())

###################################### USEFUL FUNCTIONS ############################################################


###### QUANTS
quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "first log difference" = log(s) - lag(log(s)),
               "AnnualGrowthRate" = 100 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(100 * log(s / lag(s))))
  )
}
# this command gives:
# the Xs of the series (the level--> in this case the GDP)
# the logarithm
# the annual growth rate
# the annual log rate lag

###### BIC
# Computation of the BIC
BIC_comp <- function(model) {
  ssr <- sum(model$residuals^2)#sum of squared residual
  t <- length(model$residuals)#length of model residuals
  npar <- length(model$coef)#length of coefficent
  
  return(
    round(c("p" = npar - 1,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "R2" = summary(model)$r.squared), 4)
  )
}


###################################### DATA PREPARATION #########################################################

# load data from the WEO-IMF database (an .xlsl file)
weo = read.csv("in_data/WEOApr2019all.csv", header =  TRUE, sep = ",")
# have a quick look to the data
weo %>% glimpse()
# 8732 observations of 55 variables
# rename the variables (without using any library)
names(weo)[names(weo) == "ISO"] <- "ccISO3"
names(weo)[names(weo) == "WEO.Subject.Code"] <- "var"
names(weo)[names(weo) == "Subject.Descriptor"] <- "label"
# keep the observation within the time-range 1980-2020
weo <-  weo %>% 
  select(ccISO3, var, X1980:X2020)
# clean some empty record
weo <- weo[-c(8731)]
weo <- weo[-c(8732)]
# Reshape using "melt" and remove missing values (NA)
weo1 <- weo %>% 
  melt(id=(c("var", "ccISO3"))) %>% 
  na.omit() 
# renaming the "time" var as "year"
names(weo1)[names(weo1) == "variable"] <- "year"
# transform "year" into an int and removing the X at the beggining
weo1$year <- as.integer(substr(weo1$year, start = 2, stop = 5))
# set missing values as "NA", as required by R
weo1$value[weo1$value == "n/a"] <- NA
weo1$value[weo1$value == ""] <- NA
# drop observations where "ISO" is empty
weo1 <- weo1 %>% 
  dplyr::filter(ccISO3 != "")
# Reshape again in a linear way
weo2 <- cast(weo1, ccISO3+year~var)
# select variables: ccISO3, year, NGDP_R, NGDP_D, LUR, PCPI)
weo3 <- select(weo2, ccISO3, year, NGDP_R, NGDP_D, LUR, PCPI)
# Keeping only observation regarding AUSTRIA
AUT <- weo3 %>% 
  dplyr::filter(ccISO3 == "AUT")
# view the structure of the df
str(AUT)
# change numbers into "number" format (not factor)
AUT$NGDP_R <- as.numeric(as.character(AUT$NGDP_R))
AUT$NGDP_D <- as.numeric(as.character(AUT$NGDP_D))
AUT$LUR <- as.numeric(as.character(AUT$LUR))
AUT$PCPI <- as.numeric(as.character(AUT$PCPI))
# add labels
AUT = apply_labels (AUT,
                    NGDP_R = "GDP constant prices",
                    NGDP_D = "GDP deflator",
                    LUR = "Unemployment rate",
                    PCPI = "Inflation, average consumer prices")
# save data set in the out_data folder
write.csv(AUT, file= "out_data/AUT.csv")

######################################## ANALYSIS #############################################################

#Variables considered: GDP, Inflation, Unemployment Rate.
# represent the variables, their log and and their growth rate
library(scales)
######################### NGDP_R
# build separate dataframe for NGDP_R to analyze it
AUT_NGDPR <- select (AUT, year, NGDP_R)
# represent the NGDP_R
NGDP_R1 <- ts(AUT_NGDPR$NGDP_R, start = c(1980), end = c(2020), frequency = 1)
NGDPR_plot <- NGDP_R1 %>% 
  plot.ts(col = "red", main ="Austria NGDP_R", 
          xlab = "Years", ylab = "NGDP_R", axes = TRUE)
NGDPR_plot
# save it
pdf("out_data/NGDPR_plot.pdf")
NGDPR_plot <- plot.ts(NGDP_R1, col ="red", main ="Austria NGDP_R", 
                      xlab = "Years", ylab = "NGDP_R", axes = TRUE)
NGDPR_plot
dev.off()
# compute log  of NGDP_R
logNGDP_R <- log(AUT$NGDP_R)
# put the results in the dataframe
AUT_NGDPR$logNGDP_R <- logNGDP_R
# transform it into numeric value
# AUT_NGDPR$logNGDP_R <- as.numeric(as.character(AUT_NGDPR$logNGDP_R))
# put label
AUT_NGDPR = apply_labels (AUT_NGDPR,
                          logNGDP_R = "log of GDP, constant prices")
# check the structure of the dataframe 
str(AUT_NGDPR)
# transform the log in a timeseries
logNGDP_R <- ts(AUT_NGDPR$logNGDP_R, start = c(1980), end = c(2020), frequency = 1)
# save the dataframe
write.csv(AUT_NGDPR, file= "out_data/AUT_NGDPR.csv")
# Represent the log of NGDP_R
logNGDPR_plot <- log(AUT$NGDP_R) %>% 
  plot.ts(col ="red", main ="Austria NGDP_R log", 
          xlab = "Years", ylab = "logNGDP_R", axes = TRUE)
logNGDPR_plot
# save it
pdf("out_data/logNGDPR_plot.pdf")
logNGDPR_plot <- log(AUT$NGDP_R) %>% 
  plot.ts(col ="red", main ="Austria NGDP_R log", 
          xlab = "Years", ylab = "logNGDP_R", axes = TRUE)
logNGDPR_plot
dev.off()
# Transform the data set in time series
NGDP_Rts <- ts(AUT_NGDPR$NGDP_R, start = c(1980), end = c(2020), frequency = 1)
# calculate NGDP_R growth rate
NGDP_Rgr <- growth.rate(NGDP_Rts, lag =1, simple = T)
# plot the NGDP_R growth rate
NGDPR_gr_plot <- plot.ts(NGDP_Rgr, col ="red", main ="NGDP_R growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPR_gr_plot
# save it
pdf("out_data/NGDPR_growthrate_plot.pdf")
NGDPR_gr_plot <- plot.ts(NGDP_Rgr, col ="red", main ="NGDP_R growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPR_gr_plot
dev.off()

######################### NGDP_D

# build separate dataframe for NGDP_R to analyze it
AUT_NGDPD <- select (AUT, year, NGDP_D)
# represent the NGDP_D
NGDP_D1 <- ts(AUT_NGDPD$NGDP_D, start = c(1980), end = c(2020), frequency = 1)
NGDPD_plot <- plot.ts(NGDP_D1, col ="blue", main ="Austria NGDP_D", 
                      xlab = "Years", ylab = "NGDP_D", axes = TRUE)
NGDPD_plot
# save it
pdf("out_data/NGDPD_plot.pdf")
NGDPD_plot <- plot.ts(NGDP_D1, col ="blue", main ="Austria NGDP_D", 
                      xlab = "Years", ylab = "NGDP_D", axes = TRUE)
NGDPD_plot
dev.off()
# compute log  of NGDP_R
logNGDP_D <- log(AUT$NGDP_D)
# put the results in the dataframe
AUT_NGDPD$logNGDP_D <- logNGDP_D
# transform it into a time series
logNGDP_D <- ts(AUT_NGDPD$logNGDP_D, start = c(1980), end = c(2020), frequency = 1)
# put label
AUT_NGDPD = apply_labels (AUT_NGDPD,
                          logNGDP_D = "log of GDP, deflator")
# check the structure of the dataframe 
str(AUT_NGDPD)
# save the dataframe
write.csv(AUT_NGDPD, file= "out_data/AUT_NGDPD.csv")
# Represent the log of NGDP_R
# Represent the log of LUR
logNGDPD_plot <- plot.ts(logNGDP_D, col ="blue", main ="Austria NGDP_D log", 
                         xlab = "Years", ylab = "logNGDP_D", axes = TRUE)
logNGDPD_plot
# save it
pdf("out_data/logNGDPD_plot.pdf")
logNGDPD_plot <- plot.ts(logNGDP_D, col ="blue", main ="Austria NGDP_D log", 
                         xlab = "Years", ylab = "logNGDP_D", axes = TRUE)
logNGDPD_plot
dev.off()
# Transform the data set in time series
NGDP_Dts <- ts(AUT_NGDPD$NGDP_D, start = c(1980), end = c(2020), frequency = 1)
# calculate NGDP_R growth rate
NGDP_Dgr <- growth.rate(NGDP_Dts, lag =1, simple = T)
# plot the NGDP_R growth rate
NGDPD_gr_plot <- plot.ts(NGDP_Dgr, col ="blue", main ="NGDP_D growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPD_gr_plot
# save it
pdf("out_data/NGDPD_growthrate_plot.pdf")
NGDPD_gr_plot <- plot.ts(NGDP_Dgr, col ="blue", main ="NGDP_D growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPD_gr_plot
dev.off()


######################### PCIP

# build separate dataframe for NGDP_R to analyze it
AUT_PCPI <- select (AUT, year, PCPI)
# represent the PCPI
PCPI1 <- ts(AUT_PCPI$PCPI, start = c(1980), end = c(2020), frequency = 1)
PCPI_plot <- plot.ts(PCPI1, col ="orange", main ="Austria PCPI", 
                     xlab = "Years", ylab = "PCPI", axes = TRUE)
PCPI_plot
# save it
pdf("out_data/PCPI_plot.pdf")
PCPI_plot <- plot.ts(PCPI1, col ="orange", main ="Austria PCPI", 
                     xlab = "Years", ylab = "PCPI", axes = TRUE)
PCPI_plot
dev.off()
# compute log  of NGDP_R
logPCPI <- log(AUT$PCPI)
# put the results in the dataframe
AUT_PCPI$logPCPI <- logPCPI
# transform it into a time series
logPCPI <- ts(AUT_PCPI$logPCPI, start = c(1980), end = c(2020), frequency = 1)
# put label
AUT_PCPI = apply_labels (AUT_PCPI,
                         logPCPI = "log of Inflation, average consumer prices")
# check the structure of the dataframe 
str(AUT_PCPI)
# save the dataframe
write.csv(AUT_PCPI, file= "out_data/AUT_PCPI.csv")
# Represent the log of NGDP_R
logPCPI_plot <- plot.ts(logPCPI, col ="orange", main ="Austria PCPI log", 
                        xlab = "Years", ylab = "logPCPI", axes = TRUE)
logPCPI_plot
# save it
pdf("out_data/logPCPI_plot.pdf")
logPCPI_plot <- plot.ts(logPCPI, col ="orange", main ="Austria PCPI log", 
                        xlab = "Years", ylab = "logPCPI", axes = TRUE)
logPCPI_plot
dev.off()
# Transform the data set in time series
PCPIts <- ts(AUT_PCPI$PCPI, start = c(1980), end = c(2020), frequency = 1)
# calculate NGDP_R growth rate
PCPIgr <- growth.rate(PCPIts, lag =1, simple = T)
# plot the NGDP_R growth rate
PCPI_gr_plot <- plot.ts(PCPIgr, col ="orange", main ="PCPI growth rate", 
                        xlab = "Years", ylab = "Growth precentage", axes = TRUE)
PCPI_gr_plot
# save it
pdf("out_data/PCPI_growthrate_plot.pdf")
PCPI_gr_plot <- plot.ts(PCPIgr, col ="orange", main ="PCPI growth rate", 
                        xlab = "Years", ylab = "Growth precentage", axes = TRUE)
PCPI_gr_plot
dev.off()


######################### LUR

# build separate dataframe for LUR to analyze it
AUT_LUR <- select (AUT, year, LUR)
# represent the LUR
LUR1 <- ts(AUT_LUR$LUR, start = c(1980), end = c(2020), frequency = 1)
LUR_plot <- plot.ts(LUR1, col ="darkgreen", main ="Austria LUR", 
                    xlab = "Years", ylab = "LUR", axes = TRUE)
LUR_plot
# save it
pdf("out_data/LUR_plot.pdf")
LUR_plot <- plot.ts(LUR1, col ="darkgreen", main ="Austria LUR", 
                    xlab = "Years", ylab = "LUR", axes = TRUE)
LUR_plot
dev.off()
# compute log  of NGDP_R
logLUR <- log(AUT$LUR)
# put the results in the dataframe
AUT_LUR$logLUR <- logLUR
# transform it into a time series
logLUR <- ts(AUT_LUR$logLUR, start = c(1980), end = c(2020), frequency = 1)
# put label
AUT_LUR = apply_labels (AUT_LUR,
                        logLUR = "log of Unemployment rate")
# check the structure of the dataframe 
str(AUT_LUR)
# save the dataframe
write.csv(AUT_LUR, file= "out_data/AUT_LUR.csv")
# Represent the log of LUR
logLUR_plot <- plot.ts(logLUR, col ="darkgreen", main ="Austria LUR log", 
                       xlab = "Years", ylab = "logLUR", axes = TRUE)
logLUR_plot
# save it
pdf("out_data/logLUR_plot.pdf")
logLUR_plot <- plot.ts(logLUR, col ="darkgreen", main ="Austria LUR log", 
                       xlab = "Years", ylab = "logLUR", axes = TRUE)
logLUR_plot
dev.off()
# Transform the data set in time series
LURts <- ts(AUT_LUR$LUR, start = c(1980), end = c(2020), frequency = 1)
# calculate NGDP_R growth rate
LURgr <- growth.rate(LURts, lag =1, simple = T)
# plot the NGDP_R growth rate
LUR_gr_plot <- plot.ts(LURgr, col ="darkgreen", main ="LUR growth rate", 
                       xlab = "Years", ylab = "Growth precentage", axes = TRUE)
LUR_gr_plot
# save it
pdf("out_data/LUR_growthrate_plot.pdf")
LUR_gr_plot <- plot.ts(LURgr, col ="darkgreen", main ="LUR growth rate", 
                       xlab = "Years", ylab = "Growth precentage", axes = TRUE)
LUR_gr_plot
dev.off()


######################### Show all charts together
# log charts (for all the variables)
par(mfrow = c(2, 2))
logNGDPR_plot <- plot.ts(logNGDP_R, col ="red", main ="Austria NGDP_R log", 
                         xlab = "Years", ylab = "logNGDP_R", axes = TRUE)
logNGDPR_plot
logNGDPD_plot <- plot.ts(logNGDP_D, col ="blue", main ="Austria NGDP_D log", 
                         xlab = "Years", ylab = "logNGDP_D", axes = TRUE)
logNGDPD_plot
logPCPI_plot <- plot.ts(logPCPI, col ="orange", main ="Austria PCPI log", 
                        xlab = "Years", ylab = "logPCPI", axes = TRUE)
logPCPI_plot
logLUR_plot <- plot.ts(logLUR, col ="darkgreen", main ="Austria LUR log", 
                       xlab = "Years", ylab = "logLUR", axes = TRUE)
logLUR_plot
# save it
pdf("out_data/AUT_log_plots.pdf")
par(mfrow = c(2, 2))
logNGDPR_plot <- plot.ts(logNGDP_R, col ="red", main ="Austria NGDP_R log", 
                         xlab = "Years", ylab = "logNGDP_R", axes = TRUE)
logNGDPR_plot
logNGDPD_plot <- plot.ts(logNGDP_D, col ="blue", main ="Austria NGDP_D log", 
                         xlab = "Years", ylab = "logNGDP_D", axes = TRUE)
logNGDPD_plot
logPCPI_plot <- plot.ts(logPCPI, col ="orange", main ="Austria PCPI log", 
                        xlab = "Years", ylab = "logPCPI", axes = TRUE)
logPCPI_plot
logLUR_plot <- plot.ts(logLUR, col ="darkgreen", main ="Austria LUR log", 
                       xlab = "Years", ylab = "logLUR", axes = TRUE)
logLUR_plot
dev.off()

# growth rates charts (for all the variables), with a different command
par(mfrow = c(2, 2))
NGDPR_gr_plot <- plot.ts(NGDP_Rgr, col ="red", main ="NGDP_R growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPR_gr_plot
NGDPD_gr_plot <- plot.ts(NGDP_Dgr, col ="blue", main ="NGDP_D growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPD_gr_plot
PCPI_gr_plot <- plot.ts(PCPIgr, col ="orange", main ="PCPI growth rate", 
                        xlab = "Years", ylab = "Growth precentage", axes = TRUE)
PCPI_gr_plot
LUR_gr_plot <- plot.ts(LURgr, col ="darkgreen", main ="LUR growth rate", 
                       xlab = "Years", ylab = "Growth precentage", axes = TRUE)
LUR_gr_plot
# save it
pdf("out_data/AUT_gr_plots.pdf")
par(mfrow = c(2, 2))
NGDPR_gr_plot <- plot.ts(NGDP_Rgr, col ="red", main ="NGDP_R growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPR_gr_plot
NGDPD_gr_plot <- plot.ts(NGDP_Dgr, col ="blue", main ="NGDP_D growth rate", 
                         xlab = "Years", ylab = "Growth precentage", axes = TRUE)
NGDPD_gr_plot
PCPI_gr_plot <- plot.ts(PCPIgr, col ="orange", main ="PCPI growth rate", 
                        xlab = "Years", ylab = "Growth precentage", axes = TRUE)
PCPI_gr_plot
LUR_gr_plot <- plot.ts(LURgr, col ="darkgreen", main ="LUR growth rate", 
                       xlab = "Years", ylab = "Growth precentage", axes = TRUE)
LUR_gr_plot
dev.off()

# CORRELOGRAM
#corr of both logged levels and growth rate for all var
# correlogram = visual way to show serial correlation in data that changes over times (i.e timeseries)
# set the plot space in a way to contain 2 charts
par(mfrow = c(2, 1))
######################### NGDP_R and NGDP_R growth rate
# compute a correlation for NGDP_R and NGDP_R growth rate and plot it
acf(na.omit(logNGDP_R), lag.max = 40, plot = F)
acf(logNGDP_R, lag.max = 40, main = "levels of logNGDP_R autocorrelation")
acf(na.omit(NGDP_Rgr), lag.max = 40, plot = F)
acf(na.omit(NGDP_Rgr), lag.max = 40, main ="Growth rate NGDP_R autocorrelation")
# save it
pdf("out_data/corrgram_NGDPR.pdf")
par(mfrow = c(2, 1))
acf(na.omit(logNGDP_R), lag.max = 40, plot = F)
acf(logNGDP_R, lag.max = 40, main = "levels of logNGDP_R autocorrelation")
acf(na.omit(NGDP_Rgr), lag.max = 40, plot = F)
acf(na.omit(NGDP_Rgr), lag.max = 40, main ="Growth rate NGDP_R autocorrelation")
dev.off()


######################### NGDP_D and NGDP_D growth rate
# compute a correlation for NGDP_D and NGDP_D growth rate and plot it
acf(na.omit(logNGDP_D), lag.max = 40, plot = F)
acf(logNGDP_D, lag.max = 40, main = "levels of logNGDP_D autocorrelation")
acf(na.omit(NGDP_Dgr), lag.max = 40, plot = F)
acf(na.omit(NGDP_Dgr), lag.max = 40, main ="Growth rate NGDP_D autocorrelation")
# save it
pdf("out_data/corrgram_NGDPD.pdf")
par(mfrow = c(2, 1))
acf(na.omit(logNGDP_D), lag.max = 40, plot = F)
acf(logNGDP_D, lag.max = 40, main = "levels of logNGDP_D autocorrelation")
acf(na.omit(NGDP_Dgr), lag.max = 40, plot = F)
acf(na.omit(NGDP_Dgr), lag.max = 40, main ="Growth rate NGDP_D autocorrelation")
dev.off()


######################### PCPI and PCPI growth rate
# compute a correlation for PCPI and PCPI growth rate and plot it
acf(na.omit(logPCPI), lag.max = 40, plot = F)
acf(logPCPI, lag.max = 40, main ="levels of logPCPI autocorrelation")
acf(na.omit(PCPIgr), lag.max = 40, plot = F)
acf(na.omit(PCPIgr), lag.max = 40, main ="Growth rate PCPI autocorrelation")
# save it
pdf("out_data/corrgram_PCPI.pdf")
par(mfrow = c(2, 1))
acf(na.omit(logPCPI), lag.max = 40, plot = F)
acf(logPCPI, lag.max = 40, main ="levels of logPCPI autocorrelation")
acf(na.omit(PCPIgr), lag.max = 40, plot = F)
acf(na.omit(PCPIgr), lag.max = 40, main ="Growth rate PCPI autocorrelation")
dev.off()


######################### LUR and LUR growth rate
# compute a correlation for LUR and LUR growth rate and plot it
acf(na.omit(logLUR), lag.max = 40, plot = F)
acf(logLUR, lag.max = 40, main = "levels of logLUR autocorrelation")
acf(na.omit(LURgr), lag.max = 40, plot = F)
acf(na.omit(LURgr), lag.max = 40, main ="Growth rate LUR autocorrelation")
# save it
pdf("out_data/corrgram_LUR.pdf")
par(mfrow = c(2, 1))
acf(na.omit(logLUR), lag.max = 40, plot = F)
acf(logLUR, lag.max = 40, main = "levels of logLUR autocorrelation")
acf(na.omit(LURgr), lag.max = 40, plot = F)
acf(na.omit(LURgr), lag.max = 40, main ="Growth rate LUR autocorrelation")
dev.off()


####################################### STATIONARITY TEST  ###########################################################
#Stationarity will be tested trhough the augmented Dickey-Fueller test (ADF) both on vars and their logs
#ADF H0: there is an unit root --> nonstationarity
######################### NGDP_R 
# Dickey-Fuller test on the logged levels
summary(ur.df(logNGDP_R, 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# -0.9299
# Dickey-Fuller test on the first differences of the log
# compute the first differences of the NGDP_R
diffNGDPR <- diff(logNGDP_R, lag = 1)
# H0: there is a unit root in the first differences --> nonstationarity
summary(ur.df(diff(logNGDP_R, lag = 1),
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# - 3.5945
######################### NGDP_D 
# Dickey-Fuller test on the logged levels
summary(ur.df(logNGDP_D, 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# -2.3104
# Dickey-Fuller test on the first differences of the log
# compute the first differences of the NGDP_D
diffNGDPD <- diff(logNGDP_D, lag = 1)
# H0: there is a unit root in the first differences --> nonstationarity
summary(ur.df(diff(logNGDP_D, lag = 1), 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# -2.0506
######################### PCPI
# Dickey-Fuller test on the logged levels
summary(ur.df(logPCPI, 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# -3.024
# Dickey-Fuller test on the first differences of the log
# compute the first differences of the PCPI
diffPCPI <- diff(logPCPI, lag = 1)
# H0: there is a unit root in the first differences --> nonstationarity
summary(ur.df(diff(logPCPI, lag = 1), 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# -2.6781
######################### LUR
# Dickey-Fuller test on the logged levels
summary(ur.df(logLUR, 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# -3.4578
# Dickey-Fuller test on the first differences of the log
# compute the first differences of the LUR
diffLUR <- diff(logLUR, lag = 1)
# H0: there is a unit root in the first differences --> nonstationarity
summary(ur.df(diff(logLUR, lag = 1), 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
# -4.2869


####################################### INFLATION ANALYSIS  ###########################################################
# compare different measures of inflation (NGDP_D and PCPI)
# transform the growth rate of NGDP_D in time series
NGDP_Dgr <- ts(NGDP_Dgr, start = c(1980), end = c(2019), frequency = 1)
# transform the growth rate of PCPI in time series
PCPIgr <- ts(PCPIgr, start = c(1980), end = c(2019), frequency = 1)
# transform the first differences of NGDP_D in time series
diffNGDPD <- ts(diffNGDPD, start = c(1980), end = c(2019), frequency = 1)
# transform the differences of PCPI in time series
diffPCPI <- ts(diffPCPI, start = c(1980), end = c(2019), frequency = 1)
# plot the growth time series together 
par(mfrow = c(1, 1))
ts.plot(NGDP_Dgr, PCPIgr, 
        gpars=list(xlab="year", ylab="growth rate", col = c("blue", "orange")))
# save it
pdf("out_data/inflation_gr.pdf")
par(mfrow = c(1, 1))
ts.plot(NGDP_Dgr, PCPIgr, 
        gpars=list(xlab="year", ylab="growth rate", col = c("blue", "orange")))
dev.off()
# plot the first differences of NGDP_D and PCPI together
ts.plot(diffNGDPD, diffPCPI, 
        gpars=list(xlab="year", ylab="first differences", col = c("blue", "orange")))
# save it
pdf("out_data/inflation_firstdiff.pdf")
ts.plot(diffNGDPD, diffPCPI, 
        gpars=list(xlab="year", ylab="first differences", col = c("blue", "orange")))
dev.off()


#######################################  PHILIPS CURVE ESTIMATION  ###########################################################
# Estimate the Philips curve where first differences of logged inflation are explained
# by their past history and by the past level of the unemployment rate
# diffPCPI = f(diffPCPI, logLUR)
######################### Preparing the dataframe to estimate model
# compute the first differences of the PCPI
# logPCPI <- ts(AUT_PCPI$logPCPI, start = c(1980), end = c(2020), frequency = 1)
PCPI <- quants(AUT_PCPI$PCPI)
# transform it into a timeseries
fdiffPCPI <- ts(PCPI$first.log.difference, start = c(1980), end = c(2020), frequency = 1)
# put the fdiffPCPI and the logLUR (both ts.object) in the same dataframe
Philipdata <- ts.union(fdiffPCPI, LURts)
# save it
write.csv(Philipdata, file= "out_data/Philipdata.csv")

######################### Various model to estimate the Philips curve
### Philips curve with one lag for both the regressors (Philips(1,1))
Phil_curv1 <- dynlm(fdiffPCPI ~ L(fdiffPCPI) + L(LURts),
                    start = c(1982), end = c(2020))
# show the coefficient, standard error and the singular regressor relevance
coeftest(Phil_curv1, vcov. = sandwich)
# F-test for the joint relevance of the regressors
# H0: regressors are irrelevant in explaining fdiffPCPI
linearHypothesis(Phil_curv1, 
                 c("L(fdiffPCPI)=0", 
                   "L(LURts)=0"),
                 vcov. = sandwich)
# p-value: 8.819e-06
### Philips curve with 2 lags for the fdiffPCPI and 1 lag for the logLUR (Philips(2,1))
Phil_curv2 <- dynlm(fdiffPCPI ~ L(fdiffPCPI) + L(fdiffPCPI, 2) 
                    + L(LURts),
                    start = c(1980), end = c(2020))
# show the coefficient, standard error and the singular regressor relevance
coeftest(Phil_curv2, vcov. = sandwich)
# F-test for the joint relevance of the regressors
# H0: regressor are irrelevant in explaining fdiffPCPI
linearHypothesis(Phil_curv2, 
                 c("L(fdiffPCPI)=0", "L(fdiffPCPI, 2)=0", 
                   "L(LURts)=0"),
                 vcov. = sandwich)
# p-value: 0.0001967

# Philips curve with 2 lags for the fdiffPCPI and 2 lags for the logLUR (Philips(2,2))
Phil_curv3 <- dynlm(fdiffPCPI ~ L(fdiffPCPI) + L(fdiffPCPI, 2) 
                    + L(LURts) + L(LURts, 2),
                    start = c(1980), end = c(2020))
# show the coefficient, standard error and the singular regressor relevance
coeftest(Phil_curv3, vcov. = sandwich)
# F-test for the joint relevance of the regressors
# H0: regressor are irrelevant in explaining fdiffPCPI
linearHypothesis(Phil_curv3, 
                 c("L(fdiffPCPI)=0", "L(fdiffPCPI, 2)=0", 
                   "L(LURts) = 0","L(LURts, 2)=0"),
                 vcov. = sandwich)
# p-value:  6.066e-06
### Philips curve with 3 lags for the fdiffPCPI and 2 lags for the logLUR (Philips(3,2))
Phil_curv4 <- dynlm(fdiffPCPI ~ L(fdiffPCPI) + L(fdiffPCPI, 2) + L(fdiffPCPI, 3) 
                    + L(LURts) + L(LURts, 2),
                    start = c(1980), end = c(2020))
# show the coefficient, standard error and the singular regressor relevance
coeftest(Phil_curv4, vcov. = sandwich)
# F-test for the joint relevance of the regressors
# H0: regressor are irrelevant in explaining fdiffPCPI
linearHypothesis(Phil_curv4, 
                 c("L(fdiffPCPI)=0", "L(fdiffPCPI, 2)=0", "L(fdiffPCPI, 3)=0", 
                   "L(LURts) = 0","L(LURts, 2)=0"),
                 vcov. = sandwich)
# p-value: 0.003174
### Philips curve with 3 lags for the fdiffPCPI and 3 lags for the logLUR (Philips(3,3))
Phil_curv5 <- dynlm(fdiffPCPI ~ L(fdiffPCPI) + L(fdiffPCPI, 2) + L(fdiffPCPI, 3) 
                    + L(LURts) + L(LURts, 2) + L(LURts, 3),
                    start = c(1980), end = c(2020))
# show the coefficient, standard error and the singular regressor relevance
coeftest(Phil_curv5, vcov. = sandwich)
# F-test for the joint relevance of the regressors
# H0: regressor are irrelevant in explaining fdiffPCPI
linearHypothesis(Phil_curv5, 
                 c("L(fdiffPCPI)=0", "L(fdiffPCPI, 2)=0", "L(fdiffPCPI, 3)=0", 
                   "L(LURts) = 0","L(LURts, 2)=0", "L(LURts, 3)=0"),
                 vcov. = sandwich)
# p-value: 0.005237
### Sum up all the model together in the same table
stargazer(Phil_curv1, Phil_curv2, Phil_curv3, Phil_curv4, Phil_curv5, type = "text")
######################### lag-length with information criterion approach
### The BIC is used to select lag-lengths in time series regression models with multiple predictors.
# loop 'BIC()' over multiple models (with lagmax = 12)
order <- 1:12
# compute BIC for various model with ad hoc function
BICs <- sapply(order, function(x) 
  BIC_comp(dynlm(fdiffPCPI ~ L(fdiffPCPI, 1:x) + L(LURts, 1:x),
                 start = c(1982), end = c(2020))))
#note that with this function, the BIC is computed ony for model where regressors' have the same lag (i.e 1,1; 2,2 etc)
# show BICs for various model
BICs
# select the ADL model with the smallest BIC
BICs[, which.min(BICs[2, ])]
# the BIC in in favor of the Philips curve (1,1) model estimated before
#now consdier even the model with different lags for the 2 variables with the BIC function in Stat
library(stats)
BIC(Phil_curv1)
BIC(Phil_curv2)
BIC(Phil_curv3)
BIC(Phil_curv4)
BIC(Phil_curv5)
#again, the BIC is in favor of the Phil_curv1
######################### show the chosen model
# compute robust standard errors for the chosen model
rob_std.err <- list(sqrt(diag(sandwich(Phil_curv1))))
# summarize Phil_curve1
summary(Phil_curv1, diagnostics = TRUE, se = rob_std.err)
# create a table with coef and se for Philips curve (1,1)
stargazer(Phil_curv1,
          type = "text",
          title = "",
          header = FALSE, 
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = "PHC(1,1)",
          dep.var.labels.include = FALSE,
          se = rob_std.err)
#check model accuracy
library(forecast)
accuracy(Phil_curv1)
#
fit <- fitted.values(Phil_curv1)
#transform fitted values into a ts
fit2 <- ts(fit, start = c(1980), end = c(2019), frequency = 1)
#plot the real values vs the estimated ones
library(scales)
par(mfrow=c(1,1))
par(mar = c(5,4,4,8),
    xpd = TRUE)
plot.ts(fdiffPCPI,type="l",col=alpha("orange", alpha=0.7), main ="Philip Curve Model against actual PCPI(diff)", 
        xlab = "Year", ylab = "PCPI (diff)", axes = TRUE, bty="l")
lines(fit2,col= alpha("darkgreen",alpha=0.5))
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Actual Data", "Philip Curve (1,1)"), 
       col = c(alpha("orange",alpha=0.7), alpha("darkgreen",alpha=0.5)))
#save it
pdf("out_data/realvsfitted.pdf")
par(mfrow=c(1,1))
par(mar = c(5,4,4,8),
    xpd = TRUE)
plot.ts(fdiffPCPI,type="l",col=alpha("orange", alpha=0.7), main ="Philip Curve Model against actual PCPI(diff)", 
        xlab = "Year", ylab = "PCPI (diff)", axes = TRUE, bty="l")
lines(fit2,col= alpha("darkgreen",alpha=0.5))
legend(x="topright", inset = c(-0.4,0), lty = c(1,1), legend = c("Actual Data", "Philip Curve (1,1)"), 
       col = c(alpha("orange",alpha=0.7), alpha("darkgreen",alpha=0.5)))
dev.off()
#################################################  THE END  ########################################################