## Cody Crunkilton
## Time Series: VAR Assignment
## Spring 2018



install.packages(c("vars", "tseries", "forecast"))
setwd("C:/Users/Cody/Dropbox/1school17_18/time series")
library(tidyverse)
select <- dplyr::select ## what error caused this? 
library(readstata13)
# install.packages("vars")
library(vars)
library(tseries)
library(forecast)

options(tibble.width=NULL)

# reading in data ---------------------------------------------------------


d <- read.dta13("nato_ds.dta")
d <- as.tibble(d)
d

small <- d %>% 
  select(year, rlusds, rlsovds, natods) %>% 
  rename(US=rlusds, Soviet=rlsovds, NATO=natods)

small$NATO <- ts(small$NATO)
small$US <- ts(small$US)
small$Soviet <- ts(small$Soviet)

small

divide <- function(x){
  x/1000000
}

small <- small[,2:4] %>% 
  apply(MARGIN = 2, FUN = divide) %>% 
  data.frame()

small$year <- c(1953:1988)

small$NATO <- small$NATO-small$US
small
# ggplot ------------------------------------------------------------------


## Getting ready for ggplot
small_plot <- small %>% 
  gather(Country, Spending, US:NATO)

# The plot:

ggplot(small_plot, aes(year, Spending, color=Country))+
  geom_line()


'
##### This part is setting up for defense spending as a share of GDP:
d_capita <- d %>% 
  select(year, rlusgdp, rlsovgdp, rlukgdp) %>% 
  rename(US=rlusgdp, Soviet=rlsovgdp, UK=rlukgdp) %>% 
  gather(Country, GDP, US:UK)  

d_merged <- merge(d2, d_capita, by=c("Country", "year"))
d_merged$ds_gdp <- d_merged$Spending/d_merged$GDP


## Heres the actual graph
ggplot(d_merged, aes(year, ds_gdp, color=Country))+
  geom_line()

## Getting this into wide format for the regressions:

g <- d_merged %>% 
  select(year, Country, ds_gdp) %>% 
  spread(Country, ds_gdp) 
'
# Testing for Stationarity ------------------------------------------------

adf.test(small$US)
adf.test(small$Soviet)
adf.test(small$NATO)

# US and Soviet are NOT stationary. NATO looks not stationary, but the Dickey-Fuller test disagrees so I keep it as is.

Pacf(small$US)
Pacf(small$Soviet)

# Testing for cointegration:

cointegration <- ca.jo(small[,1:3], type = "eigen")

summary(cointegration)

# no cointegration. I proceed with VAR, first-differencing the US and Soviet data. 

Soviet_diff <- diff(small$Soviet, lag=1)
US_diff <- diff(small$US, lag=1)
# NATO_diff <- diff(small$NATO, lag=1)

dif <- data.frame(year=c(1954:1988), Soviet=Soviet_diff, US=US_diff, NATO=small$NATO[-1])

plot(dif$NATO)

small <- dif
 

 
######################


# Test for lag order ------------------------------------------------------

# theory: 1 bc data is small. 

Acf(small$US)
Pacf(small$US)

Acf(g$US)
Pacf(g$US)

# looks like the US is an AR1 process

Acf(small$US)
Pacf(small$US)

Acf(small$Soviet)
Pacf(small$Soviet)

Acf(small$NATO)
Pacf(small$NATO)


Acf(g$Soviet)
Pacf(g$Soviet)
# also looks like AR1 for the Soviets - note five year increments- five year plans? 
small
# the var package's canned command also suggests 1:
VARselect(small[,2:4], lag.max =6, type="both") # 1 lag is best.

# note that given small dataset increasing lags=can't compute values with OLS. I tried this measure up to higher lag lengths, but began getting NA and -infinity results. 

# VAR ---------------------------------------------------------------------

v <- VAR(small[,2:4], p=1)

summary(v)

# Diagnostic plots--the fit is on top. 
plot(v, names="US")
plot(v, names="Soviet")
plot(v, names="NATO")



## more tests:
ser1 <- serial.test(v, lags.pt = 16, type = "PT.asymptotic")

ser1$serial

# per capita: don't care about this
'
v_capita <- VAR(g[,2:4])
'

# Granger Causality -------------------------------------------------------

# Does soviet spending cause US spending?
grangertest(small$US~small$Soviet, order=1)

# Does US spending cause Soviet spending?
grangertest(small$Soviet~small$US, order=1)

# Does US spending cause NATO spending?
grangertest(small$NATO~small$US, order=1)

# the rest:
grangertest(small$US~small$NATO, order=1) # Interesting. 
grangertest(small$Soviet~small$NATO, order=1) # .17
grangertest(small$NATO~small$Soviet, order=1) # Significant too

# No to all - p values are from .1 to .2 for all. 

## For per capita:
'
grangertest(g$US~g$Soviet, order=1) #definitely not

grangertest(g$Soviet~g$US, order=1)
'
# Still no to both. 

## QUESTION: how to control for GDP in equation? 


# impulse response function -----------------------------------------------

irf_sn <- irf(v, impulse = c("Soviet"), response = "NATO",boot=T)
plot(irf_sn)

irf_su <- irf(v, impulse = c("Soviet"), response = "US",boot=T)
plot(irf_su)

irf_un <- irf(v, impulse = c("US"), response = "NATO",boot=T)
plot(irf_un)

irf_nu <- irf(v, impulse = c("NATO"), response = "US",boot=T)
plot(irf_nu)

irf_us <- irf(v, impulse = c("US"), response = "Soviet",boot=T)
plot(irf_us)

irf_ns <- irf(v, impulse = c("NATO"), response = "Soviet",boot=T)
plot(irf_ns)

## ordering the shocks:

irf_USN <- irf(v, impulse = c("US"), response = c("US", "Soviet", "NATO"), boot=T)
plot(irf_USN)

irf_UNS <- irf(v, impulse = c("US"), response = c("US", "Soviet", "NATO"), boot=T)
plot(irf_UNS)

irf_SUN <- irf(v, impulse = c("Soviet"), response = c("US", "Soviet", "NATO"), boot=T)
plot(irf_SUN)


irf_NSU <- irf(v, impulse = c("NATO"), response = c("US", "Soviet", "NATO"), boot=T)
plot(irf_NSU)

#note for paper: flip these orderings around. 

## non-US NATO matters????

# Looking at only US and Soviet: basically the same results?
v2 <- VAR(small[,2:3])
irf_su2 <- irf(v2, impulse = "Soviet", response = "US",boot=T)
plot(irf_su2)

irf_us2 <- irf(v2, impulse = c("US"), response = "Soviet",boot=T)
plot(irf_us2)

'
irf_capita <- irf(v_capita, impulse = "US", response = c("UK", "Soviet"),boot=F)

plot(irf_capita)
'

# forecast error decomposition --------------------------------------------

errors <- fevd(v)
errors

summary(errors)

plot(errors)
plot(errors$US)

plot(errors$NATO)

plot(errors$Soviet)



