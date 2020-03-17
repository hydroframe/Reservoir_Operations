rm (list=ls())
library(ggplot2)
library(dplyr)
library(lubridate)
library(reservoir)
#library(hrbrthemes)

#Script to test forward reservoir simulations

setwd("/Users/laura/Documents/Git_Repos/Reservoir_Operations") #CHANGE for your working directory

##################################################
## Read in the inputs
#Set the path to the database:
dbpath="/Users/laura/Google Drive File Stream/My Drive/Condon_Research_Group/Research_Projects/Jen_Dams/RESERVOIR_DATABASE"

#Pick the reservoir ID number that you want to evaluate
RID=421

#Read in the reservoir data and plot timeseries
fin=paste(dbpath,"/TimeSeries/", RID, ".csv", sep="")
resdata=as.data.frame(read.csv(fin))

##################################################

### temporarily sourcing functions
source("./ResOps/R/Make_Monthly.R")


##################################################

# convert timeseries to monthly data
source("./ResOps/R/Make_Monthly.R")
inflowM = make.monthly(dates=resdata$Date, values=resdata$IN, fun="sum")
outflowM = make.monthly(dates=resdata$Date, values=resdata$OUT, fun="sum")
storM = make.monthly(dates=resdata$Date, values=resdata$STOR, fun="first")

# Grab out
dates=resdata$Date
values=resdata$IN
TimeSeries=xts::xts(values, as.Date(dates))
mtest=month(TimeSeries)


# Calculate a standard operating policy from monthly values
source("./ResOps/R/Get_MonthlyPercentiles.R")
#pctl_list=(2:98)/100
pctl_list=c(0.25, 0.50, 0.75)
stor.pct= get.monthlypctl(storM$mat, percentile.list=pctl_list, year.range=c(1940, 2020))
outflow.pct= get.monthlypctl(outflowM$mat, percentile.list=pctl_list, year.range=c(1940, 2020))


# Calculate a standard operating policy from daily values
source("./ResOps/R/Make_TimeSeries.R")
source("./ResOps/R/Get_MonthlyPercentiles_FromDaily.R")
Stor.TimeSeries=make.xts(dates=resdata$Date, values=resdata$STOR, na.val=NA, na.omit = T)
Stor.pctD=monthlypctl.fromday(Stor.TimeSeries, percentile.list=pctl_list, start="1940-01-01", stop="2020-03-01")

IN.TimeSeries=make.xts(dates=resdata$Date, values=resdata$IN, na.val=NA, na.omit = T)
IN.pctD=monthlypctl.fromday(IN.TimeSeries, percentile.list=pctl_list, start="1940-01-01", stop="2020-03-01")

OUT.TimeSeries=make.xts(dates=resdata$Date, values=resdata$OUT, na.val=NA, na.omit = T)
OUT.pctD=monthlypctl.fromday(OUT.TimeSeries, percentile.list=pctl_list, start="1940-01-01", stop="2020-03-01")

### Make a plot of it
quartz()
par(mfrow=c(3,4))
for(m in 1:12){
  plot(stor.pct[,m], outflow.pct[,m], typ='l')
  #plot(Stor.pctD[,m], OUT.pctD[,m], typ='l', col=2)
}



### Do a forward reservoir simulation
source("./ResOps/R/Clalc_Release.R")



test=storM$mat
test1=apply(monthlydata,2,quantile, c(0.5, 0.75), na.rm=T)

quantile(monthlydata[,2], c(.5, .75), na.rm=T)

median(monthlydata[,2], na.rm=T)
####################################

values=resdata$IN

library(xts)
ints=xts(resdata$IN, as.Date(resdata$Date, "%Y-%m-%d"))
in_m=apply.monthly(ints, FUN=mean)


range(resdata$IN)

date_test=resdata$Date

#Convert to monthly # this one works
res.month=month(resdata$Date)
res.year=year(resdata$Date)
dd <- data.frame(res.month, res.year, resdata$IN)
dd.agg <- aggregate(resdata.IN ~ res.month + res.year, dd, FUN = sum)


dd.agg$date <- as.POSIXct(paste(dd.agg$res.year, dd.agg$res.month, "01", sep = "-"))
plot(dd.agg$date, dd.agg$resdata.IN, type="l")

## another way
library(xts)
ints=xts(resdata$IN, as.Date(resdata$Date, "%Y-%m-%d"))
in_m=apply.monthly(ts, FUN=sum)

outts=xts(resdata$OUT, as.Date(resdata$Date, "%Y-%m-%d"))
out_m=apply.monthly(outts, FUN=sum)

start=endpoints(ts, on="months", k=1)

nts=xts(resdata$STOR, as.Date(resdata$Date, "%Y-%m-%d"))
storstart=endpoints(ints, "months")+1

first.values <- endpoints(nts, "months") + 1
first.values <- first.values[-length(first.values)]
last.values <- endpoints(nts, "months")
last.values <- last.values[-1]
dstor=nts[last.values]-nts[first.values]


#########
#Testing the reservoir library
# DEFINE RESERVOIR SPECS AND MODEL INPUTS
res_cap <- 1500 #Mm3
targ <- 150 #Mm3
area <- 40 #km2
max_d <- 40 #m
ev = 0.2 #m
Q_pre1980 <- window(resX$Q_Mm3, end = c(1979, 12), frequency = 12)
Q_post1980 <- window(resX$Q_Mm3, start = c(1980, 1), frequency = 12)
# SIMULATE WITH SOP
layout(1:3)
simSOP <- simRes(Q_post1980, capacity = res_cap, target = targ,
                 surface_area = area, max_depth = max_d, evap = ev)
# TRAIN SDP POLICY ON HISTORICAL FLOWS
policy_x <- sdp_supply(Q_pre1980, capacity = res_cap, target = targ,
                       surface_area = area, max_depth = max_d, evap = ev, Markov = TRUE, plot = FALSE, S_disc = 100)
# SIMULATE WITH SDP-DERIVED POLICY
simSDP <- simRes(Q_post1980, capacity = res_cap, target = targ,
                 surface_area = area, max_depth = max_d, evap = ev, policy = policy_x)




####
##All scratch  work below here







first.values <- month[head(endpoints(month, "months") + 1, -1)]


plot(in_m, col="green", lwd=1)
lines(out_m, col="red", lty=1)



#Convert to monthly # this one works
df.my=floor_date(resdata$Date, "month")


test=aggregate(resdata.IN, by=res.month, FUN=mean)

#add the month as a column
resdata <- resdata %>%
  mutate(month = res.month)

dates=as.Date(resdata$Date)
data.monthly <- endpoints(rdates, on="months", k=1)




res.month<- resdata %>%
  group_by(month) %>%
  summarise(sum_IN = sum(IN))

#test=daily2monthly(resdata$IN, FUN=mean)
#month=format(resdata$Date, '%Y.%M')
#aggregate(z, by=week, FUN=mean)



# Make a timeseries plot of the reservoir water balance
#p <- ggplot(resdata, aes(x=Date, y=IN)) +
p1 <- ggplot(resdata, aes(x=Date, y=IN, group=1)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Temperature: range 1-10")
  #theme_ipsum()
p1

