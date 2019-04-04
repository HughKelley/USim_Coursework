library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(leaflet)

EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")

London <- EW[grep("^E09",EW@data$lad15cd),]
#plot it
plot(London)


BNG = "+init=epsg:27700"
LondonBNG <- spTransform(London, BNG)
#now, order by borough code - *this step will be imporant later on*
LondonBNG <- LondonBNG[order(LondonBNG$lad15cd),]
#now use spDists to generate a big distance matrix of all distances between boroughs in London
dist <- spDists(LondonBNG)
#melt this matrix into a list of origin/destination pairs using melt. Melt in in the reshape2 package. Reshape2, dplyr and ggplot, together, are some of the best packages in R, so if you are not familiar with them, get googling and your life will be much better!
distPair <- melt(dist)


#read in your London Commuting Data
cdata <- read.csv("https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1")
#read in a lookup table for translating between old borough codes and new borough codes
CodeLookup <- read.csv("https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1")
#read in some population and income data
popincome <- read.csv("https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1")

#now merge these supplimentary data into your flow data dataframe
cdata$OrigCodeNew <- CodeLookup$NewCode[match(cdata$OrigCode, CodeLookup$OldCode)]
cdata$DestCodeNew <- CodeLookup$NewCode[match(cdata$DestCode, CodeLookup$OldCode)]
cdata$vi1_origpop <- popincome$pop[match(cdata$OrigCodeNew, popincome$code)]
cdata$vi2_origsal <- popincome$med_income[match(cdata$OrigCodeNew, popincome$code)]
cdata$wj1_destpop <- popincome$pop[match(cdata$DestCodeNew, popincome$code)]
cdata$wj2_destsal <- popincome$med_income[match(cdata$DestCodeNew, popincome$code)]

#Data needs to be ordered by borough code, if it's not, we will run into problems when we try to merge our distance data back in later, so to make sure, we can arrange by orign and then destination using dplyr's 'arrange' function

cdata <- arrange(cdata, OrigCodeNew, DestCodeNew)

#First create a new total column which excludes intra-borough flow totals (well sets them to a very very small number for reasons you will see later...)
cdata$TotalNoIntra <- ifelse(cdata$OrigCode == cdata$DestCode,0,cdata$Total)
cdata$offset <- ifelse(cdata$OrigCode == cdata$DestCode,0.0000000001,1)
# now add the distance column into the dataframe
cdata$dist <- distPair$value

#remove intraborough flows
cdata <- cdata[!(cdata$OrigCodeNew == cdata$DestCodeNew),]

cdatasub <- cdata 

cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything())

#use the od2line function from RObin Lovelace's excellent stplanr package
travel_network <- od2line(flow = cdatasub, zones = LondonBNG)
#and set the line widths to some sensible value according to the flow
w <- cdatasub$Total / max(cdatasub$Total) * 10
#now plot it...
plot(travel_network, lwd = w)
plot(LondonBNG, add=T)

#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig", "Dest"))
cdatasubmat


#set up some variables to hold our parameter values in:
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(cdatasub$Total)

vi1_mu <- cdatasub$vi1_origpop^mu
wj2_alpha <- cdatasub$wj2_destsal^alpha
dist_beta <- cdatasub$dist^beta
T1 <- vi1_mu*wj2_alpha*dist_beta
k <- T2/sum(T1)

#run the model and store all of the new flow estimates in a new column in the dataframe
cdatasub$unconstrainedEst1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
#check that the sum of these estimates makes sense
sum(cdatasub$unconstrainedEst1)

cdatasubmat1 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "unconstrainedEst1", margins=c("Orig", "Dest"))
cdatasubmat1

CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}
CalcRSquared(cdatasub$Total,cdatasub$unconstrainedEst1)
# [1] 0.2557722

CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}
CalcRMSE(cdatasub$Total,cdatasub$unconstrainedEst1)
# [1] 3179.638

############################################################################
# poisson unconstrained model

#run the unconstrained model
uncosim <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)

# now calculate estimates
cdatasub$fitted <- fitted(uncosim)

cdatasub$fitted <- round(cdatasub$fitted,0)
sum(cdatasub$fitted)


CalcRSquared(cdatasub$Total,cdatasub$fitted)
# [1] 0.3857616

CalcRMSE(cdatasub$Total,cdatasub$fitted)
# [1] 2330.952