library(sp)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(MASS)
library(sf)
library(tmap)
library(tmaptools)
library(stringr)

#run a production constrained SIM
prodSim <- glm(Total ~ Orig+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(prodSim)

# switch the reference borough

#set the contrasts
options(contrasts=c('contr.SAS','contr.SAS'))
prodSim2 <- glm(Total ~ Orig+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(prodSim2)


# switch back
options(contrasts=c('contr.treatment','contr.treatment'))

prodSim <- glm(Total ~ OrigCodeNew+log(wj2_destsal)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
summary(prodSim)


#create some Oi and Dj columns in the dataframe and store row and column totals in them:
#to create O_i, take cdatasub ...then... group by origcodenew ...then... summarise by calculating the sum of Total
O_i <- cdatasub %>% group_by(OrigCodeNew) %>% summarise(O_i = sum(Total))
cdatasub$O_i <- O_i$O_i[match(cdatasub$OrigCodeNew,O_i$OrigCodeNew)]
D_j <- cdatasub %>% group_by(DestCodeNew) %>% summarise(D_j = sum(Total))
cdatasub$D_j <- D_j$D_j[match(cdatasub$DestCodeNew,D_j$DestCodeNew)]

#You can do this in a number of ways, for example this prints everything:
prodSim_out <- tidy(prodSim)
prodSim_out

# or you can just pull out the coefficients and put them into an object
coefs <- as.data.frame(prodSim$coefficients)
#then once you have done this, you can join them back into the dataframe using a regular expression to match the bits of the identifier that you need - *note, this bit of code below took me about 2 hours to figure out!*
cdatasub$mu_i <- coefs$`prodSim$coefficients`[match(cdatasub$OrigCodeNew,sub(".*OrigCodeNew","", rownames(coefs)))]
#now, where we have missing values for our reference mu_i variable, fill those with 1s
cdatasub$mu_i <- ifelse(is.na(cdatasub$mu_i),0,cdatasub$mu_i)
head(cdatasub)

k <- prodSim$coefficients[1]
mu_i <- prodSim$coefficients[2:7]
alpha <- prodSim$coefficients[8]
beta <- prodSim$coefficients[9]

cdatasub$prodsimest1 <- exp(k+(cdatasub$mu_i)+(alpha*log(cdatasub$wj2_destsal))+(beta*log(cdatasub$dist)))
# or
cdatasub$prodsimFitted <- fitted(prodSim)

#first round the estimates
cdatasub$prodsimFitted <- round(fitted(prodSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat3 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "prodsimFitted", margins=c("Orig", "Dest"))
cdatasubmat3

CalcRSquared(cdatasub$Total,cdatasub$prodsimFitted)

CalcRMSE(cdatasub$Total,cdatasub$prodsimFitted)


#######################################################################
#######################################################################
# Scenario

cdatasub$wj3_destsalScenario <- cdatasub$wj2_destsal
cdatasub$wj3_destsalScenario <- ifelse(cdatasub$wj3_destsalScenario == 16200,25000,cdatasub$wj3_destsalScenario)

cdatasub$prodsimest2 <- exp(k+(cdatasub$mu_i)+(alpha*log(cdatasub$wj3_destsalScenario))+(beta*log(cdatasub$dist)))
cdatasub$prodsimest2 <- round(cdatasub$prodsimest2,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat4 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "prodsimest2", margins=c("Orig", "Dest"))
cdatasubmat4

#calculate some new wj^alpha and dij^beta values
wj2_alpha <- cdatasub$wj2_destsal^alpha
dist_beta <- cdatasub$dist^beta
#calculate the first stage of the Ai values
cdatasub$Ai1 <- wj2_alpha*dist_beta
#now do the sum over all js bit
A_i <- cdatasub %>% group_by(OrigCodeNew) %>% summarise(A_i = sum(Ai1))
#now divide in to 1
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdatasub$A_i <- A_i$A_i[match(cdatasub$OrigCodeNew,A_i$OrigCodeNew)]

#To check everything works, recreate the original estimates
cdatasub$prodsimest3 <- cdatasub$A_i*cdatasub$O_i*wj2_alpha*dist_beta


#calculate some new wj^alpha and dij^beta values
wj3_alpha <- cdatasub$wj3_destsalScenario^alpha
#calculate the first stage of the Ai values
cdatasub$Ai1 <- wj3_alpha*dist_beta
#now do the sum over all js bit
A_i <- cdatasub %>% group_by(OrigCodeNew) %>% summarise(A_i = sum(Ai1))
#now divide in to 1
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdatasub$A_i <- A_i$A_i[match(cdatasub$OrigCodeNew,A_i$OrigCodeNew)]

#To check everything works, recreate the original estimates
cdatasub$prodsimest4_scenario <- cdatasub$A_i*cdatasub$O_i*wj3_alpha*dist_beta

cdatasub$prodsimest4_scenario <- round(cdatasub$prodsimest4_scenario,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat5 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "prodsimest4_scenario", margins=c("Orig", "Dest"))
cdatasubmat5

#########################################################################
##########################################################################
# attraction constrained

attrSim <- glm(Total ~ DestCodeNew+log(vi1_origpop)+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
summary(attrSim)

#first round the estimates
cdatasub$attrsimFitted <- round(fitted(attrSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat6 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "attrsimFitted", margins=c("Orig", "Dest"))
cdatasubmat6

#use the functions from the last practical to calculate some goodness-of-fit statistics
CalcRSquared(cdatasub$Total,cdatasub$attrsimFitted)

CalcRMSE(cdatasub$Total,cdatasub$attrsimFitted)

#########################################################################
##########################################################################
# doubly constrained

# run a production constrained SIM
doubSim <- glm(Total ~ Orig+Dest+log(dist), na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(doubSim)

#then round the estimates
cdatasub$doubsimFitted <- round(fitted(doubSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat7 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "doubsimFitted", margins=c("Orig", "Dest"))
cdatasubmat7


CalcRSquared(cdatasub$Total,cdatasub$doubsimFitted)

CalcRMSE(cdatasub$Total,cdatasub$doubsimFitted)


#########################################################################
##########################################################################
# distance decay

#run a production constrained SIM
doubSim1 <- glm(Total ~ Orig+Dest+dist, na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
#let's have a look at it's summary...
summary(doubSim1)

cdatasub$doubsimFitted1 <- round(fitted(doubSim1),0)

CalcRSquared(cdatasub$Total,cdatasub$doubsimFitted1)

CalcRMSE(cdatasub$Total,cdatasub$doubsimFitted1)






