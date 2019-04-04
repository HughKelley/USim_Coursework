# preps all the london data for the spatial interaction modelling

# load london data
# data_source <- "http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson"
# layers <- rgdal::ogrListLayers(data_source)
# layer_name <- layers[1]
# EW <- geojsonio::geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
# London <- EW[grep("^E09",EW@data$lad15cd),]
# 
# BNG = "+init=epsg:27700"
# 
# LondonBNG <- sp::spTransform(London, BNG)
# place_holder <- rgdal::writeOGR(LondonBNG, dsn = "London_Boundaries.geojson", "London_Boundaries", driver = "GeoJSON")
# placeholder
# or

LondonBNG <- geojsonio::geojson_read("London_Boundaries.geojson", what = "sp")

# order by borough code
LondonBNG <- LondonBNG[order(LondonBNG$lad15cd),]

dist <- sp::spDists(LondonBNG)

distPair <- reshape2::melt(dist)


#read in your London Commuting Data
commute_data <- read.csv("https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1")
#read in a lookup table for translating between old borough codes and new borough codes
CodeLookup <- read.csv("https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1")
#read in some population and income data
popincome <- read.csv("https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1")

#now merge these supplimentary data into your flow data dataframe
commute_data$OrigCodeNew <- CodeLookup$NewCode[match(commute_data$OrigCode, CodeLookup$OldCode)]
commute_data$DestCodeNew <- CodeLookup$NewCode[match(commute_data$DestCode, CodeLookup$OldCode)]
commute_data$vi1_origpop <- popincome$pop[match(commute_data$OrigCodeNew, popincome$code)]
commute_data$vi2_origsal <- popincome$med_income[match(commute_data$OrigCodeNew, popincome$code)]
commute_data$wj1_destpop <- popincome$pop[match(commute_data$DestCodeNew, popincome$code)]
commute_data$wj2_destsal <- popincome$med_income[match(commute_data$DestCodeNew, popincome$code)]

#Data needs to be ordered by borough code, if it's not, we will run into problems when we try to merge our distance data back in later, so to make sure, we can arrange by orign and then destination using dplyr's 'arrange' function

commute_data <- dplyr::arrange(commute_data, OrigCodeNew, DestCodeNew)


# now add the distance column into the dataframe
commute_data$dist <- distPair$value

# remove "loops" or flows within a borough
# follow: 
# d<-d[!(d$A=="B" & d$E==0),]

commute_data <- commute_data[!(commute_data$OrigCodeNew == commute_data$DestCodeNew), ]

#First create a new total column which excludes intra-borough flow totals (well sets them to a very very small number for reasons you will see later...)
# commute_data$TotalNoIntra <- ifelse(commute_data$OrigCode == commute_data$DestCode,0.0000000001,commute_data$Total)
# commute_data$dist <- ifelse(commute_data$OrigCode == commute_data$DestCode, 0.0000000001, commute_data$dist)

commute_data <- dplyr::select(commute_data, OrigCodeNew, DestCodeNew, Total, everything())

commute_data_matrix <- dcast(commute_data, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig","Dest"))
