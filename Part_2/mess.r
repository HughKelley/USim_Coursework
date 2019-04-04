#We'll just use the first 7 boroughs by code, so first, create a vector of these 7 to match with our data
toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG")
#subset the data by the 7 sample boroughs
#first the origins
cdatasub <- cdata[grep(paste(toMatch,collapse = "|"), cdata$OrigCode),]
#then the destinations
cdatasub <- cdatasub[grep(paste(toMatch,collapse = "|"), cdata$DestCode),]
#now chop out the intra-borough flows
cdatasub <- cdatasub[cdatasub$OrigCode!=cdatasub$DestCode,]
#now unfortunately if you look at the file, for some reason the grep process has left a lot of empty data cells in the dataframe, so let's just chop out everything after the 7*7 - 7 (42) pairs we are interested in...
cdatasub <- cdatasub[1:42,]
#now re-order so that OrigCodeNew, DestCodeNew and TotalNoIntra are the first three columns *note that you have to be explicit about the select function in the dplyr package as MASS also has a 'select' function and R will try and use this by default. We can be explict by using the syntax package::function
cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything())