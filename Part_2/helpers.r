

package_loader <- function(x)
{
  if (require(sp) &&  require(MASS) &&
  require(reshape2) &&
  require(geojsonio) &&
  require(rgdal) &&
  require(tidyverse) &&
  require(downloader) &&
  require(maptools) &&
  require(dplyr) &&
  require(broom) &&
  require(stplanr) &&
  require(ggplot2) &&
  require(leaflet)){
    
    return(0)
  }
  else {return(1)}
}



data_loader <- function(a,b,c,d)
{
  require(sp)
  boundaries = geojson_read(a, what = "sp")
  london_boundaries = boundaries[grep("^E09", boundaries@data$lad15cd),]
  BNG = "+init=epsg:27700"
  BNGLondon = spTransform(london_boundaries, BNG)
  
  # order by borough code
  BNGLondon = BNGLondon[order(BNGLondon$lad15cd),]
  
  # calc matrix of distances between borough center points
  distances = spDists(BNGLondon)
  
  paired_distances = melt(distances)
  
  commute_data = read.csv(b)
  
  code_dict = read.csv(c)
  
  pop_and_inc_data = read.csv(d)
  
  ####
  # Merge data
  
  commute_data$OrigCodeNew = code_dict$NewCode[match(commute_data$OrigCode, code_dict$OldCode)]
  commute_data$DestCodeNew <- code_dict$NewCode[match(commute_data$DestCode, code_dict$OldCode)]
  commute_data$vi1_origpop <- pop_and_inc_data$pop[match(commute_data$OrigCodeNew, pop_and_inc_data$code)]
  commute_data$vi2_origsal <- pop_and_inc_data$med_income[match(commute_data$OrigCodeNew, pop_and_inc_data$code)]
  commute_data$wj1_destpop <- pop_and_inc_data$pop[match(commute_data$DestCodeNew, pop_and_inc_data$code)]
  commute_data$wj2_destsal <- pop_and_inc_data$med_income[match(commute_data$DestCodeNew, pop_and_inc_data$code)]
  
  # then sort correctly
  
  commute_data = arrange(commute_data, OrigCodeNew, DestCodeNew)
  
  # use distances in kilometers hopefully solving bug in balancing
  # factor calculation for double constrained model
  commute_data$distances = paired_distances$value / 1000
  # remove intra-borough flows
  
  commute_data = commute_data[!(commute_data$OrigCodeNew == commute_data$DestCodeNew),]
  
  # sort the columns and return
  commute_data = dplyr::select(commute_data, OrigCodeNew, DestCodeNew, Total, everything())
  return(commute_data)
}

CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}

CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}


O_and_D_flow_totals <- function(flow_data_frame) {
  
  O_i <- flow_data_frame %>% group_by(OrigCodeNew) %>% summarise(O_i = sum(Total))
  flow_data_frame$O_i <- O_i$O_i[match(flow_data_frame$OrigCodeNew,O_i$OrigCodeNew)]
  D_j <- flow_data_frame %>% group_by(DestCodeNew) %>% summarise(D_j = sum(Total))
  flow_data_frame$D_j <- D_j$D_j[match(flow_data_frame$DestCodeNew,D_j$DestCodeNew)]
  
  return(flow_data_frame)
}

subsetter <- function (data, boroughs) {
  
  len = length(boroughs)
  # origins
  cdatasub <- cdata[grep(paste(toMatch,collapse = "|"), cdata$OrigCode),]
  # destinations
  cdatasub <- cdatasub[grep(paste(toMatch,collapse = "|"), cdata$DestCode),]
  # take just the rows with data
  cdatasub <- cdatasub[1:((len^2)-2),]
  # check order
  cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything())
  }


flow_totals <- function(cdatasub)
{
  O_i <- cdatasub %>% group_by(OrigCodeNew) %>% summarise(O_i = sum(Total))
  cdatasub$O_i <- O_i$O_i[match(cdatasub$OrigCodeNew,O_i$OrigCodeNew)]
  D_j <- cdatasub %>% group_by(DestCodeNew) %>% summarise(D_j = sum(Total))
  cdatasub$D_j <- D_j$D_j[match(cdatasub$DestCodeNew,D_j$DestCodeNew)]

  return(cdatasub)
  
  }

# load function to calculate balancing factors for double constrained model
source('seniors_algorithm.r', echo=TRUE)


# code_dictionary = "https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1"
# codes <- read.csv(code_dictionary)
# written_codes <- write.csv(codes, file='borough_codes.csv')
# rm(codes, written_codes, code_dictionary)

