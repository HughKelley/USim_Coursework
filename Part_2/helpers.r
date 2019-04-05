

package_loader <- function(x)
{
  if (require(sp) &&  require(MASS) &&
  require(reshape2) &&
  require(geojsonio) &&
  require(rgdal) &&
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
  commute_data$distances = paired_distances$value
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



