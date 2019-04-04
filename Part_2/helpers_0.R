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