##########################################################################
#This block of code will calculate balancing factors for an entropy
#maximising model (doubly constrained)


#set beta to the appropriate value according to whether exponential or power
# if(tail(names(coef(doubSim)),1)=="dist"){
#   cdatasub$beta <- coef(doubSim)["dist"]
#   disdecay = 0
# } else {
#   cdatasub$beta <- coef(doubSim)["log(dist)"]
#   disdecay = 1
# }

balancing_factor_calc <- function(cdatasub){
  
  disdecay = 1
  
  cdatasub$Ai <- 1
  cdatasub$Bj <- 1
  cdatasub$OldAi <- 10
  cdatasub$OldBj <- 10
  cdatasub$diff <- abs((cdatasub$OldAi-cdatasub$Ai)/cdatasub$OldAi)
  
  #create convergence and iteration variables and give them initial values
  cnvg = 10
  its = 0
  #This is a while-loop which will calculate Orig and Dest balancing
  #factors until the specified convergence criteria is met
  
  while(cnvg > 1)
  {
    
    print(paste0("iteration ", its))
    its = its + 1 #increment the iteration counter by 1
    
    #First some initial calculations for Ai...
    if(disdecay==0)
      {
        cdatasub$Ai <- (cdatasub$Bj*cdatasub$D_j*exp(cdatasub$distances*cdatasub$beta))
      } else 
      {
        cdatasub$Ai <- (cdatasub$Bj*cdatasub$D_j*exp(log(cdatasub$distances)*cdatasub$beta))
      } 
    
    #aggregate the results by your Origs and store in a new dataframe
    AiBF <- aggregate(Ai ~ Orig, data = cdatasub, sum)
    #now divide by 1
    AiBF$Ai <- 1/AiBF$Ai 
    #and replace the initial values with the new balancing factors
    updates = AiBF[match(cdatasub$Orig,AiBF$Orig),"Ai"]
    cdatasub$Ai = ifelse(!is.na(updates), updates, cdatasub$Ai)
    
    
    #now, if not the first iteration, calculate the difference between  the new Ai values and the old Ai values and once done, overwrite the old Ai values with the new ones. 
    if(its==1)
      {
        cdatasub$OldAi <- cdatasub$Ai    
      } else 
      {
        cdatasub$diff <- abs((cdatasub$OldAi-cdatasub$Ai)/cdatasub$OldAi)    
        cdatasub$OldAi <- cdatasub$Ai
      }
    
    #Now some similar calculations for Bj...
    if(disdecay==0)
      {
        cdatasub$Bj <- (cdatasub$Ai*cdatasub$O_i*exp(cdatasub$distances*cdatasub$beta))
      } else 
      {
        cdatasub$Bj <- (cdatasub$Ai*cdatasub$O_i*exp(log(cdatasub$distances)*cdatasub$beta))
      }
    
    
    #aggregate the results by your Dests and store in a new dataframe
    BjBF <- aggregate(Bj ~ Dest, data = cdatasub, sum)
    #now divide by 1
    BjBF$Bj <- 1/BjBF$Bj  
    #and replace the initial values by the balancing factor
    updates = BjBF[match(cdatasub$Dest,BjBF$Dest),"Bj"]
    cdatasub$Bj = ifelse(!is.na(updates), updates, cdatasub$Bj)
    
    
    #now, if not the first iteration, calculate the difference between the new Bj values and the old Bj values and once done, overwrite the old Bj values with the new ones.
    if(its==1)
      {
        cdatasub$OldBj <- cdatasub$Bj
      } else 
      {
        cdatasub$diff <- abs((cdatasub$OldBj-cdatasub$Bj)/cdatasub$OldBj)    
        cdatasub$OldBj <- cdatasub$Bj
      } 
    
    #overwrite the convergence variable with 
    cnvg = sum(cdatasub$diff)
    
  } # end of while
  
  return(cdatasub)
  
} # end of function

