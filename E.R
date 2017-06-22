
E <- function(meas, mod) {
  t95<- abs(qt(0.05/2,(meas$Replicates)-2))
  Meanmeas <- mean(meas$Mean)
  diffmeasmod <- meas$Mean-mod$Mod
  sumdiffmeasmod <- sum(diffmeasmod)
  SET95<-meas$SEM*t95
  sumSET95 <- sum(SET95)
  
  E <-(100/Meanmeas)*sumdiffmeasmod/length(diffmeasmod)
  E95 <-(100/Meanmeas)*sumSET95/length(SET95) 
 Sigbi <-ifelse(E>E95,"Yes-Bad","No-Good")
  output <-list("Relative Error"=E,"E (95% Confidence Limit)"=E95,"Significant bias?"=Sigbi)
  return(output)
  
}


  
