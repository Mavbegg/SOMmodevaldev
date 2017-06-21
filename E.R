#E
E <- function(meas, mod) {
  Meanmeas <- mean(meas$Mean)
  diffmeasmod <- meas$Mean-mod$Mod
  sumdiffmeasmod <- sum(diffmeasmod)
  SET95<-meas$SEM*t95
  sumSET95 <- sum(SET95)
  
  E <-(100/Meanmeas)*sumdiffmeasmod/length(diffmeasmod)
  E95 <-(100/Meanmeas)*sumSET95/length(SET95) 
  
  
}

