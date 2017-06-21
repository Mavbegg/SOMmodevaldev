#LOFIT

LOFIT <- function(meas,mod)
{
  
  LOFIT <- (((meas$Mean-mod$Mod)^2)*meas$Replicates)  
  lofsum<- sum(LOFIT)
  LOFIT2 <- (((meas$Means-meas$Mod)^2)*meas$Replicates)*(meas$Replicates-1)
  
  #((Rep 1 - Mod)-(Means-mod))^2
  LO <- function(O,Oj,M)
  {
    ((O-M)-(Oj-M))^2
  }
  
  LOF<- mapply(LO,meas,!names[(meas) %in% c("Mean","SEM","Replicates")],meas$Mean,mod$Mod)
  
  Losum <- rowSums(LOF)
  Losumsum <- sum(Losum) 
  
  #F: 
  sumlofit <- sum(LOFIT)
  Fvalue <- sumlofit/(length(Losum)*Losumsum)
  FvalueatP<- qf(0.05, (length(mod$Mod)-2),(length(meas[(meas) %in% c("Mean","SEM","Replicates")])*(nrow(meas[(meas) %in% c("Mean","SEM","Replicates")])))-2, lower.tail = FALSE)
}


