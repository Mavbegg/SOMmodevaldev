
LOFIT <- function(meas,mod)
{
  LOFIT <- (((meas$Mean-mod$Mod)^2)*meas$Replicates)  
  lofsum<- sum(LOFIT)

  LOFIT2 <- (((meas$Mean-mod$Mod)^2)*meas$Replicates)*(meas$Replicates-1)
  lofsum2 <- sum(LOFIT2)

 
  LO <- function(O,Oj,M)
  {
    ((O-M)-(Oj-M))^2
  }
  
  LOF<- LO(meas[(!names(meas) %in% c("Mean","SEM","Replicates"))],meas$Mean,mod$Mod)
  
  Losum <- rowSums(LOF)
  Losumsum <- sum(Losum) 

  #F: 
  sumlofit <- sum(LOFIT)
  Fvalue <- lofsum2/(length(Losum)*Losumsum)
  FvalueatP<- qf(0.05, (length(mod$Mod)-2),(length(meas[(!names(meas) %in% c("Mean","SEM","Replicates"))])*(nrow(meas[!names(meas) %in% c("Mean","SEM","Replicates")])))-2, lower.tail = FALSE)
   Siger<- ifelse(Fvalue>FvalueatP,"Yes-Bad","No-Good")
  output <-list("Lack of Fit"=lofsum,"F=MSLOFIT/MSE"=Fvalue,"F (Critical at 5%"=FvalueatP,"Significant error between simulated and measured values?"=Siger)
  return(output)  
  
    
}
