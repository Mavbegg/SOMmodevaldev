
M <- function(meas,mod) {
  diffmeasmod <- meas$Mean-mod$Mod
   meandiffmeasmod <- mean(diffmeasmod)
  M1<- (diffmeasmod-meandiffmeasmod)^2
  sumM1 <- sum(M1)
  tvalue <-(meandiffmeasmod*(sqrt(length(M1)))/sqrt((sumM1-meandiffmeasmod)/(length(M1)-1)))
  t2.5<- abs(qt(0.05/2,(length(M1))-2))  
  
  sigbi <- ifelse(tvalue>t2.5,"Yes-Bad","No-Good")
  output <-list("Mean Difference"=meandiffmeasmod,"Students's t of M"=tvalue,"t-value (Critical at 2.5%- Two tailed "=t2.5,"Significant bias?"=sigbi)
  return(output)
}
