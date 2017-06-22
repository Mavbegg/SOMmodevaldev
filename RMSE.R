
RMSE <- function(meas,mod)
{
  Meanmeas <- mean(meas$Mean)
  
  difmeasmodsq <- (meas$Mean-mod$Mod)^2
  difmeasmodsqsum <- sum((meas$Mean-mod$Mod)^2)
  
  RMSE<-(100/Meanmeas)*sqrt(difmeasmodsqsum/length(meas$Mean))
  
  t95<- abs(qt(0.05/2,(meas$Replicates)-2))
  SEt95sq <- (meas$SEM*t95)^2
  sumSET95sq <- sum(SEt95sq)
  RMSE95<- (100/Meanmeas)*sqrt(sumSET95sq/length(SEt95sq)) 
    Siger <- ifelse(RMSE<RMSE95,"No-Good","Yes-Bad")
  
  output <-list("Root measn square error of model"=RMSE,"RMSE (95% Confidence Limit)"=RMSE95,"Significant total error?"=Siger)
  return(output)
  
}
  

