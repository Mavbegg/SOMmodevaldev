
# data import. Rows as relicates, calc mean, SE about measured mean, input modelled value.

# Leave import to user, instruct to store measuresed samples in 'meas', modelled in 'mod'. Samples in rows, replicates as colums. See example data.ID as well
#measured<- read.csv("exinp.csv", header = TRUE) # read csv as in excel
#mod<- read.csv("exmod.csv", header = TRUE) # read csv as in excel
#meas<- measured[2:7]
#ID <- measured[1]

modeval <- function(meas,mod) {
  meas["Mean"] <- rowMeans(meas)  
  
  meas["SEM"]<- apply(meas[!names(meas) %in% c("Mean","SEM","Replicates")],1,function(meas)sd(meas)/sqrt(length(meas)))
  meas["Replicates"]<- ncol(meas[!names(meas) %in% c("Mean","SEM")])
  
  
  
  
  
  Meanmeas <- mean(meas$Mean)
  meanmod <- mean(mod$Mod)
  r1<- (mod$Mod-meanmod)
  r1sq<- sum((mod$Mod-meanmod)^2)
  r2 <- (meas$Mean-Meanmeas)
  r2sq <- sum((meas$Mean-Meanmeas)^2)
  r3<- r1*r2
  r3sum <- sum(r1*r2)
  r4 <- sqrt(r1sq)
  r5 <- sqrt(r2sq)
  r <- r3sum/(r4*r5)
  rFvalue <- ((length(r3)-2)*r*r)/(1-r*r)
  rFvaluep0.05<- abs(qt(0.05/2,(length(r3))-2)^2)
  sigas <- ifelse(rFvalue>rFvaluep0.05,"Yes-Good","No-Bad")
  routput <-list("r"=r,"F value"=rFvalue,"F value at p=0.05"=rFvaluep0.05,"Signficant association? "=sigas)
 
  diffmeasmod <- meas$Mean-mod$Mod
  meandiffmeasmod <- mean(diffmeasmod)
  M1<- (diffmeasmod-meandiffmeasmod)^2
  sumM1 <- sum(M1)
  tvalue <-(meandiffmeasmod*(sqrt(length(M1)))/sqrt((sumM1-meandiffmeasmod)/(length(M1)-1)))
  t2.5<- abs(qt(0.05/2,(length(M1))-2))  
  
  sigbi <- ifelse(tvalue>t2.5,"Yes-Bad","No-Good")
  moutput <-list("Mean Difference"=meandiffmeasmod,"Students's t of M"=tvalue,"t-value (Critical at 2.5%- Two tailed "=t2.5,"Significant bias?"=sigbi)
  
  Meanmeas <- mean(meas$Mean)
  
  difmeasmodsq <- (meas$Mean-mod$Mod)^2
  difmeasmodsqsum <- sum((meas$Mean-mod$Mod)^2)
  
  RMSE<-(100/Meanmeas)*sqrt(difmeasmodsqsum/length(meas$Mean))
  
  t95<- abs(qt(0.05/2,(meas$Replicates)-2))
  SEt95sq <- (meas$SEM*t95)^2
  sumSET95sq <- sum(SEt95sq)
  RMSE95<- (100/Meanmeas)*sqrt(sumSET95sq/length(SEt95sq)) 
  Siger <- ifelse(RMSE<RMSE95,"No-Good","Yes-Bad")
  
  rmsoutput <-list("Root measn square error of model"=RMSE,"RMSE (95% Confidence Limit)"=RMSE95,"Significant total error?"=Siger)
  
  t95<- abs(qt(0.05/2,(meas$Replicates)-2))
  Meanmeas <- mean(meas$Mean)
  diffmeasmod <- meas$Mean-mod$Mod
  sumdiffmeasmod <- sum(diffmeasmod)
  SET95<-meas$SEM*t95
  sumSET95 <- sum(SET95)
  
  E <-(100/Meanmeas)*sumdiffmeasmod/length(diffmeasmod)
  E95 <-(100/Meanmeas)*sumSET95/length(SET95) 
  Sigbi <-ifelse(E>E95,"Yes-Bad","No-Good")
  eoutput <-list("Relative Error"=E,"E (95% Confidence Limit)"=E95,"Significant bias?"=Sigbi)
  
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
  loutput <-list("Lack of Fit"=lofsum,"F=MSLOFIT/MSE"=Fvalue,"F (Critical at 5%"=FvalueatP,"Significant error between simulated and measured values?"=Siger)
  
  #ME=Maximum Error. Best = ABS(M)
  ME <- sqrt(max(difmeasmodsq))
  
  #RMSE*Obar/100
  RMSEobar<- RMSE*(Meanmeas/100)

  
  #Number of values
  No <- length(meas$Replicates)
  
  meobnoutput <- list("Maximum Error. Best = ABS(M)"=ME,"RMSE*Obar/100"=RMSEobar,"Number of values"=No)
  
  require(ggplot2)
  
  plot1 <- ggplot(meas, aes(meas$Mean ,mod$Mod)) + 
    geom_point(bg='yellow', pch=21, cex=3, lwd=3)+
    coord_flip()+ # is this neccsassary or could I swap them before?
    geom_smooth(method = "lm", se=FALSE, formula=y~x-1)+
    geom_errorbar(aes(ymin= mod$Mod-meas$SEM, ymax= mod$Mod+meas$SEM), width=.01)+
    xlab("Measured values (t C ha-1)")+
    ylab("Simulated values (t C ha-1)")+
    ylim(0,1)+
    xlim(0,1)
  
  plot2<- ggplot(meas, aes(ID$Name, meas$Mean))+     #Plot 2
    geom_point(bg='yellow', pch=21, cex=3, lwd=3)+
    ylim(0,1)+
    geom_errorbar(aes(ymin= meas$Mean-meas$SEM, ymax= meas$Mean+meas$SEM), width=1)+
    geom_line(aes(ID$Name, mod$Mod),size=1)+
    xlab("Sample")+
    ylab("Value")
  
  ploutput <- list(plot1,plot2)
  
  
  
  output <- list(routput,moutput,rmsoutput,eoutput,loutput,meobnoutput,ploutput)
  return(output)
  
  
    }



