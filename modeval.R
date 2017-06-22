rm(list=ls()) #empties the workspace
# Step 1: data import. Rows as relicates, calc mean, SE about measured mean, input modelled value.
meas<- read.csv("exinp.csv", header = TRUE) # read csv as in excel
means <- rowMeans(meas[,2:7]) #calculates means
meas["Means"] <-means

sem <- apply(meas[, 2:7], 1,function(x)sd(x)/sqrt(length(x))) #calculates standard error of the mean for reach row
meas["SEM"] <-sem
meas["Replicates"]<-ncol(meas[2:7])
meas["Mod"]<- read.csv("exmod.csv", header = TRUE) # read csv as in excel
meas
library(ggplot2)  #Plot 1
 ggplot(meas, aes(Means ,Mod)) + 
  geom_point(bg='yellow', pch=21, cex=3, lwd=3)+
   coord_flip()+ # is this neccsassary or could I swap them before?
   geom_smooth(method = "lm", se=FALSE, formula=y~x-1)+
   geom_errorbar(aes(ymin= Mod-SEM, ymax= Mod+SEM), width=.01)+
  xlab("Measured values (t C ha-1)")+
  ylab("Simulated values (t C ha-1)")+
   ylim(0,1)+
   xlim(0,1)
 
 ggplot(meas, aes(Name, Means))+     #Plot 2
  geom_point(bg='yellow', pch=21, cex=3, lwd=3)+
   ylim(0,1)+
   geom_errorbar(aes(ymin= Means-SEM, ymax= Means+SEM), width=1)+
   geom_line(aes(Name, Mod),size=1)+
   xlab("Sample")+
   ylab("Value")

 #LOFIT
 LOFIT <- (((meas$Means-meas$Mod)^2)*meas$Replicates)
 lofsum<- sum(LOFIT)
 LOFIT2 <- (((meas$Means-meas$Mod)^2)*meas$Replicates)*(meas$Replicates-1)
 LOFIT2
 
 #((Rep 1 - Mod)-(Means-mod))^2
 LO <- function(O,Oj,M)
   {
   ((O-M)-(Oj-M))^2
 }
 
 LOF<- mapply(LO,meas[2:7],meas[8],meas[11])#8 is meabs, 11 is mod
 
 Losum <- rowSums(LOF)
Losumsum <- sum(Losum)   
   

#F: 
sumlofit <- sum(LOFIT)
Fvalue <- sumlofit/(length(Losum)*Losumsum)
FvalueatP<- qf(0.05, (length(meas$Mod)-2),(length(meas[2:7])*(nrow(meas[2:7])))-2, lower.tail = FALSE)


#RMSE

Meanmeas <- mean(meas$Mean)

difmeasmodsq <- (meas$Means-meas$Mod)^2
difmeasmodsqsum <- sum((meas$Means-meas$Mod)^2)

RMSE<-(100/Meanmeas)*sqrt(difmeasmodsqsum/length(meas$Means))

t95<- abs(qt(0.05/2,(meas$Replicates)-2))
SEt95sq <- (meas$SEM*t95)^2
sumSET95sq <- sum(SEt95sq)
RMSE95<- (100/Meanmeas)*sqrt(sumSET95sq/length(SEt95sq))

#E
diffmeasmod <- meas$Means-meas$Mod
sumdiffmeasmod <- sum(diffmeasmod)
SET95<-meas$SEM*t95
sumSET95 <- sum(SET95)

E <-(100/Meanmeas)*sumdiffmeasmod/length(diffmeasmod)
E95 <-(100/Meanmeas)*sumSET95/length(SET95)


#M
meandiffmeasmod <- mean(diffmeasmod)
M1<- (diffmeasmod-meandiffmeasmod)^2
sumM1 <- sum(M1)
tvalue <-(meandiffmeasmod*(sqrt(length(M1)))/sqrt((sumM1-meandiffmeasmod)/(length(M1)-1)))
t2.5<- abs(qt(0.05/2,(length(M1))-2))



#r

meanmod <- mean(meas$Mod)
r1<- (meas$Mod-meanmod)
r1sq<- sum((meas$Mod-meanmod)^2)
r2 <- (meas$Means-Meanmeas)
r2sq <- sum((meas$Means-Meanmeas)^2)
r3<- r1*r2
r3sum <- sum(r1*r2)
r4 <- sqrt(r1sq)
r5 <- sqrt(r2sq)
r <- r3sum/(r4*r5)
rFvalue <- ((length(r3)-2)*r*r)/(1-r*r)
rFvaluep0.05<- abs(qt(0.05/2,(length(r3))-2)^2)



#Summary

#r= Correlation Coeff.
r
rFvalue
rFvaluep0.05
#signficant correlation
ifelse(rFvalue>rFvaluep0.05,"Yes-Good","No-Bad")

#RMSE= Root mean square error of model
RMSE
RMSE95
#Significant total error
ifelse(RMSE<RMSE95,"No-Good","Yes-Bad")

#M= Mean Difference

meandiffmeasmod
tvalue
t2.5
#Significant bias?
ifelse(tvalue>t2.5,"Yes-Bad","No-Good")

# E=Relative error
E
E95
ifelse(E>E95,"Yes-Bad","No-Good")



# LOFIT= Lack of Fit
lofsum
Fvalue
FvalueatP
ifelse(Fvalue>FvalueatP,"Yes-Bad","No-Good")
#ME=Maximum Error. Best = ABS(M)
ME <- sqrt(max(difmeasmodsq))
ME
#RMSE*Obar/100
RMSEobar<- RMSE*(Meanmeas/100)
RMSEobar

#Number of values
No <- length(meas$Replicates)
No

