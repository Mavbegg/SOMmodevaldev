

modplot<- function(meas,mod) {
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
   
  output <- list(plot1,plot2)
 return(output)  

   }


