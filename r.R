

r <- function(meas,mod) {
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
  output <-list("r"=r,"F value"=rFvalue,"F value at p=0.05"=rFvaluep0.05,"Signficant association? "=sigas)
  return(output)
  
}

