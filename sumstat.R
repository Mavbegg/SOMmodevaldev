source('import.R')
#calculate  mean,SE about measured mean
sumstat <-  function(meas)
{

meas["Mean"] <- rowMeans(meas)  

meas["SEM"]<- apply(meas,1,function(meas)sd(meas)/sqrt(length(meas)))
meas["Replicates"]<- ncol(meas[!names(meas) %in% c("Mean","SEM")])

return(meas)
}





                  
