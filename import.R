# data import. Rows as relicates, calc mean, SE about measured mean, input modelled value.

# Leave import to user, instruct to store measuresed samples in 'meas', modelled in 'mod'. Samples in rows, replicates as colums. See example data.ID as well
measured<- read.csv("exinp.csv", header = TRUE) # read csv as in excel
mod<- read.csv("exmod.csv", header = TRUE) # read csv as in excel
meas<- measured[2:7]
ID <- measured[1]
