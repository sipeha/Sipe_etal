library(coda)
library(data.table)

#load in MCMC model outout
# out1<-(as.data.frame(fread(file="VS1", sep=",", quote='"',
#                            header=T, data.table=T)[,]))
# 
# out2<-(as.data.frame(fread(file="VS2", sep=",", quote='"',
#                            header=T, data.table=T)[,]))
# 


x11<-as.mcmc(out1[,-1]) #remove first column which is the index from 1:nsamples
x22<-as.mcmc(out2[,-1])
out.mcmc <- as.mcmc.list(list(x11,x22))
v<-gelman.diag(out.mcmc, multivariate = T, transform = F)
v



