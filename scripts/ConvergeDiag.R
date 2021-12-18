library(coda)
library(data.table)

setwd("C:/Users/Hannah Sipe/Desktop/")

out2<-(as.data.frame(fread(file="June2122222", sep=",", quote='"',
                           header=T, data.table=T)[,]))
out3<-(as.data.frame(fread(file="June2133333", sep=",", quote='"',
                           header=T, data.table=T)[,]))


x22<-as.mcmc(out2[,-1])
x33<-as.mcmc(out3[,-1])
out.mcmc <- as.mcmc.list(list(x22,x33))
v<-gelman.diag(out.mcmc, multivariate = T, transform = F)
v

acd<-autocorr.diag(out.mcmc)
pdf("VS_acf_81.pdf")
autocorr.plot(out.mcmc)
dev.off()

