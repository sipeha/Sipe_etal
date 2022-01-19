library(coda)
library(data.table)

#load in MCMC model outout
# out1<-(as.data.frame(fread(file="VS1", sep=",", quote='"',
#                            header=T, data.table=T)[,]))
# 
# out2<-(as.data.frame(fread(file="VS2", sep=",", quote='"',
#                            header=T, data.table=T)[,]))
# 
# out<-rbind(out1, out2)

#read in the covariate data
#can be found in:
#SitesandCovs.csv
sitecovs<-read.csv("data/SitesandCovs.csv", header=T)

#site covs and data for those sites not in the model

#set constants
N<-766
N2<-2324 #all sites 
T<-18 #total number of years

####Scale and center the covariates from the model
elev_mN<-scale(sitecovs$Elevation_m[1:N])
hiiN<-scale(sitecovs$HII[1:N])
pcN<-scale(sitecovs$Perimeter.Complexity[1:N])
areaN<-scale(sitecovs$AreaSKM[1:N])
canopyN<-scale(sitecovs$Percent.Canopy.Cover[1:N])

#Scale and center the covariates both in the model and not, using the same scale and center
#as the covariates from model fitting
hii<-scale(sitecovs$HII, center=attributes(hiiN)$`scaled:center`, 
           scale=attributes(hiiN)$`scaled:scale`)
area<-scale(sitecovs$AreaSKM, center=attributes(areaN)$`scaled:center`,
            scale=attributes(areaN)$`scaled:scale`)
elev<-scale(sitecovs$Elevation_m, center=attributes(elev_mN)$`scaled:center`,
            scale=attributes(elev_mN)$`scaled:scale`)
pc<-scale(sitecovs$Perimeter.Complexity, 
          center=attributes(pcN)$`scaled:center`,
          scale=attributes(pcN)$`scaled:scale`)
canopy<-scale(sitecovs$Percent.Canopy.Cover, center=attributes(canopyN)$`scaled:center`,
              scale=attributes(canopyN)$`scaled:scale`)

#######################
##################################################################
##################################################################
#predictions for sites from model

#form covariate matrix
X<-matrix(nrow=N2, ncol=9)
X[,1]<-rep(1, N2)#alpha
X[,2]<-elev
X[,3]<-hii
X[,4]<-pc
X[,5]<-area
X[,6]<-canopy
X[,7]<-sitecovs$Forest.Type
X[,8]<-sitecovs$Developed.Type
X[,9]<-sitecovs$Other.Type



#grab the estimates of random site effects on psi from the mcmc data
epsN<-matrix(nrow=N)
epsiN<-matrix(nrow=N, ncol=length(out$V1))
for(i in 1:N){
  a1<-paste("epsilon_i[", i, sep="")
  a2<-paste(a1, "]", sep="")
  epsN[i]<-a2
  epsiN[i,]<-unlist(out[epsN[i]])
}

#grab the estimates of random year effects on psi from the mcmc data
epsT<-matrix(nrow=T)
epsiT<-matrix(nrow=T, ncol=length(out$V1))
for(i in 1:T){
  a1<-paste("epsilon_t[", i, sep="")
  a2<-paste(a1, "]", sep="")
  epsT[i]<-a2
  epsiT[i,]<-unlist(out[epsT[i]])
}
#matrix of psi coefficient estimates from mcmc sampling
Betamat<-matrix(nrow=length(out$V1),ncol=9)
Bmat[,1]<-out$alpha
Bmat[,2]<-out$beta.elev
Bmat[,3]<-out$beta.hii
Bmat[,4]<-out$beta.pc
Bmat[,5]<-out$beta.area
Bmat[,6]<-out$beta.canopy
Bmat[,7]<-out$beta.11
Bmat[,8]<-out$beta.2
Bmat[,9]<-out$beta.31

#matrix of R estimates from mcmc sampling
Rmat<-matrix(nrow=length(out$V1),ncol=9)
Rmat[,1]<-out$mu
Rmat[,2]<-out$theta.elev
Rmat[,3]<-out$theta.hii
Rmat[,4]<-out$theta.pc
Rmat[,5]<-out$theta.area
Rmat[,6]<-out$theta.canopy
Rmat[,7]<-out$theta.11
Rmat[,8]<-out$theta.2
Rmat[,9]<-out$theta.31


#Grab the estimated occupancy state for each site, at time in each iteration of mcmc
zzNR<-matrix(nrow=N, ncol=T)
ZZ<-array(dim=c(N,T,length(out$V1)))
for(i in 1:N){
  for(t in 1:T){
    zn<-paste("z[", i, sep="")
    znt<-paste(zn, t, sep=", ")
    zNR<-paste(znt, "]", sep="")
    zzNR[i,t]<-zNR
    ZZ[i,t,]<-unlist(out[zzNR[i,t]])
  }
}


#take z and make it a 1 if occupied, to use for the focal effect
zpsi<-ZZ
for(i in 1:N){
  for(t in 1:T){
    for(n in 1:length(out$V1)){
      if(zpsi[i,t,n]==1){zpsi[i,t,n]<-0}else{zpsi[i,t,n]<-1}
    }
  }
}

#use all of the above to predict back to each site in each year, for psi and R, for
#each sample from mcmc output
psia<-array(dim=c(N,T,length(out$V1)))
Ra<-matrix(nrow=N,ncol=length(out$V1))
gammafoc<-out$gamma.focal
R1means<-colMeans(plogis(R1))
for(i in 1:N){
  psia[i,1,]<-(t(X[i,1:9])%*%t(Bmat[,1:9])+epsiN[i,]+epsiT[1,])[1,]
  Ra[i,]<-(t(X[i,1:9])%*%t(Rmat[,1:9]))[1,]
  for(t in 2:18){
    psia[i,t,]<-(t(X[i,1:9])%*%t(Bmat[,1:9])+epsiN[i,]+epsiT[t,]+(zpsi[i,t,]*gammafoc))[1,]
  }
}

#summarize the estimates, mean, 0.025, 0.5,0.975
psiamean<-matrix(nrow=N, ncol=T)
Ramean<-psisiteonly<-numeric(nrow=N)
for(i in 1:N){
  for(t in 1:T){
    psiamean[i,t]<-mean(plogis(psia[i,t,]))
  }
}

for(i in 1:N){
  Ramean[i]<-mean(plogis(Ra[i,]))
  psisiteonly[i]<-mean(psiamean[i,])
}



#occupancy states by site across years
state1<-state2<-state3<-numeric(N)
for(i in 1:N){
  state1[i]<-(1-psisiteonly[i])
  state2[i]<-psisiteonly[i]*(1-Ramean[i])
  state3[i]<-psisiteonly[i]*Ramean[i]
}


#predict psi to all sites based on covariate relationships
xb2<-Bmat%*%t(X2)
plot(xbmean2)
xbmeanP2<-colMeans(plogis(xb2)) 

#######predict R to all sites

xRb2<-Rmat%*%t(X)
xRbmean2<-colMeans(xRb2)
xRbmeanP2<-colMeans(plogis(xRb2))


#########
library(ggplot2)
library(cowplot)

########Figure 1 - Occupancy and reproduction for sites in the model
#with overall for all sites mean 
nx<-c(rep(1,N),rep(2,N))
allsi<-c(psisiteonly, Ramean)
alldf<-data.frame((allsi), nx=nx)


ny<-c(rep(1,N2), rep(2,N2))
allests<-c(xbmeanP2, xRbmeanP2)
allestsdf<-data.frame((allests), ny=ny)


#for Figure 1
p1<-ggplot(alldf,  aes(factor(nx), as.numeric(allsi)))+
  ggplot2::geom_violin(scale="width", trim=T, adjust=1, aes(fill=factor(nx)))+
  ggplot2::theme(legend.position = "none")+
  ggplot2::labs(
    title = "(a)")+
  ggplot2::scale_x_discrete(name="", labels = c(expression(paste("Mean ",Psi, sep="")), "Mean R"))+
  ggplot2::scale_y_continuous(name="Probability")+
  ggplot2::theme(axis.text=element_text(size=15, color=
                                          "black", family="Helvetica serif"),
                 axis.title = element_text(size=15, color="black", family="Helvetica serif"), 
                 plot.background = element_rect(fill="white"),
                 panel.background = element_rect(fill="white", colour="black"),
                 panel.grid.major = element_line(color="grey"))

p12<-ggplot(allestsdf,  aes(factor(ny), as.numeric(allests)))+
  ggplot2::geom_violin(scale="width", trim=T, adjust=1, aes(fill=factor(ny)))+
  ggplot2::theme(legend.position = "none")+
  ggplot2::labs(
    title = "(b)")+
  ggplot2::scale_x_discrete(name="", labels = c(expression(paste("Mean ",Psi, sep="")), "Mean R"))+
  ggplot2::scale_y_continuous(name="Probability")+
  ggplot2::theme(axis.text=element_text(size=15, color=
                                          "black", family="Hel"),
                 axis.title = element_text(size=15, color="black", family="Hel"), 
                 plot.background = element_rect(fill="white"),
                 panel.background = element_rect(fill="white", colour="black"),
                 panel.grid.major = element_line(color="grey"))



rowS1<-plot_grid(p1,p12,nrow=1,ncol=2)
plot_grid(rowS1,rel_heights = c(0.1, 1))

#For Figure 4
st<-cbind(state1, state2,state3)
nst<-c(rep(1,N), rep(2,N), rep(3,N))
stsdf<-data.frame(st=as.numeric(st), nst=nst)

p5<-ggplot(stsdf,  aes(factor(nst), (st)))+
  ggplot2::geom_violin(scale="width", trim=T, adjust=1, aes(fill=factor(nst)))+
  ggplot2::theme(legend.position = "none")+
  ggplot2::labs(
    title = "")+
  ggplot2::scale_x_discrete(name="", labels = c(expression(paste("1-", Psi)),
                                                expression(paste(Psi,"(1-R)")),expression(paste(Psi,"R"))))+
  ggplot2::scale_y_continuous(name="Probability")+
  ggplot2::theme(axis.text=element_text(size=15, color=
                                          "black"),
                 axis.title = element_text(size=15, color="black"), 
                 plot.background = element_rect(fill="white"),
                 panel.background = element_rect(fill="white", colour="black"),
                 panel.grid.major = element_line(color="grey"))


