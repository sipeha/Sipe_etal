library(coda)
library(data.table)

setwd("C:/Users/Hannah Sipe/Desktop/")


out1<-(as.data.frame(fread(file="June2122222", sep=",", quote='"',
                           header=T, data.table=T)[,]))

out2<-(as.data.frame(fread(file="June2133333", sep=",", quote='"',
                           header=T, data.table=T)[,]))
out3<-(as.data.frame(fread(file="June2144444", sep=",", quote='"',
                           header=T, data.table=T)[,]))

out<-rbind(out1, out2, out3)

##################
#posterior figures for all parameters
#################
library(ggplot2)
library(bayesplot)
library(cowplot)

######Occupancy Parameters
mm1<-mcmc_areas(out, pars=c("alpha"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(a) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mm2<-mcmc_areas(out, pars=c("gamma.focal"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(b) Focal")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mm3<-mcmc_areas(out, pars=c("beta.hii"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(c) HII")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mm4<-mcmc_areas(out, pars=c("beta.elev"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(d) Elevation")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mm5<-mcmc_areas(out, pars=c("beta.canopy"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(e) Canopy")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mm6<-mcmc_areas(out, pars=c("beta.pc"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(g) Perimeter Complexity")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mm7<-mcmc_areas(out, pars=c("beta.area"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(h) Area")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

row<-plot_grid(mm1,mm2, mm3, mm4, mm5, mm6, mm7,ncol=2,nrow=4)
plot_grid(row,rel_heights = c(0.1, 1))

mm9<-mcmc_areas(out, pars=c("beta.11"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(a) Forest")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mm10<-mcmc_areas(out, pars=c("beta.2"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(b) Developed + Cultivated")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mm11<-mcmc_areas(out, pars=c("beta.31"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(c) Other")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mm12<-mcmc_areas(out, pars=c("alpha"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(d) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

row<-plot_grid(mm9,mm10, mm11, mm12, ncol=1,nrow=4)
plot_grid(row,rel_heights = c(0.1, 1))
##########reproduction parameters
mr1<-mcmc_areas(out, pars=c("mu"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(a) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mr3<-mcmc_areas(out, pars=c("theta.hii"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(c) HII")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mr4<-mcmc_areas(out, pars=c("theta.elev"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(d) Elevation")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mr5<-mcmc_areas(out, pars=c("theta.canopy"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(e) Canopy")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mr6<-mcmc_areas(out, pars=c("theta.pc"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(g) Perimeter Complexity")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))
mr7<-mcmc_areas(out, pars=c("theta.area"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(h) Area")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

row<-plot_grid(mr1,mr3, mr4, mr5, mr6, mr7,ncol=2,nrow=4)
plot_grid(row,rel_heights = c(0.1, 1))

mr9<-mcmc_areas(out, pars=c("theta.11"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(a) Forest")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mr10<-mcmc_areas(out, pars=c("theta.2"),
                 prob=0.5,
                 prob_outer = 0.99,
                 point_est = "median")+
  ggplot2::labs(title = "(b) Developed + Cultivated")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mr11<-mcmc_areas(out, pars=c("theta.31"),
                 prob=0.5,
                 prob_outer = 0.99,
                 point_est = "median")+
  ggplot2::labs(title = "(c) Other")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mr12<-mcmc_areas(out, pars=c("mu"),
                 prob=0.5,
                 prob_outer = 0.99,
                 point_est = "median")+
  ggplot2::labs(title = "(d) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

row<-plot_grid(mr9,mr10, mr11, mr12, ncol=1,nrow=4)
plot_grid(row,rel_heights = c(0.1, 1))

######################
#eBird detection Parameters
me1<-mcmc_areas(out, pars=c("rho.1"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(a) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me2<-mcmc_areas(out, pars=c("eta1.dist"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(b) Distance Travelled")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me3<-mcmc_areas(out, pars=c("eta1.area"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(c) Area")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me4<-mcmc_areas(out, pars=c("eta1.dur"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(d) Survey Duration")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me5<-mcmc_areas(out, pars=c("eta1.start"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(e) Time")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me6<-mcmc_areas(out, pars=c("rho.2"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(f) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me7<-mcmc_areas(out, pars=c("eta2.dist"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(g) Distance Travelled")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me8<-mcmc_areas(out, pars=c("eta2.area"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(h) Area")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me9<-mcmc_areas(out, pars=c("eta2.dur"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(i) Survey Duration")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

me10<-mcmc_areas(out, pars=c("eta2.start"),
                 prob=0.5,
                 prob_outer = 0.99,
                 point_est = "median")+
  ggplot2::labs(title = "(j) Time")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

row<-plot_grid(me1,me6,me2,me7, me3,  me8,me4, me9,me5, me10,ncol=2,nrow=5)
plot_grid(row,rel_heights = c(0.1, 1))

#################
#WDFW Detection Parameters
mw1<-mcmc_areas(out, pars=c("rho.1"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(a) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw2<-mcmc_areas(out, pars=c("lambda.survey1"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(b) Survey")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw3<-mcmc_areas(out, pars=c("omega1.area"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(c) Area")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw4<-mcmc_areas(out, pars=c("omega1.dur"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(d) Survey Duration")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw5<-mcmc_areas(out, pars=c("omega1.start"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(e) Time")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw6<-mcmc_areas(out, pars=c("rho.2"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(f) Intercept")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw7<-mcmc_areas(out, pars=c("lambda.survey2"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(g) Survey")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw8<-mcmc_areas(out, pars=c("omega2.area"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(h) Area")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw9<-mcmc_areas(out, pars=c("omega2.dur"),
                prob=0.5,
                prob_outer = 0.99,
                point_est = "median")+
  ggplot2::labs(title = "(i) Survey Duration")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

mw10<-mcmc_areas(out, pars=c("omega2.start"),
                 prob=0.5,
                 prob_outer = 0.99,
                 point_est = "median")+
  ggplot2::labs(title = "(j) Time")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggplot2::theme(axis.text.x = element_text(size=15))

row<-plot_grid(mw1,mw6,mw2,mw7, mw3,  mw8,mw4, mw9,mw5,   mw10,ncol=2,nrow=5)
plot_grid(row,rel_heights = c(0.1, 1))

#########################For Parameter Posterior Table
psiests<-((out[,grep("^beta.", colnames(out))]))
psiests<-cbind(psiests,out[,grep("alpha", colnames(out))])
psiests<-cbind(psiests,out[,grep("gamma.focal", colnames(out))])
colnames(psiests)[11:12]<-c("alpha", "gamma.focal")
psiests<-cbind(psiests, out[,grep("^sd", colnames(out))])
colnames(psiests)
psiets<-summary(as.mcmc(psiests))
psiout<-matrix(nrow=13,ncol=8)
rownames(psiout)<-colnames(psiests)
psiout[,1:2]<-psiets[[1]][,1:2]
psiout[,3:7]<-psiets[[2]]
psiout[,8]<-rep(1,13)
colnames(psiout)<-c("mean","sd","0.025","0.25", "0.5","0.75", "0.975","")

Rests<-out[,grep("^theta.", colnames(out))]
Rests<-cbind(Rests, out[,grep("mu", colnames(out))])
colnames(Rests)[10]<-c("mu")
colnames(Rests)
Rets<-summary(as.mcmc(Rests))
Rout<-matrix(nrow=10, ncol=8)
rownames(Rout)<-colnames(Rests)
colnames(Rout)<-c("mean","sd","0.025","0.25", "0.5","0.75", "0.975","")
Rout[,1:2]<-Rets[[1]][,1:2]
Rout[,3:7]<-Rets[[2]]
Rout[,8]<-rep(2,10)
View(Rout)

ebdet<-out[,grep("^eta.", colnames(out))]
ebdet<-cbind(ebdet,out[,grep("rho.1", colnames(out))])
ebdet<-cbind(ebdet,out[,grep("rho.2", colnames(out))])
colnames(ebdet)[9:10]<-c("rho.1", "rho.2")
colnames(ebdet)
ebets<-summary(as.mcmc(ebdet))
ebout<-matrix(nrow=10, ncol=8)             
colnames(ebout)<-c("mean","sd","0.025","0.25", "0.5","0.75", "0.975","")
rownames(ebout)<-colnames(ebdet)
ebout[,1:2]<-ebets[[1]][,1:2]
ebout[,3:7]<-ebets[[2]]
ebout[,8]<-rep(3,10)

wdet<-out[,grep("^omega.", colnames(out))]
wdet<-cbind(wdet,out[,grep("rho.1", colnames(out))])
wdet<-cbind(wdet,out[,grep("rho.2", colnames(out))])
wdet<-cbind(wdet,out[,grep("lambda.survey1", colnames(out))][1])
wdet<-cbind(wdet,out[,grep("lambda.survey2", colnames(out))][1])

colnames(wdet)[7:10]<-c("rho.1", "rho.2", "lambda.survey1","lambda.survey2")
colnames(wdet)
wets<-summary(as.mcmc(wdet))
wout<-matrix(nrow=10, ncol=8)             
colnames(wout)<-c("mean","sd","0.025","0.25", "0.5","0.75", "0.975","")
rownames(wout)<-colnames(wdet)
wout[,1:2]<-wets[[1]][,1:2]
wout[,3:7]<-wets[[2]]
wout[,8]<-rep(4,10)


del<-summary(as.mcmc(out[,grep("delta", colnames(out))]))
delout<-numeric(8)
delout[1:2]<-del[[1]][1:2]
delout[3:7]<-del[[2]]
delout[8]<-4
delout


#allests_VS<-rbind(psiout,Rout,ebout, wout, delout)

#write.csv(allests_VS, file="post_all_VS_731.csv")
