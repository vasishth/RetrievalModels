# specify parameters of interest and save to concatenate all later.

# stan results function
stan_results<-function(m=m,params=NULL){
  m_extr<-rstan::extract(m,pars=params)
  means<-lapply(m_extr,mean)
  quants<-lapply(m_extr,function(x)quantile(x,probs=c(0.025,0.975)))
  means<-data.frame(means)
  quants<-data.frame(quants)
  smry<-cbind(means=t(means),quants=t(quants))
  colnames(smry)<-c("mean","lower","upper")
  smry
}

