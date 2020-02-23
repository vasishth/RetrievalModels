plotresults<-function(dat,maintitle="Expt 1 \n (critical region)",ylabel="Reading time (ms)",
                      cols=c("red", "black","black"),removelegend=FALSE,legendposition=c(0.73,0.78),lowery=-100,uppery=200,xaxisblank=FALSE){
  pd<-position_dodge(0.6)
  
  #  decorate<-theme(axis.text=element_text(size=14),
  #                  axis.title=element_text(size=14,face="bold"))
  
  resultsplot<-ggplot(dat, aes(x=cond, 
                               y=mean, shape=expt, 
                               group=expt,colour=expt)) +
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.25, size=.5, position=pd) +
    labs(title=maintitle) +
    xlab("Predictor")+
    ylab(ylabel)+
    geom_hline(yintercept=0)+
    geom_point(position=pd, size=2)+
    theme_bw()+
    magnifytext()+ylim(lowery,uppery)
  
  if(xaxisblank){
    resultsplot<-resultsplot+theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
  }
  
  if(removelegend){
  resultsplot+ scale_color_manual(values=cols)+ scale_shape_manual(values=c(0,1,2,4))+theme(legend.position="none")
  } else{
    resultsplot+ scale_color_manual(values=cols)+ scale_shape_manual(values=c(0,1,2,4)) +theme(legend.position=legendposition) 
  }
}
