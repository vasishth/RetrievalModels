plotmeanSE<-function(d=Match,title="Target Match"){
  d$Publication<-factor(d$Publication,
                        levels=d$Publication[order(d$Effect)])
  
  df <- data.frame(x = d$Publication,
                   y = d$Effect,
                   lower = d$Effect-2*d$SE,
                   upper = d$Effect+2*d$SE)
  
  df$x <- factor(df$x,levels=df$x[order(df$y)])
  

  pd <- position_dodge(.5)
  p<-ggplot(data=df,
                 aes(x=x,y=y))+
    geom_point(position=pd, size=3, fill="white") +
    geom_errorbar(data=df,aes(ymin=lower,
                                         ymax=upper), 
                  width=.1, position=pd)+
    scale_colour_manual(values=c("black"))+
    #  scale_shape_manual(values=c(1,2))+
    geom_hline(yintercept=0,linetype="dashed")+
    ylab("Interference effect (ms)")+
    xlab("Study")+ggtitle(title)+
    #theme(legend.title=element_blank())+
    ## don't display posterior of theta:
    #    annotate("pointrange", x = 67.2, y = post[2], ymin = post[1], ymax = post[3],colour = "red", size = .1)+
    #    annotation_custom(g, ymin=-100, ymax=-50, xmin=64, xmax=65) +
    coord_flip()  + theme_bw()+theme(legend.title=element_blank())+  scale_shape_manual(values=c(17)) 
  
  p
}
