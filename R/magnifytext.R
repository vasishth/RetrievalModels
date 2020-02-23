magnifytext<-function(sze=12){
  theme(plot.title = element_text(lineheight=.8, 
                                  size=sze,
                                  face="bold"))+
    theme(axis.text=element_text(size=sze),
          axis.title=element_text(size=sze,face="bold"))+
    theme(legend.text = element_text(colour="black", 
                                     size = sze, 
                                     face = "bold"))+
    theme(legend.title = element_text(colour="black", 
                                      size = sze, 
                                      face = "bold"))
}
