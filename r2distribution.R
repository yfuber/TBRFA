model_cor<-function(xlsxname){
  index=c()
  r2=c()
  type=c()
  setwd('E:/学习/硕士/免疫/2021/imm')
  for (i in 1:12){
    cor<-read.xlsx(xlsxname,i)
    index<-append(index,rep(label1[i],20))
    r2<-append(r2,cor$train[1:10])
    r2<-append(r2,cor$test[1:10])
    type<-append(type,rep(c('train','test'),each=10))
  }
  setwd('E:/学习/硕士/免疫/2021/burden')
  for (i in 1:3){
    cor<-read.xlsx(xlsxname,i)
    index<-append(index,rep(label2[i],20))
    r2<-append(r2,cor$train[1:10])
    r2<-append(r2,cor$test[1:10])
    type<-append(type,rep(c('train','test'),each=10))
  }
  frame<-data.frame(index=index,r2=r2,type=type)
  return(frame)
}

cor_rmse_output<-function(xlsxname){
  for (i in 1:15){
    if (i<=12) {
      setwd('E:/学习/硕士/免疫/2021/imm')
      cor<-read.xlsx(xlsxname,i)
    } else {
      setwd('E:/学习/硕士/免疫/2021/burden')
      cor<-read.xlsx(xlsxname,i-12)
    }
    print(paste0(label[i],'-r2_train:',round(mean(cor$train[1:10]),3),'±',round(sd(cor$train[1:10]),3)))
    print(paste0(label[i],'-r2_test:',round(mean(cor$test[1:10]),3),'±',round(sd(cor$test[1:10]),3)))
    print(paste0(label[i],'-rmse_train:',round(mean(cor$rmse_train[1:10]),3),'±',round(sd(cor$rmse_train[1:10]),3)))
    print(paste0(label[i],'-rmse_test:',round(mean(cor$rmse_test[1:10]),3),'±',round(sd(cor$rmse_test[1:10]),3)))
  }
}




model_cor_all<-function(xlsxname){
  index=c()
  r2=c()
  type=c()
  for (i in 1:15){
    cor<-read.xlsx(xlsxname,i)
    index<-append(index,rep(label[i],20))
    r2<-append(r2,cor$train)
    r2<-append(r2,cor$test)
    type<-append(type,rep(c('train','test'),each=10))
  }
  frame<-data.frame(index=index,r2=r2,type=type)
  return(frame)
}

r2_dis<-function(corframe,pdfname,max=1){
  corframe$index<-factor(corframe$index,level=unique(corframe$index))
  
  p<-ggplot(corframe, aes(x=index,y=r2,fill=type))+
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.7)+
    geom_pointrange(stat="summary", fun.data="mean_sdl",fun.args = list(mult=1),
                    color = "black",size = 1.2,alpha=0.8)+
    geom_point(stat="summary", fun.y="mean",fun.args = list(mult=1),
               color = "white",size = 4,alpha=1)+
    theme_classic()+
    theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
          axis.line=element_line(colour="black",size=0.25),
          axis.title=element_text(size=13,face="plain",color="black"),
          axis.text = element_text(size=12,face="plain",color="black"),
          axis.ticks.length = unit(0.4,'line'),
          legend.position="none")+
    scale_y_continuous(limits=c(0,max))
  ggsave(paste0('E:/学习/硕士/免疫/2021/plot/R2',pdfname,'.pdf'),width=19.2,height = 5.4)
  return(p)
}