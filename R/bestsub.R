bestsub<-function(x,y){
  library(combinat)
  crit2<-matrix(0)
  p<-dim(x)[2]
  subsets<-matrix(0,p,p)
  min_crit1<-matrix(0)

  for(i in 1:p){
    crit1<-matrix(0)
    k<-i
    n<-choose(p,k)
    m<-as.matrix(combn(p,k))
    #RSS (Deviance)
    for(j in 1:n){
      predic1<-x[,c(m[,j])]
      data1<-data.frame(y,predic1)
      mod1<-glm(y~.,data=data1)                     #glm/lda/qda
      crit1[j]<-summary(mod1)$deviance	            #deviance/R^2
    }
    min<-which.min(crit1)
    min_crit1[i]<-crit1[min]
    Mk<-m[,min]
    subsets[i,]<-c(Mk,rep(0,p-length(Mk)))
    #AIC
    predic2<-x[,Mk]
    data2<-data.frame(y,predic2)
    mod2<-glm(y~.,data=data2)
    crit2[i]<-summary(mod2)$aic

  }

  result<-data.frame(min_crit1,crit2,subsets)
  colnames(result)<-c("RSS","AIC",rep("*",p))
  return(result)
}

