## ####interaction
## f<-function(df,y,maxtime=90) {
##     yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
##     print(yy)
##     df$del<-difftime(df$t,yy,units='days')
##     df<-df[df$del>0 & df$del<(maxtime),]
##     ######################################
##     library(lfe)
##     df$del<-as.numeric(df$del)
##     m<-felm(score~del+del:mm|id+title,df)
##     S<-summary(m)$coef
##     S
## }

## out<-list()
## for (y in 2019:2020) {
##     load("df.Rdata")
##     x<-df[df$ay==y-1 & !df$covid,]
##     M<-by(x$score,x$id,mean,na.rm=TRUE)
##     M<-data.frame(id=names(M),mm=as.numeric(M))
##     df<-merge(df[df$ay==y,],M)
##     est<-list()
##     for (g in 1:4) {
##         z<-df[df$grade==g & df$ay==y,]
##         z<-f(z,y=y)
##         est[[g]]<-z
##     }
##     out[[as.character(y)]]<-est
## }

