## f<-function(df,y,maxtime=90) {
##     yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
##     print(yy)
##     df$del<-difftime(df$t,yy,units='days')
##     df<-df[df$del>0 & df$del<(maxtime),]
##     ######################################
##     library(lfe)
##     df$del<-as.numeric(df$del)
##     m<-felm(score~del|title+id,df)
##     coef(m)
## }

## load("df.Rdata")
## df<-df[df$ay==2020 & df$grade %in% 1:4,]

## L<-split(df,df$nces)
## n<-sapply(L,nrow)
## L<-L[n>100]

## est<-numeric()
## for (i in 1:length(L)) est[i]<-f(L[[i]],y=2020)
## est<-data.frame(leaid=names(L),est=est)
## ##

## load("geo.Rdata")
## z<-geo[geo$month %in% 9:12,]

## L<-split(z,z$leaid)
## f<-function(x) {
##     m50<-mean(x$share_elem_closed_50,na.rm=TRUE)
##     m25<-mean(x$share_elem_closed_25,na.rm=TRUE)
##     sm<-mean(x$seda.mean,na.rm=TRUE)
##     c(leaid=unique(x$leaid),m50=m50,m25=m25,sm=sm)
## }
## L<-lapply(L,f)
## geo2<-data.frame(do.call("rbind",L))
## ##

## x<-merge(est,geo2)
## cor(x[,-1])
