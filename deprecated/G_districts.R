## x<-read.csv("seda_geodist_pool_cs_v30.csv",header=TRUE)
## x<-x[x$subgroup=='all',]
## seda<-x[,c("leaidC","mn_avg_ol")]
## names(seda)[1]<-'nces'

## ##

## load("df.Rdata")
## df<-merge(df,seda,all.x=TRUE)
## #ids<-unique(df$id[df$covid])
## #df<-df[df$id %in% ids,]
## id2020<-df$id[df$ay==2020 & df$covid]
## df<-df[df$id %in% unique(id2020),]

## L<-split(df,df$ay)
## for (i in 1:length(L)) {
##     y<-names(L)[i]
##     df<-L[[i]]
##     yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
## #print(yy)
##     df$del<-as.numeric(difftime(df$t,yy,units='days'))
##     L[[i]]<-df
## }
## df<-data.frame(do.call("rbind",L))

## ##
## df<-df[df$del<90,]
## tab<-table(df$nces[df$ay==2019])
## nms1<-names(tab)[tab>50]
## tab<-table(df$nces[df$ay==2020])
## nms2<-names(tab)[tab>50]
## df<-df[df$nces %in% intersect(nms1,nms2),]

## #z<-df[!duplicated(df$nces),]


## ff<-function(df) {
##     f<-function(df,y,maxtime=90) {
##         yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
##         #print(yy)
##         df$del<-difftime(df$t,yy,units='days')
##         df<-df[df$del>0 & df$del<(maxtime),]
##         ######################################
##         library(lfe)
##         df$del<-as.numeric(df$del)
##         m<-felm(score~del|title+id,df)
##         c(as.numeric(t(summary(m)$coef[,1:2])),nrow(m$resid))
##     }
##     x2019<-f(2019,df=df)
##     x2020<-f(2020,df=df)
##     l<-list(`2019`=x2019,`2020`=x2020)
##     l
## }
## L<-split(df,df$nces)
## L<-lapply(L,ff)

## d<-data.frame(nces=names(L),gr=sapply(L,function(x) x[[2]][[1]]))
## tmp<-merge(seda,d)
