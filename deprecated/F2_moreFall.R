## ##will now look at relatively low achievers in 2019

## f<-function(df,y,maxtime=90) {
##     yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
##     print(yy)
##     df$del<-difftime(df$t,yy,units='days')
##     df<-df[df$del>0 & df$del<(maxtime),]
##     ######################################
##     library(lfe)
##     df$del<-as.numeric(df$del)
##     m<-felm(score~del|title+id,df)
##     c(as.numeric(t(summary(m)$coef[,1:2])))#,nrow(m$resid))
## }

## load("df.Rdata")
## df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))


## L<-list()
## for (gr in 1:4) {
##     tmp<-df[df$grade==gr,]
##     x2019<-f(2019,df=tmp)
##     x2020<-f(2020,df=tmp)
##     ##
##     tmp.lastyear<-df[df$grade==gr-1,]
##     z<-tmp.lastyear[tmp.lastyear$ay==2018 & tmp.lastyear$m %in% 3:5,]
##     yy<-strptime(paste("2018-09-01",sep=""),format="%Y-%m-%d")
##     z$del<-as.numeric(difftime(z$t,yy,units='days'))
##     m<-felm(as.formula(score~del|title),z)
##     z$score<-z$score - coef(m)*z$del
##     sc<-by(z$score,z$id,mean,na.rm=TRUE)
##     ids<-names(sc)[sc<mean(sc)]
##     x2019b<-f(2019,df=tmp[tmp$id %in% ids,])
##     ##
##     L[[as.character(gr)]]<-c(x2019,x2019b,x2020)
## }
## tab<-do.call("rbind",L)

## library(xtable)
## print(xtable(tab,digits=3,display=c("d",rep("f",4))
##              ),include.rownames=TRUE)
