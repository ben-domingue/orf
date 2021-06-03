


## ##################################
## library(lfe)
## bigfun<-function(df,grade,months=9:11) {
##     df<-df[df$m %in% 9:11,] #this just gets all scores in fall
##     df<-df[df$grade==grade,]
##     ############################
##     ## residfun<-function(df) {
##     ##     y<-unique(df$ay)
##     ##     yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
##     ##     df$del<-as.numeric(difftime(df$t,yy,units='days'))
##     ##     m<-felm(as.formula(score~del|title),df[df$del>0,])
##     ##     df$score2<-df$score - coef(m)*df$del
##     ##     df
##     ## }
##     ## lll<-split(df,df$ay)
##     ## lll<-lapply(lll,residfun)
##     ## df<-data.frame(do.call("rbind",lll))
##     df$score2<-df$score
##     ############################
##     ##static deciles
##     ## z<-df[df$ay==2019 & df$m %in% months,]
##     ## s<-by(z$score2,z$id,mean)
##     ## deciles<-quantile(s,(1:9)/10)
##     ## ##
##     ## ##get rid of tmp.id s<-by(df$score2,df$tmp.id,mean)
##     ## ##get rid of tmp.id tmp<-data.frame(tmp.id=names(s),sc=as.numeric(s))
##     ## tmp$q.all<-cut(tmp$sc,c(-Inf,deciles,Inf),labels=1:10)
##     ## tmp$s<-NULL
##     ## df<-merge(df,tmp,all.x=TRUE)
##     ##
##     getq<-function(df,months) {
##         z<-df[df$m %in% months,]
##         s<-by(z$score2,z$id,mean)
##         deciles<-quantile(s,(1:9)/10)
##         tmp<-data.frame(id=names(s),sc=as.numeric(s))
##         tmp$q.ay<-cut(tmp$sc,c(-Inf,deciles,Inf),labels=1:10)
##         tmp$s<-NULL
##         dim(df)
##         df<-merge(df,tmp,all.x=TRUE)
##         dim(df)
##         df
##     }
##     L<-split(df,df$ay)
##     L<-lapply(L,getq,months=months)
##     df<-data.frame(do.call("rbind",L))
##     ## ##densities
##     ## cols<-c("lightblue","darkblue","red")
##     ## ayL<-2018:2020
##     ## plot(NULL,xlim=c(0,150),ylim=c(0,.04))
##     ## for (i in 1:length(ayL)) {
##     ##     z<-df[df$ay==ayL[i],]
##     ##     z<-z[!duplicated(z$id),]
##     ##     lines(density(z$sc),col=cols[i],lwd=3)
##     ## }   
##     ## ##
##     f<-function(df,y,maxtime=90) {
##         yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
##         df$del<-difftime(df$t,yy,units='days')
##         df<-df[df$del>0 & df$del<(maxtime),]
##         ######################################
##         df$del<-as.numeric(df$del)
##         m<-felm(score~del|title+id,df)
##         print(length(fitted(m)))
##         summary(m)$coef[,1:2]
##     }
##     tab.all<-tab.ay<-list()
##     for (y in 2019:2020) {
##         z.all<-z.ay<-list()
##         for (i in 1:10) {
##             #z.all[[i]]<-f(y,df=df[df$q.all==i,])
##             z.ay[[i]]<-f(y,df=df[df$q.ay==i,])
##         }
##         #tab.all[[as.character(y)]]<-do.call("rbind",z.all)
##         tab.ay[[as.character(y)]]<-do.call("rbind",z.ay)
##     }
##     #tab.all<-do.call("rbind",tab.all)
##     #tab.ay<-do.call("rbind",tab.ay)
##     list(#all=tab.all,
##          ay=tab.ay)
## }

## load("df.Rdata")
## df<-df[df$ay>=2019,]
## df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
## ##
## #id2020<-df$id[df$ay==2020 & df$covid]
## #df<-df[df$id %in% id2020,]

## ll<-list()
## for (i in 1:4) ll[[i]]<-bigfun(df,i)


## pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/decile.pdf",width=2.5,height=6)
## par(mfrow=c(4,1),mgp=c(2,1,0),mar=c(3,3,0,.2),oma=c(.35,.35,2,.35))
## library(gplots)
## years<-2019:2020 #no 2017
## for (i in 1:4) {
##     tab.ay<-ll[[i]]$ay
##     #
##     ##
##     plot(NULL,xlim=c(1,10),ylim=c(-.05,.35),xlab='',ylab='Growth'); abline(h=0)
##     cols<-c("gray","red")
##     for (j in 1:length(years)) { 
##         z<-tab.ay[[as.character(years)[j] ]]
##         lines(z[,1],type='b',pch=19,cex=.5,col=cols[j],lwd=2)
##         x<-z[,1]
##         s<-z[,2]
##         col<-col2rgb(cols[j])
##         col<-rgb(col[1],col[2],col[3],alpha=105,max=255)
##         polygon(c(1:10,10:1),c(x+1.96*s,rev(x-1.96*s)),col=col,border=NA)
##     }
##     if (i==1) {
##         legend("topleft",bty='n',legend=c("19-20","20-21"),fill=cols,title=paste("Grade",i),cex=.7)
##     } else legend("topleft",bty='n',title=paste("Grade",i),legend='',cex=.7)
##     if (i==4) mtext(side=1,line=2.2,'Decile',cex=.7)
##     ##
## }
## dev.off()

## ## ll9<-list()
## ## for (i in 1:3) ll9[[i]]<-bigfun(df,i,months=9)

## ## par(mfrow=c(3,2),mgp=c(2,1,0),mar=c(3,3,0,.2),oma=c(.35,.35,2,.35))
## ## library(gplots)
## ## for (i in 1:3) {
## ##     tab.all<-ll[[i]]$all
## ##     tab.ay<-ll[[i]]$ay
## ##     #
## ##     plot(NULL,xlim=c(1,10),ylim=c(-.1,.4),xlab='',ylab='growth'); abline(h=0)
## ##     legend("topright",bty='n',paste("grade=",i,sep=''))
## ##     cols<-c("lightblue","darkblue","red")
## ##     for (j in 1:length(tab.all)) lines(tab.all[[j]][,1],type='b',pch=19,cex=.5,col=cols[j])
## ##     if (i==1) mtext(side=3,'static deciles',line=0.3)
## ##     ##
## ##     plot(NULL,xlim=c(1,10),ylim=c(-.1,.4),xlab='',ylab='growth'); abline(h=0)
## ##     cols<-c("lightblue","darkblue","red")
## ##     for (j in 1:length(tab.ay)) lines(tab.ay[[j]][,1],type='b',pch=19,cex=.5,col=cols[j])
## ##     if (i==1) {
## ##         mtext(side=3,'dynamic deciles',line=0.3)
## ##         legend("topright",bty='n',legend=2018:2020,fill=cols)
## ##     }
## ## }



    
