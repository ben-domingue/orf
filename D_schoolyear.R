pf<-function(df,
             nspl=3,
             years=2017:2020,
             timelim=365+90,
             ymax=35,
             txt=list(`2017`='2017',`2018`='2018',`2019`='2019',`2020`='2020')
             ) {
    par(mgp=c(2,1,0))
    plot(NULL,xlim=c(0,1.03*timelim),ylim=c(-1,ymax),xaxt='n',ylab="Growth",xlab="Days since Sept 1 of year",bty='n')
    axis(side=1,at=seq(0,timelim,by=90))
    for (y in years) {
        #tmp<-df[df$ay==y & !is.na(df$ay),]
        yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
        df$del<-difftime(df$t,yy,units='days')
        tmp<-df[df$del>0 & df$del<timelim,]
        ##
        library(splines)
        spl<-bs(tmp$del,df=nspl)
        for (i in 1:ncol(spl)) tmp[[paste("spl",i,sep="")]]<-spl[,i]
        library(lfe)
        fm<-paste(paste("spl",1:nspl,sep=""),collapse='+')
        fm<-paste("score~",fm,"|title+id")
        m<-felm(as.formula(fm),tmp)
        ##
        m1<-.98*timelim
        m2<-max(tmp$del)
        MM<-min(m1,m2)
        t<-seq(min(tmp$del),MM,length.out=1000)
        s<-predict(spl,t)
        co<-coef(m)
        y0<-s %*% matrix(co,ncol=1) #co[1]*s[,1]+co[2]*s[,2]+co[3]*s[,3]
        y0<-as.numeric(y0)
        col<-ifelse(y<2019,'darkgray','red')
        lines(t,y0,lwd=3,col=col)
        #text(t[1],y0[1],pos=2,y,cex=.4)
        text(t[length(t)],y0[length(y0)],pos=4,y,cex=1,col=col,labels=txt[[as.character(y)]],xpd=TRUE)
    }
    covid.day<-strptime('2020-03-11',format="%Y-%m-%d")
    z<-difftime(strptime("2019-09-01",format="%Y-%m-%d"),covid.day,units='days')
    abline(v=abs(z),col='red')
    print(z)
    ##polygons
    summer<-strptime('2020-06-15',format="%Y-%m-%d")
    z<-difftime(summer,strptime("2019-09-01",format="%Y-%m-%d"),units='days')
    summer.end<-strptime('2020-09-01',format="%Y-%m-%d")
    z.end<-difftime(summer.end,strptime("2019-09-01",format="%Y-%m-%d"),units='days')
    #abline(v=abs(z),col='gray')
    col<-col2rgb('blue')
    col<-rgb(col[1],col[2],col[3],alpha=25,max=255)
    polygon(c(z,rep(z.end,2),z),100*c(-5,-5,5,5),col=col,border=NA)
}



load("df.Rdata")
ids<-unique(df$id[df$covid])
df.pc<-df[df$id %in% ids,]

pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/spl1.pdf",width=4,height=4)
par(mar=c(3,3,1,1.8),oma=rep(.5,4))
pf(df.pc,nspl=4,years=2018:2019)
dev.off()

##students with post-covid scores
pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/spl_sense.pdf",width=7,height=5)
par(mfrow=c(2,2),mar=c(3,3,1,1.8),oma=rep(.5,4))
for (n in 3:6) {
    pf(df.pc,nspl=n,years=2018:2019)
    legend("topleft",bty='n',legend=paste('k=',n,sep=''))
}
dev.off()



##by grade
pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/grades.pdf",width=7,height=5)
par(mfrow=c(3,2),mar=c(3,3,1,1),oma=rep(.5,4))
for (gr in 0:5) {
    pf(df.pc[df.pc$grade==gr,],years=2018:2019,nspl=4,ymax=70)
    legend("topleft",bty='n',legend=paste('grade',gr))
}
dev.off()


## ##by starting value
## sept<-grep("2018-09",df.pc$t)
## sept<-df.pc[sept,]
## sept<-by(sept$score,sept$id,mean,na.rm=TRUE)
## sept<-data.frame(id=names(sept),s1=as.numeric(sept))
## qu<-quantile(sept[,2],c(.25,.5,.75))
## sept$qu<-cut(sept[,2],c(-Inf,qu,Inf),labels=1:4)
## df.sept<-merge(df.pc,sept)
## par(mfrow=c(2,2),mar=c(3,3,1,1),oma=rep(.5,4))
## for (qu in 1:4) {
##     pf(df.sept[df.sept$qu==qu,],years=2018:2019,nspl=4,ymax=50)
##     legend("topleft",bty='n',legend=paste('qu',qu))
## }
