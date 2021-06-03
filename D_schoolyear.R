pf<-function(df,
             nspl=3,
             years=2017:2020,
             timelim=365+90,
             ymax=35,
             txt=list(`2017`='2017-18',`2018`='2018-19',`2019`='2019-20',`2020`='2020-21')
             ) {
    par(mgp=c(2,1,0))
    plot(NULL,xlim=c(0,1.2*timelim),ylim=c(-1,ymax),xaxt='n',ylab='Oral reading fluency (words per minute)',xlab="Days since September 1 of AY",bty='n')
    axis(side=1,at=seq(0,timelim,by=90),cex=.9)
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
        #fm<-paste("score~",fm,"|title+id")
        fm<-paste("score~",fm,"|title+id")
        m<-felm(as.formula(fm),tmp)
        ##
        m1<-.92*timelim
        m2<-max(tmp$del)
        MM<-min(m1,m2)
        t<-seq(min(tmp$del),MM,length.out=1000)
        s<-predict(spl,t)
        co<-coef(m)
        y0<-s %*% matrix(co,ncol=1) #co[1]*s[,1]+co[2]*s[,2]+co[3]*s[,3]
        y0<-as.numeric(y0)
        if (y==2018) col<-'black'
        if (y==2019) col<-'blue'
        if (y==2020) col<-'darkgray'
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

pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/main_fig3.pdf",width=6.5,height=6)
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.6,4))
load("df.Rdata")
ids<-unique(df$id[df$covid])
df<-df[df$id %in% ids,]
for (gr in 1:4) {
    pf(df[df$grade==gr,],years=2018:2019,nspl=5,ymax=52)
    legend("topleft",bty='n',legend=paste('Grade',gr))
}
dev.off()




#############################################################################
## pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/spl1.pdf",width=4,height=4)
## par(mar=c(3,3,1,1.8),oma=rep(.5,4))
## pf(df.pc,nspl=4,years=2018:2019)
## dev.off()

## ##students with post-covid scores
## pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/spl_sense.pdf",width=7,height=5)
## par(mfrow=c(2,2),mar=c(3,3,1,1.8),oma=rep(.5,4))
## for (n in 3:6) {
##     pf(df.pc,nspl=n,years=2018:2019)
##     legend("topleft",bty='n',legend=paste('k=',n,sep=''))
## }
## dev.off()





## ##by starting value
load("df.Rdata")
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
MM<-list()
for (i in 1:3) MM[[as.character(i)]]<-mean(df$score[df$grade==i],na.rm=TRUE)
L<-split(df,paste(df$id,df$ay))
ff<-function(x) {
    mm<-mean(x$score[x$m %in% 9:12],na.rm=TRUE)
    c(unique(x$id),unique(x$ay),mm)
}
L<-lapply(L,ff)
ms<-data.frame(do.call("rbind",L))
names(ms)<-c("id","ay","fallmean")
ms<-ms[is.finite(ms$fallmean),]

pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/growth_by_ability.pdf",width=5,height=6)
par(mfrow=c(3,2),mar=c(3,3,1,2),oma=rep(.5,4))
for (gr in 1:3) {
    tmp<-df.pc[df.pc$grade==gr,]
    tmp<-merge(tmp,ms)
    tmp.l<-tmp[tmp$fallmean<=MM[[as.character(gr)]],]
    pf(tmp.l,years=2018:2020,nspl=4,ymax=70)
    legend("topleft",bty='n',title=paste('grade',gr),legend=paste("Mean",round(mean(tmp.l$score,na.rm=TRUE),0)))
    tmp.h<-tmp[tmp$fallmean>MM[[as.character(gr)]],]
    pf(tmp.h,years=2018:2020,nspl=4,ymax=70)
    legend("topleft",bty='n',title=paste('grade',gr),legend=paste("Mean",round(mean(tmp.h$score,na.rm=TRUE),0)))
}
dev.off()
