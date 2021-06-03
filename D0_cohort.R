
pdf("/home/bd/Dropbox/projects/orf/docs/v1/fig2.pdf",width=6,height=3.5)
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=c(.5,.5,1,.5))
for (grade in 1) {
    load("df.Rdata")
    ids<-unique(df$id[df$ay==2018 & df$grade==grade])
    df<-df[df$id %in% ids,]
    ids<-unique(df$id[df$covid])
    df<-df[df$id %in% ids,]
    yy<-strptime(paste("2018-09-01",sep=""),format="%Y-%m-%d")
    df$del<-as.numeric(difftime(df$t,yy,units='days'))
    plot(NULL,xlim=c(0,835),ylim=c(0,65),xlab='Days since September 1 2018',bty='n',ylab='ORF (WPM)')
    nspl<-7
    ##for (ii in 1:10) {
    #tmp<-df[df$qu==ii & df$del>0,]
    mmm<-0 ##if (grade==0) mmm<-120 else mmm<-0
    tmp<-df[df$del>mmm,]    ##
    library(splines)
    spl<-bs(tmp$del,df=nspl)
    for (i in 1:ncol(spl)) tmp[[paste("spl",i,sep="")]]<-spl[,i]
    library(lfe)
    fm<-paste(paste("spl",1:nspl,sep=""),collapse='+')
    fm<-paste("score~",fm,"|id+title")
    m<-felm(as.formula(fm),tmp)
    ##
    t<-(mmm+10):830
    s<-predict(spl,t)
    co<-coef(m)
    y0<-s %*% matrix(co,ncol=1) #co[1]*s[,1]+co[2]*s[,2]+co[3]*s[,3]
    y0<-as.numeric(y0)
    ##
    lines(t,y0,lwd=2,col="blue")
    #text(t[length(t)],y0[length(t)],pos=4,labels=ii,cex=.8)
    ##covid
    covid.day<-strptime('2020-03-11',format="%Y-%m-%d")
    z<-difftime(yy,covid.day,units='days')
    abline(v=abs(z),col='red')
    mtext(side=3,line=0,"Onset of COVID",col='red',at=abs(z))
    iii<-which.min(abs(t-abs(z)))
    #
    slope<-y0[iii]/t[iii]
    lines(c(0,max(t)),c(0,slope*max(t)),lwd=2,col='black')
    ########################
    for (y in 2019:2020) {
        ##summer
        summer<-strptime(paste(y,'-06-15',sep=''),format="%Y-%m-%d")
        z<-difftime(summer,yy,units='days')
        summer.end<-strptime(paste(y,'-09-01',sep=''),format="%Y-%m-%d")
        z.end<-difftime(summer.end,yy,units='days')
        #abline(v=abs(z),col='gray')
        col<-col2rgb('blue')
        col<-rgb(col[1],col[2],col[3],alpha=25,max=255)
        polygon(c(z,rep(z.end,2),z),100*c(-5,-5,5,5),col=col,border=NA)
    }
    text(50,60,paste("Grade",grade,"\nin AY18-19"),cex=.8)
    text(440,60,paste("Grade",grade+1,"\nin AY19-20"),cex=.8)
    text(798,10,paste("Grade",grade+2,"\nin AY20-21"),cex=.8)
}
dev.off()





## pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/spl_2y.pdf",width=3,height=2.5)





                

