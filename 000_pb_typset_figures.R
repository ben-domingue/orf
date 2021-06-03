## All of the color values are listed in the “working” guidelines document (attached) on page 1, and the order in which they should be used for figures is on page 2. Since most of the figures appear to require two colors, please should use PACE Blue and Stanford Sun.

## Please use Calibri, which is PACE’s standard sans serif font. I recommend 11 points for the figures, which is halfway between the body copy and the endnotes.

## Full width figures should be 6.5” wide, and half-column figures should be 3” wide with a .5” space in between.

##pace blue RGB 0 / 81 /138
##stanford sun RGB 234 / 171 / 0
paceblue<-rgb(0,81,138,max=255)
stansun<-rgb(234,171,0,max=255)
pacered<-rgb(161, 31, 65,max=255)
library(showtext)
font_add_google("Open Sans","opensans")
#font_add_google("Schoolbell", "bell")

#####################
#####################
#####################
#####################
#####################
#####################
#
#postscript('/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig2.ps',width=6.5,height=3.5)
png("/tmp/fig.png",width = 500, height = 350,)#,width=4.5,height=3.5,units="in",res=200)
#pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig2.pdf",width=6.5,height=3.5)
showtext_auto()
par(mgp=c(2,1,0),mar=c(3,4,1,1),oma=c(.5,.5,1,.5))
for (grade in 1) {
    load("df.Rdata")
    ids<-unique(df$id[df$ay==2018 & df$grade==grade])
    df<-df[df$id %in% ids,]
    ids<-unique(df$id[df$covid])
    df<-df[df$id %in% ids,]
    yy<-strptime(paste("2018-09-01",sep=""),format="%Y-%m-%d")
    df$del<-as.numeric(difftime(df$t,yy,units='days'))
    plot(NULL,xlim=c(0,835),ylim=c(0,65),xlab='Days since September 1, 2018',bty='n',ylab='',family='opensans')
    mtext(side=2,line=2,'Oral reading fluency\n(words per minute)',family='opensans')
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
    lines(t,y0,lwd=3.5,col=paceblue)
    #text(t[length(t)],y0[length(t)],pos=4,labels=ii,cex=.8)
    ##covid
    covid.day<-strptime('2020-03-11',format="%Y-%m-%d")
    z<-difftime(yy,covid.day,units='days')
    abline(v=abs(z),col=pacered)
    mtext(side=3,line=0,"Onset of COVID-19",col=pacered,at=abs(z),,family='opensans')
    iii<-which.min(abs(t-abs(z)))
    #
    slope<-y0[iii]/t[iii]
    lines(c(0,max(t)),c(0,slope*max(t)),lwd=2.5,col='black')
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
    text(50,60,paste("Grade",grade,"\n2018-19"),cex=.8,family='opensans')
    text(440,60,paste("Grade",grade+1,"\n2019-20"),cex=.8,family='opensans')
    text(798,10,paste("Grade",grade+2,"\n2020-21"),cex=.8,family='opensans')
}
dev.off()


#####################
#####################
#####################
#####################
#####################
#####################
pf<-function(df,
             nspl=3,
             years=2017:2020,
             timelim=365+90,
             ymax=35,
             txt=list(`2017`='2017-18',`2018`='2018-19',`2019`='2019-20',`2020`='2020-21')
             ) {
    par(mgp=c(2,1,0))
    plot(NULL,xlim=c(0,1.2*timelim),ylim=c(-1,ymax),xaxt='n',ylab='',xlab="Days since September 1 of AY",bty='n',family='opensans',cex.axis=.8)
    axis(side=1,at=seq(0,timelim,by=90),cex.axis=.8,family='opensans')
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
        if (y==2018) col<-stansun
        if (y==2019) col<-paceblue
        if (y==2020) col<-'darkgray'
        lines(t,y0,lwd=3,col=col)
        #text(t[1],y0[1],pos=2,y,cex=.4)
        text(t[length(t)],y0[length(y0)],pos=4,y,cex=1,col=col,labels=txt[[as.character(y)]],xpd=TRUE,family='opensans')
    }
    covid.day<-strptime('2020-03-11',format="%Y-%m-%d")
    z<-difftime(strptime("2019-09-01",format="%Y-%m-%d"),covid.day,units='days')
    abline(v=abs(z),col=pacered)
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

#pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig3.pdf",width=6.5,height=3)
postscript('/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig3.ps',width=6.5,height=3)
showtext_auto()
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,2,1,1),oma=c(.6,3,.6,.6))
par(family = "opensans")
load("df.Rdata")
ids<-unique(df$id[df$covid])
df<-df[df$id %in% ids,]
for (gr in 1:3) {
    pf(df[df$grade==gr,],years=2018:2019,nspl=5,ymax=52)
    if (gr==1)  mtext(side=2,line=0,"Oral reading fluency\n(words per minute)",outer=TRUE,cex=.8,family='opensans')
    legend("topleft",bty='n',legend=paste('Grade',gr),cex=.8)
    #text(53,47,paste('Grade',gr),family='opensans')
}
dev.off()

#####################
#####################
#####################
#####################
#####################
#####################

f<-function(df,y,maxtime=90) {
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    print(yy)
    df$del<-difftime(df$t,yy,units='days')
    df<-df[df$del>0 & df$del<(maxtime),]
    ######################################
    library(lfe)
    df$del<-as.numeric(df$del)
    m<-felm(score~del|id+title,df)
    c(as.numeric(t(summary(m)$coef[,1:2])))#,nrow(m$resid))
}
load("df.Rdata")
#ids<-unique(df$id[df$covid])
#df<-df[df$id %in% ids,]
id2020<-df$id[df$ay==2020 & df$covid]
#df<-df[df$id %in% unique(id2020),]
df<-df[df$id %in% id2020,]
#################################
L<-list()
for (gr in 1:4) {
    tmp<-df[df$grade==gr,]
    x2019<-f(2019,df=tmp)
    x2020<-f(2020,df=tmp)
    L[[as.character(gr)]]<-c(x2019,x2020)
}
tab<-do.call("rbind",L)

##barplot
#pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig4.pdf",width=3,height=3.6)
postscript('/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig4.ps',width=3,height=3.6)

showtext_auto()
par(mar=c(3,3,1,1),mgp=c(2,1,0),oma=c(.3,1.3,.3,.3))
par(family = "opensans")
library(gplots)
z<-t(tab)
barplot2(z[c(1,3),],ylim=c(0,.24),yaxt='n',
         beside=TRUE,col=c(stansun,paceblue),
         names.arg=1:4,
         plot.ci=TRUE,
         ci.u=rbind(z[1,]+1.96*z[2,],z[3,]+1.96*z[4,]),
         ci.l=rbind(z[1,]-1.96*z[2,],z[3,]-1.96*z[4,]),
         ylab='',xlab="Grade"
         )->zz
mtext(side=2,line=2,'Growth rate in oral reading fluency\n(words per minute/day)',cex=1)
axis(side=2,at=seq(0,.2,by=.05))
## comment out this for block if you don't want labels
for (i in 1:ncol(zz)) text(x=zz[,i],
                            y=c(z[1,i]+1.96*z[2,i],z[3,i]+1.96*z[4,i]),
                            labels=round(c(z[1,i],z[3,i]),2),
                            #pos=3,
                            adj=c(-.085,.5),
                            offset=0,
                            cex=.7,srt=90)
legend("topright",bty='n',fill=c(stansun,paceblue),legend=c("2019-20","2020-21"),cex=1)
dev.off()


#####################
#####################
#####################
#####################
#####################
#####################

##seda
#run F4_...

#pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig5.pdf",width=6.5,height=3)
postscript('/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/fig5.ps',width=6.5,height=3)
showtext_auto()
par(family = "opensans")
dist<-df[,c("nces","seda")]
dist<-dist[!duplicated(dist$nces),]
qu<-quantile(dist$seda,c(.1,.9))
xv<-0:90
par(mfrow=c(1,3),mgp=c(2,1,0),oma=c(.6,3,.6,.6),mar=c(3,2,1,1))
for (i in 1:3) {
    for (j in 2) {
        if (j==2) {
            plot(NULL,xlim=c(0,90),ylim=c(0,22),bty='n',xlab="Days since September 1, 2020",ylab='')
            if (i==1)  mtext(side=2,line=0,"Oral reading fluency\n(words per minute)",outer=TRUE,cex=.8)
        }
        est<-out[[j]][[i]]
        y1<-xv*est[1,1]+est[2,1]*qu[1]*xv
        y2<-xv*est[1,1]+est[2,1]*qu[2]*xv
        lines(xv,y1,col=paceblue,lwd=3)
        lines(xv,y2,col=stansun,lwd=3)
        legend("bottomright",bty='n',legend=paste("Grade",i))
    }
    if (i==1) legend(5,20,bty='n',title="Percentile of\nSEDA achievement",cex=.8,legend=rev(c("10th","90th")),fill=rev(c(paceblue,stansun)))
}
dev.off()
