
##selection
pf_selection<-function(df,
                       nspl=3,
                       years=2017:2020,
                       timelim=365+90,
                       ymax=35,
                       txt=list(`2017`='2017',`2018`='2018',`2019`='2019',`2020`='2020'),
                       poly=TRUE
                       ) {
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
        lines(t,y0,lwd=3)
        #text(t[1],y0[1],pos=2,y,cex=.4)
        text(t[length(t)],y0[length(y0)],pos=4,y,cex=1,labels=txt[[as.character(y)]],xpd=TRUE)
    }
    ##polygons
    if (poly) {
        summer<-strptime('2020-06-15',format="%Y-%m-%d")
        z<-difftime(summer,strptime("2019-09-01",format="%Y-%m-%d"),units='days')
        summer.end<-strptime('2020-09-01',format="%Y-%m-%d")
        z.end<-difftime(summer.end,strptime("2019-09-01",format="%Y-%m-%d"),units='days')
        #abline(v=abs(z),col='gray')
        col<-col2rgb('blue')
        col<-rgb(col[1],col[2],col[3],alpha=25,max=255)
        polygon(c(z,rep(z.end,2),z),100*c(-5,-5,5,5),col=col,border=NA)
}
}

load("df.Rdata")
##making groups
group<-list()
covid.ids<-unique(df$id[df$covid])
id0<-unique(df$id[df$ay==2019])
group$`No obs`<-id0[!(id0 %in% covid.ids)]
group$`Obs in 2020`<-intersect(id0,unique(df$id[df$ay==2020]))
group$`Obs in 2019`<-intersect(id0,unique(df$id[df$ay==2019 & df$covid]))
                     
##
pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/selection.pdf",width=7,height=4)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,5),oma=rep(.5,4))
##
plot(NULL,xlim=c(0,1.03*(365+90)),
     ylim=c(-1,45),
     xaxt='n',ylab="Growth",xlab="Days since Sept 1 of year",bty='n')
axis(side=1,at=seq(0,365+90,by=90))
for (i in 1:3) {
    ##
    txt=list(names(group)[i])
    names(txt)[1]<-'2018'
    pf_selection(df[df$id %in% group[[i]],],years=2018,nspl=4,ymax=30,txt=txt,poly=(i==1))
    legend("topleft",bty='n',legend=2018)
}
plot(NULL,xlim=c(0,1.03*(200)),
     ylim=c(-1,45),
     xaxt='n',ylab="Growth",xlab="Days since Sept 1 of year",bty='n')
axis(side=1,at=seq(0,365+90,by=90))
for (i in 1:3) {
    txt=list(names(group)[i])
    names(txt)[1]<-'2019'
    pf_selection(df[!df$covid & df$id %in% group[[i]],],years=2019,nspl=4,ymax=30,txt=txt)
    legend("topleft",bty='n',legend=2019)
}
dev.off()

####################################################################
##mean scores
load("df.Rdata")
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
df<-df[df$m %in% 9:11 & df$ay==2019,]
t<-list()
for (i in 1:3) {
    tmp<-df[df$id %in% group[[i]],]
    t[[names(group)[i] ]]<-by(tmp$score,tmp$grade,mean)
}

tab<-do.call("cbind",t)
library(xtable)
print(xtable(tab,digits=1,display=c("d","d","d","d")
             ),include.rownames=TRUE)
