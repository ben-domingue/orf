
f<-function(df,y,maxtime=90) {
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    print(yy)
    df$del<-difftime(df$t,yy,units='days')
    df<-df[df$del>0 & df$del<(maxtime),]
    ######################################
    library(lfe)
    df$del<-as.numeric(df$del)
    m<-felm(score~del|title+id,df)
    c(as.numeric(t(summary(m)$coef[,1:2])),nrow(m$resid))
}

#load("df.Rdata")
#ids<-unique(df$id[df$covid])
#df<-df[df$id %in% ids,]
#id2020<-df$id[df$ay==2020 & df$covid]
#df<-df[df$id %in% unique(id2020),]
load("df.Rdata")
##making groups
group<-list()
covid.ids<-unique(df$id[df$covid])
id0<-unique(df$id[df$ay==2019])
group$`No obs`<-id0[!(id0 %in% covid.ids)]
group$`Obs in 2020`<-intersect(id0,unique(df$id[df$ay==2020]))
group$`Obs in 2019`<-intersect(id0,unique(df$id[df$ay==2019 & df$covid]))

L<-list()
for (gr in 1:4) {
    tmp<-df[df$grade==gr,]
    x2019a<-f(2019,df=tmp[tmp$id %in% group[[2]],])
    x2019b<-f(2019,df=tmp[tmp$id %in% group[[3]],])
    x2020a<-f(2020,df=tmp[tmp$id %in% group[[2]],])
    x2020b<-f(2020,df=tmp[tmp$id %in% group[[3]],])
    l<-list(`2019-Obs in 2020`=x2019a,
            `2019-Obs in 2019`=x2019b,
            `2020-Obs in 2020`=x2020a,
            `2020-Obs in 2019`=x2020b)
    L[[as.character(gr)]]<-l
}


##figure
f<-function(est) {
    t0<-seq(0,90,length.out=1000)
    z<-cbind(t0,t0*est[1])
    #z[[3]]<-cbind(t2+d1+d2,est[7]+t2*est[9])
    #z[[4]]<-cbind(t3+d1+d2+d3,est[11]+t3*est[13])
    z
}

pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/fall.pdf",width=3,height=9)
par(mfrow=c(4,1),mar=c(3,3,1,3),mgp=c(2,1,0))
for (iii in 1:length(L)) {
    x<-L[[iii]]
    xv<-list()
    for (i in 1:length(x)) xv[[i]]<-f(x[[i]])
    names(xv)<-names(x)
    plot(NULL,pch=19,col='darkgray',cex=.2,ylim=c(-1,25),
         xlab="Days since start of AY",
         ylab="Growth",
         xlim=c(0,135),bty='n',xaxt='n')
    axis(side=1,at=c(0,30,60,90))
    for (i in 1:2) {
        lines(xv[[i]],col='gray',pch=19,cex=.2,lty=i,lwd=2.5)
        nn<-nrow(xv[[i]])
        text(xv[[i]][nn,1],xv[[i]][nn,2],names(xv)[i],cex=.75,pos=4)
    }
    for (i in 3:4) {
        lines(xv[[i]],col='red',pch=19,cex=.2,lty=i-2,lwd=2.5)
        nn<-nrow(xv[[i]])
        text(xv[[i]][nn,1],xv[[i]][nn,2],names(xv)[i],cex=.75,pos=4)
    }
    abline(v=192,col='red')
    legend("topleft",bty='n',legend=paste("Grade",names(L)[iii]),)
}
#legend("bottomright",bty='n',fill=c("darkgray","red"),legend=2019:2020)
dev.off()
