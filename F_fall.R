
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

library(xtable)
print(xtable(tab,digits=3,display=c("d",rep("f",4))
             ),include.rownames=TRUE)


##barplot
pdf("/home/bd/Dropbox/projects/orf/docs/v1/fig5.pdf",width=4,height=4)
par(mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.3,4))
library(gplots)
z<-t(tab)
barplot2(z[c(1,3),],ylim=c(0,.22),yaxt='n',
         beside=TRUE,col=c("darkgray","blue"),
         names.arg=1:4,
         plot.ci=TRUE,
         ci.u=rbind(z[1,]+1.96*z[2,],z[3,]+1.96*z[4,]),
         ci.l=rbind(z[1,]-1.96*z[2,],z[3,]-1.96*z[4,]),
         ylab='Growth rate in ORF (WPM/day)',xlab="Grade"
         )->zz
axis(side=2,at=seq(0,.2,by=.05))
## comment out this for block if you don't want labels
for (i in 1:ncol(zz)) text(x=zz[,i],
                            y=c(z[1,i]+1.96*z[2,i],z[3,i]+1.96*z[4,i]),
                            labels=round(c(z[1,i],z[3,i]),2),
                            #pos=3,
                            adj=c(-.085,.5),
                            offset=0,
                            cex=.7,srt=90)
legend("topright",bty='n',fill=c("darkgray","blue"),legend=c("AY19-20","AY20-21"),cex=1)
dev.off()


## #################

##add f() from top
##making groups
load("df.Rdata")
group<-list()
covid.ids<-unique(df$id[df$covid])
id0<-unique(df$id[df$ay==2019])
group$`No obs`<-id0[!(id0 %in% covid.ids)]
id2019<-intersect(id0,unique(df$id[df$ay==2019 & df$covid]))
id2020<-intersect(id0,unique(df$id[df$ay==2020]))
group$`Obs in 2020F`<-id2020[!(id2020 %in% id2019)]
group$`Obs in 2020S`<-id2019

length(id0)
sapply(group,length)
for (i in 1:length(group)) {
    tmp<-df[df$id %in% group[[i]],]
    print(names(group)[i])
    tmp<-tmp[!duplicated(tmp$id),]
    print(table(tmp$grade)/nrow(tmp))
}

L<-list()
for (gr in 1:4) {
    tmp<-df[df$grade==gr,]
    x2019a<-f(2019,df=tmp[tmp$id %in% group[[2]],])
    x2019b<-f(2019,df=tmp[tmp$id %in% group[[3]],])
    x2020a<-f(2020,df=tmp[tmp$id %in% group[[2]],])
    x2020b<-f(2020,df=tmp[tmp$id %in% group[[3]],])
    l<-list(`2019-Obs in 2020F`=x2019a,
            `2019-Obs in 2020S`=x2019b,
            `2020-Obs in 2020F`=x2020a,
            `2020-Obs in 2020S`=x2020b)
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

paceblue<-rgb(0,81,138,max=255)
stansun<-rgb(234,171,0,max=255)
pacered<-rgb(161, 31, 65,max=255)

txt.fancy<-c(`2019`="2019-20",`2020`="2020-21")
#pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/fall.pdf",width=6,height=8)
pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/si_fig6.pdf",width=6.5,height=8)
fancy1<-list("Observed in fall 2020","Observed in spring 2020")
names(fancy1)<-tmp
par(mfrow=c(4,2),mar=c(3,3,1,.3),mgp=c(2,1,0))
for (iii in 1:length(L)) {
    x<-L[[iii]]
    xv<-list()
    for (i in 1:length(x)) xv[[i]]<-f(x[[i]])
    names(xv)<-names(x)
    for (i.index in list(c(1,3),c(2,4))) {
        plot(NULL,pch=19,col=paceblue,cex=.2,ylim=c(-1,25),
             xlab="Days since September 1 of academic year",
             ylab="Growth",
             xlim=c(0,125),bty='n',xaxt='n')
        axis(side=1,at=c(0,30,60,90))
        for (i in i.index) {
            yyy<-substr(names(xv)[i],1,4)
            lines(xv[[i]],col=ifelse(yyy==2019,paceblue,pacered),pch=19,cex=.2,lty=1,lwd=2.5)
            nn<-nrow(xv[[i]])
            text(xv[[i]][nn,1],xv[[i]][nn,2],txt.fancy[yyy],cex=.75,pos=4)
            print(names(xv[i]))
        }
        ## for (i in 3:4) {
        ##     lines(xv[[i]],col='red',pch=19,cex=.2,lty=i-2,lwd=2.5)
        ##     nn<-nrow(xv[[i]])
        ##     text(xv[[i]][nn,1],xv[[i]][nn,2],names(xv)[i],cex=.75,pos=4)
        ## }
        abline(v=192,col=pacered)
        tmp<-names(xv)[i.index]
        n<-sapply(tmp,nchar)
        if (substr(tmp[[1]],5,n[1])==substr(tmp[[2]],5,n[2])) {
                                        #             legend("topleft",bty='n',title=paste("Grade",names(L)[iii]),legend=substr(tmp[[1]],6,n[1]),cex=.7)
            legend("topleft",bty='n',title=paste("Grade",names(L)[iii]),legend=fancy1[[ifelse(i.index[1]==1,1,2)]],cex=1)
        } else {
            print("problem")
        }
    }
}
#legend("bottomright",bty='n',fill=c("darkgray","red"),legend=2019:2020)
dev.off()
