
load("df.Rdata")
dim(df)
L<-split(df,df$ay)
f<-function(xx,df) {
    y<-unique(xx$ay)
    x<-df[df$ay==y-1 & !df$covid,]
    if (nrow(x)>0) {
        M<-by(x$score,x$id,mean,na.rm=TRUE)
        M<-data.frame(id=names(M),mm=as.numeric(M),ay=y)
        xx<-merge(xx,M,all.x=TRUE)
    } else {
        xx$mm<-NA
    }
    xx
}
L<-lapply(L,f,df=df)
df<-data.frame(do.call("rbind",L))
dim(df)

load("geo.Rdata")
geo<-geo[!is.na(geo$month) & geo$month==9 & geo$year==2020,]
dist<-geo[geo$nces %in% unique(df$nces),]
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
dist$seda<-std(dist$seda)
dist$ses<-std(dist$ses)
df<-merge(df,dist)
df$mm<-std(df$mm)


####interaction
f<-function(df,y,maxtime=90) {
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    print(yy)
    df$del<-difftime(df$t,yy,units='days')
    df<-df[df$del>0 & df$del<(maxtime),]
    ######################################
    library(lfe)
    df$del<-as.numeric(df$del)
    m<-felm(score~del+del:inter|id+title,df)
    S<-summary(m)$coef
    list(S=S,nscore=length(m$fitted.values),nstud=nrow(getfe(m)))
}

Lout<-list()
for (inter in c("seda","ses","mm"
                ,"share_elem_closed_50"
                )) {
    df$inter<-df[[inter]]
    out<-list()
    for (y in 2019:2020) {
        est<-list()
        for (g in 1:4) {
            z<-df[df$grade==g & df$ay==y,]
            z<-f(z,y=y)
            est[[g]]<-z
        }
        out[[as.character(y)]]<-est
    }
    Lout[[inter]]<-out
}

ff<-function(x) {
    c(x$S[1,1],x$S[1,2],x$S[2,1],x$S[2,2],x$nscore,x$nstud)
}
tab<-list()
for (i in 1:length(Lout)) {
    for (j in 1:4) {
        t1<-ff(Lout[[i]]$`2019`[[j]])
        t2<-ff(Lout[[i]]$`2020`[[j]])
        tab[[paste(i,j)]]<-c(names(Lout)[i],j,t1,t2)
    }
}
tab<-do.call("rbind",tab)
tab<-as.data.frame(tab)
for (i in 2:ncol(tab)) tab[,i]<-as.numeric(tab[,i])

library(xtable)
print(xtable(tab,digits=3,display=c("d", #dummy
                                    "s","d",
                                    "f","f",
                                    "f","f",
                                    "d","d",
                                    "f","f",
                                    "f","f",
                                    "d","d")
             ),include.rownames=FALSE)


out<-list()
for (j in 1:2) {
    tmp<-list()
    for (i in 1:4) {
        tmp[[i]]<-Lout$seda[[j]][[i]]$S
    }
    out[[j]]<-tmp
}

        

##both years
#pdf("/home/bd/Dropbox/projects/orf/docs/v1/fig6.pdf",width=6,height=8)
pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/main_fig6.pdf",width=6.5,height=8)
dist<-df[,c("nces","seda")]
dist<-dist[!duplicated(dist$nces),]
qu<-quantile(dist$seda,c(.1,.9))
xv<-0:90
par(mfrow=c(4,2),mgp=c(2,1,0),oma=c(2,3,1,1))
for (i in 1:4) {
    for (j in 1:2) {
        if (j==1) {
            par(mar=c(3,1,0.4,0))
            plot(NULL,xlim=c(0,90),ylim=c(0,26),bty='n',ylab="",xlab='',xaxt='n')
            axis(side=1,seq(0,90,by=30))
            mtext(side=2,line=2,'Oral reading fluency\n(words per minute)',cex=.7)
        }            
        if (j==2) {
            par(mar=c(3,1,0.4,0))
            plot(NULL,xlim=c(0,90),ylim=c(0,25),bty='n',ylab='',xlab='',yaxt='n',xaxt='n')
            axis(side=1,seq(0,90,by=30))
        }
        est<-out[[j]][[i]]
        y1<-xv*est[1,1]+est[2,1]*qu[1]*xv
        y2<-xv*est[1,1]+est[2,1]*qu[2]*xv
        lines(xv,y1,col='blue',lwd=3)
        lines(xv,y2,col='black',lwd=3)
        if (i==1 & j==1) {
            legend("topleft",bty='n',title="Percentile of SEDA achievement",legend=c("10th","90th"),fill=c("blue","black"),cex=1)
        }
        if (i==1) title<-c("2019-20","2020-21")[j] else title<-''
        legend("bottomright",bty='n',legend=paste("Grade",i), title=title,cex=1)
        if (i==4) mtext(side=1,line=2,"Days since September 1")
        if (j==2) print(100*c(y1[length(y1)],y2[length(y2)]))
    }
}
dev.off()

##ballparkin SDs, Reid Saaris april 6 2021
load("df.Rdata")
df<-df[df$ay==2020 & df$grade==2,]
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
df<-df[df$m==9,]
sd(df$score)
##[1] 1052 1907
(1907-1052)/(100*sd(df$score))


## ##small figure, just 2020-2021
## #pdf("/home/bd/Dropbox/projects/orf/docs/v1/fig6.pdf",width=6,height=8)
## dist<-df[,c("nces","seda")]
## dist<-dist[!duplicated(dist$nces),]
## qu<-quantile(dist$seda,c(.1,.9))
## xv<-0:90
## par(mfrow=c(4,2),mgp=c(2,1,0),oma=rep(.5,4),mar=c(3,3,1,1))
## for (i in 1:4) {
##     for (j in 2) {
##         if (j==2) {
##             plot(NULL,xlim=c(0,90),ylim=c(0,22),bty='n',xlab="Days since Sept 1, 2020",ylab='ORF (WPM)')
##         }
##         est<-out[[j]][[i]]
##         y1<-xv*est[1,1]+est[2,1]*qu[1]*xv
##         y2<-xv*est[1,1]+est[2,1]*qu[2]*xv
##         lines(xv,y1,col='blue',lwd=3)
##         lines(xv,y2,col='black',lwd=3)
##         legend("bottomright",bty='n',legend=paste("Grade",i))
##     }
##     if (i==1) legend(5,20,bty='n',title="Percentile of\nSEDA achievement",cex=.8,legend=rev(c("10th","90th")),fill=rev(c("blue","black")))
## }
## #dev.off()
