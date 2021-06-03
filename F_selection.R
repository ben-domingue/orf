##table

f<-function(df,y,maxtime=90) {
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    print(yy)
    df$del<-difftime(df$t,yy,units='days')
    df<-df[df$del>0 & df$del<(maxtime),]
    ######################################
    df
}

load("df.Rdata")
#ids<-unique(df$id[df$covid])
#df<-df[df$id %in% ids,]
L<-list()
for (ay in 2018:2020) {
    tmp<-df[df$ay==ay,]
    L[[as.character(ay)]]<-f(df=tmp,y=ay)
}

tab<-list()
for (i in 1:2) {
    for (g in 1:4) {
        tmp1<-L[[i]]
        tmp1<-tmp1[tmp1$grade==g,]
        tmp2<-L[[i+1]]
        tmp2<-tmp2[tmp2$grade==g+1,]
        ##
        id1<-unique(tmp1$id)
        id2<-unique(tmp2$id)
        print(length(unique(c(id1,id2))))
        ids<-intersect(id1,id2)
        n1<-length(id1[!(id1 %in% ids)])
        n2<-length(id2[!(id2 %in% ids)])
        n12<-length(ids)
        tab[[paste(names(L)[i],g)]]<-c(n1,n12,n2)
    }
}

#for (i in 1:length(tab)) tab[[i]]<-tab[[i]]/sum(tab[[i]])
tab<-do.call("rbind",tab)
tab<-cbind(c(1:4,1:4),tab)


ss<-tab[,2]+tab[,3]
#tab[,2]<-tab[,2]/ss
#tab[,3]<-tab[,3]/ss
#tab[,4]<-tab[,4]/ss
tab<-cbind(tab[,1],ss,100*tab[,2]/ss,tab[,4])


library(xtable)
print(xtable(tab,digits=1,display=c("d","d","d","f","d")
             ),include.rownames=FALSE)


##################################################################################3
load("df.Rdata")
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
group<-list()
covid.ids<-unique(df$id[df$covid])
id0<-unique(df$id[df$ay==2019])
group$`No obs`<-id0[!(id0 %in% covid.ids)]
id2019<-intersect(id0,unique(df$id[df$ay==2019 & df$covid]))
id2020<-intersect(id0,unique(df$id[df$ay==2020]))
group$`Obs in 2020F`<-id2020[!(id2020 %in% id2019)]
group$`Obs in 2020S`<-id2019


pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/main_fig7.pdf",width=6.5,height=5)
par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=c(2,.2,.2,.2))
nms<-c('Obs in 2020F','Obs in 2020S')
fancy.nms<-list('Observation in fall 2020','Observation in spring 2020')
names(fancy.nms)<-nms
for (g in 1:4) {
    z<-df[df$grade==g & df$ay==2019 & !df$covid,]
    plot(NULL,xlim=c(0,200),#quantile(z$score,c(.01,.99)),
         ylim=c(0,1),xlab="",ylab="Probability of observation",yaxt='n')
    axis(side=2,at=seq(0,1,by=.5))
    mtext(side=1,line=2,"Oral reading fluency score",cex=.8)
    cols<-c("black","blue")
    for (nm in nms) {
        i<-match(nm,nms)
        z$obs<-z$id %in% group[[nm]]
        M<-by(z$obs,z$nces,mean)
        z<-z[z$nces %in% names(M)[M>0],]
        z<-z[z$m %in% 9:11,]
        mm<-by(z$score,z$id,mean)
        tmp<-data.frame(id=names(mm),mm=as.numeric(mm))
        z<-merge(z,tmp)
        m<-loess(obs~mm,z)
        xv<-range(z$mm)
        xv<-seq(xv[1],xv[2],length.out=100)
        yv<-predict(m,data.frame(mm=xv))
        lines(xv,yv,lwd=2,col=cols[i],lty=i)
    }
    if (g==4) legend("topright",bty='n',lty=c(1,2),col=cols,unlist(fancy.nms[nms]),cex=.8)
    legend("topleft",bty='n',paste("Grade",g),cex=.8)
}
dev.off()

