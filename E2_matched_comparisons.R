## load("df.Rdata")
## ids<-unique(df$id[df$covid & df$ay==2019])
## df<-df[df$id %in% ids,]
## g<-2
## df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))

## par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,1,0))
## for (g in 1:4) {
##     L<-list()
##     for (y in 2018:2019) {
##         z<-df[df$ay==y & df$grade==g,]
##         m1<-z[z$m %in% 9:10,]
##         m<-by(m1$score,m1$id,mean)
##         m1<-data.frame(id=names(m),m1=as.numeric(m))
##         m2<-z[z$m %in% 4:5,]
##         m<-by(m2$score,m2$id,mean)
##         m2<-data.frame(id=names(m),m2=as.numeric(m))
##         print(c(nrow(m1),nrow(m2)))
##         z<-merge(m1,m2)
##         L[[as.character(y)]]<-z
##     }
##     lapply(L,nrow)
##     ##
##     x<-L$`2018`
##     m<-loess(m2~m1,x)
##     xv<-range(x$m1)
##     xv<-seq(xv[1],xv[2],length.out=100)
##     yv<-predict(m,data.frame(m1=xv))
##     plot(xv,yv,type='l')
##     x<-L$`2019`
##     m<-loess(m2~m1,x)
##     xv<-range(x$m1)
##     xv<-seq(xv[1],xv[2],length.out=100)
##     yv<-predict(m,data.frame(m1=xv))
##     lines(xv,yv,col='red')
## }

#######################################
##densities
load("df.Rdata")
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
ids<-unique(df$id[df$covid & df$ay==2019])
y<-2019
L<-list()
for (g in 1:4) {
    z<-df[df$ay==y & df$grade==g,]
    m1<-z[z$m %in% 9:11,]
    m<-by(m1$score,m1$id,mean)
    m1<-data.frame(id=names(m),m1=as.numeric(m))
    m1$covid<-m1$id %in% ids
    L[[as.character(g)]]<-m1
}

paceblue<-rgb(0,81,138,max=255)
stansun<-rgb(234,171,0,max=255)
pacered<-rgb(161, 31, 65,max=255)

                                        #pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/fallmean_densities.pdf",width=4,height=4)
pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/si_fig4.pdf",width=6.5,height=4)
par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,1,0))
for (i in 1:length(L)) {
    z<-L[[i]]
    plot(density(z$m1[!z$covid]),col=paceblue,type='l',lwd=3,main='',sub='',xlab="Oral reading fluency")
    lines(density(z$m1[z$covid]),col=pacered,lwd=3)
    if (i==1) {
        legend("topright",bty='n',title=paste("Grade",names(L)[i]),legend=c("Observed","Not observed"),fill=c(pacered,paceblue),cex=.85)
    } else {
        legend("topright",bty='n',title=paste("Grade",names(L)[i]),legend='',cex=.85)
    }
}
dev.off()

