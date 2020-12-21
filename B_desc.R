load("df.Rdata")
f<-function(df) {
    n1<-length(unique(df$id))
    n2<-mean(as.numeric(table(df$id)))
    c(n1,n2)
}
f(df)

L<-split(df,df$grade)
tab<-lapply(L,f)
tab<-do.call("rbind",tab)

library(xtable)
print(xtable(tab,digits=2,display=c("d","d","f")
             ),include.rownames=TRUE)


##numbers
load("df.Rdata")
ids<-unique(df$id[df$covid])
df<-df[df$id %in% ids,]
M<-list()
for (y in 2017:2020) {
    ## yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    ## print(yy)
    ## del<-x$time-yy
    ## x$del<-difftime(x$time,yy,units='days')
    ## z<-x[x$del>0 & x$del<365,]
    z<-df[df$ay==y,]
    nr<-nrow(z)
    ns<-length(unique(z$id))
    M[[as.character(y)]]<- c(y,ns,nr)
    #tmp<-x[x$del<30 & x$del>0,]
    #M[[as.character(y)]]<-mean(tmp$score,na.rm=TRUE)
}
tab<-do.call("rbind",M)

library(xtable)
print(xtable(tab,digits=2,display=c("d","d","d","d")
             ),include.rownames=FALSE)


##################################################
##growth
load("df.Rdata")
y<-2018
df<-df[df$ay==y,]
yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
print(yy)
df$del<-as.numeric(difftime(df$t,yy,units='days'))
df<-df[df$del>0 & df$del<(365-90),]
tab<-list()
for (g in 0:7) {
    library(lfe)
    m<-felm(score~del|title+id,df[df$grade==g,])
    tab[[as.character(g)]]<-c(c(g,coef(m),1/coef(m)))
}
tab<-do.call("rbind",tab)
library(xtable)
print(xtable(tab,digits=3,display=c("d","d","f","d")
             ),include.rownames=FALSE)


############################################################
load("df.Rdata")
ids<-unique(df$id[df$covid])
df<-df[df$id %in% ids,]
##average score by month
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
out<-list()
for (y in 2018:2019) {
    for (g in 0:7) {
        tmp<-df[df$ay==y & df$grade==g,]
        NN<-nrow(tmp)
        for (m in c(9:12,1:8)) {
            z<-tmp[tmp$m==m,]
            ns<-nrow(z)
            ms<-mean(z$score,na.rm=TRUE)
            out[[paste(y,g,m)]]<-c(y,g,m,ms,ns)
        }
    }
}
z<-data.frame(do.call("rbind",out))
names(z)<-c("y","g","m","ms","ns")
z$m<-factor(z$m,ordered=TRUE,levels=c(9:12,1:8))

pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/ns.pdf",width=6,height=8)
par(mfrow=c(4,2),mar=c(3,3,1,1),mgp=c(2,1,0))
for (g in 0:7) {
    plot(NULL,xlim=c(1,12),ylim=c(0,max(z$ns)),xlab='month of AY',ylab="# Readings")
    x<-z[z$g==g & z$y==2018,]
    x<-x[order(x$m),]
    lines(x$ns,col='darkgray',lwd=3)
    x<-z[z$g==g & z$y==2019,]
    x<-x[order(x$m),]
    lines(x$ns,col='red',lwd=3)
    abline(v=7,col='red')
    abline(v=9,col='gray')
    legend("topleft",bty='n',paste('grade',g))
}
dev.off()

pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/ms.pdf",width=6,height=8)
par(mfrow=c(4,2),mar=c(3,3,1,1),mgp=c(2,1,0))
for (g in 0:7) {
    plot(NULL,xlim=c(1,12),ylim=c(min(z$ms,na.rm=TRUE),max(z$ms,na.rm=TRUE)),xlab='month of AY',ylab="Mean Score")
    x<-z[z$g==g & z$y==2018,]
    x<-x[order(x$m),]
    lines(x$ms,col='darkgray',lwd=3)
    x<-z[z$g==g & z$y==2019,]
    x<-x[order(x$m),]
    lines(x$ms,col='red',lwd=3)
    abline(v=7,col='red')
    abline(v=9,col='gray')
    legend("topleft",bty='n',paste('grade',g))
}
dev.off()

