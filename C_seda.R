x<-read.csv("seda_geodist_pool_cs_v30.csv",header=TRUE)
x<-x[x$subgroup=='all',]
tmp<-x[,c("leaidC","mn_avg_ol")]

load("df.Rdata")
L<-split(df,df$nces)
n1<-sapply(L,nrow)
n2<-sapply(L,function(x) length(unique(x$id)))


tmp[,3]<-tmp[,1] %in% unique(df$nces)

z.in<-tmp[tmp[,3],]
#z.out<-tmp[!tmp[,3],]

pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/seda.pdf",width=5,height=3)
par(mgp=c(2,1,0))
hist(tmp[,2],fr=FALSE,main='',xlab="Average Achievement across Districts",ylab='',col='lightblue')
lines(density(z.in[,2]),col='red',lwd=4)
legend("topleft",bty='n',paste(nrow(z.in),"focal districts"),fill='red')
dev.off()
