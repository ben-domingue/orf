load("df.Rdata")
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))

base.grade<-1

L<-list()
for (base.year in 2017:2018) {
    print(base.year)
    id<-list()
    for (i in 0:(2020-base.year)) {
        y<-base.year+i
        g<-base.grade+i
        tmp<-df[df$ay==y & df$grade==g,]
        if (i==0) {
            keepers<-unique(tmp$id)
        } else {
            tmp<-tmp[tmp$id %in% keepers,]
        }
        tmp1<-tmp[tmp$m %in% 9:12,]
        id[[paste(i,'fall')]]<-length(unique(tmp1$id))
        tmp2<-tmp[tmp$m %in% 3:6,]
        id[[paste(i,'spring')]]<-length(unique(tmp2$id))
    }
    z<-unlist(id)
    L[[as.character(base.year)]]<-z[-length(z)]
}

x2017<-L[[1]][-1] #no fall 2017
x2018<-c(NA,L[[2]])
txt<-c("Spring\n2018","Fall\n2018","Spring\n2019","Fall\n2019","Spring\n2020","Fall\n2020")

pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/main_fig1.pdf",width=5,height=4)
par(mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(0.4,4))
plot(x2017,type='l',ylim=c(0,max(c(x2017,x2018),na.rm=TRUE)),xaxt='n',xlab='',ylab="Number of students",lwd=3,col='blue',bty='n',xlim=c(1,8))
n<-length(x2017)
text(n,x2017[n],pos=4,'Fourth graders\n2020-21',cex=.9)
lines(x2018,lwd=3,col='blue')
n<-length(x2018)
text(n,x2018[n],pos=4,'Third graders\n2020-21',cex=.9)
abline(v=4.5,col='red')
for (i in 1:length(txt)) mtext(side=1,line=1,at=i,txt[i],cex=.9)
#axis(side=1,at=1:length(txt),txt,cex=.7,line=1)
for (i in seq(0,5000,by=500)) segments(0,i,6,i,col='lightgray')
dev.off()



         
