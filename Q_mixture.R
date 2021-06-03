est<-c(0.181,0.159,0.115,0.082)
s<-c(0.005,.006,007,.008)
out<-tab<-list()
rates<-c(0,0.05,0.1,0.2)

for (bad in c(0,-0.05)) {
    for (i in 1:length(est)) {
        ee<-numeric()
        for (rate in rates) {
            #n<-10000000
            #n2<-round(rate*n)
            #n1<-round((1-rate)*n)
            #theta<-rnorm(n1,mean=est[i],sd=s[i])
            #z<-rep(bad,n2)
            ee[as.character(rate)]<-est[i]*(1-rate)+bad*rate#mean(c(theta,z))
        }
        tab[[paste(i)]]<-ee
    }
    out[[as.character(bad)]]<-tab
}

tab<-lapply(out,function(x) do.call("rbind",x))

pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/main_fig8.pdf",width=6.5,height=4)
par(mfrow=c(1,2),mar=c(3,4,1,1),mgp=c(2,1,0),oma=rep(.5,4))
library(gplots)
cols<-colorRampPalette(c("blue", "lightblue"))( 4 ) ## (n)
for (i in 1:length(tab)) {
    z<-t(tab[[i]])
    barplot2(z,beside=TRUE,
             col=cols,
             xlab="Grade",
             ylab="Growth in oral reading fluency\n(words per minute/day)",
             ylim=c(0,.22))
    legend("topleft",bty='n',legend=paste('Growth=',names(out)[[i]],sep=''),cex=.6)
    legend("topright",bty='n',fill=cols,legend=rates,title='Missingness',cex=.6)
}
dev.off()



tab<-do.call("rbind",tab)
tab<-cbind(c(1:4,1:4),tab)


library(xtable)
print(xtable(tab,digits=2,display=c("d","d",rep("f",4))
             ),include.rownames=FALSE)




##example
effect<-0.18
se<-0.005
rate.missing<-0.1
n<-100000
n2<-round(rate.missing*n)
n1<-round((1-rate.missing)*n)
theta<-rnorm(n1,mean=effect,sd=se)
z<-rep(0,n2)
mean(c(theta,z))
