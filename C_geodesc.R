load("geo.Rdata")
geo<-geo[!is.na(geo$month) & geo$month==9 & geo$year==2020,]
load("df.Rdata")
geo$indf<-geo$nces %in% unique(df$nces)

###########################################
##seda
f<-function(nm) {
    tmp<-geo[!is.na(geo[[nm]]),]
    #tmp<-geo[,c("nces","seda","state_abb")]
    #tmp<-tmp[!is.na(tmp$seda),]
    n0<-nrow(df)
    zz<-df[df$nces %in% tmp$nces,]
    n1<-nrow(zz)
    per<-n1/n0
    print(per)
    ##
    tmp$indf<-tmp$nces %in% unique(df$nces)
    mm<-by(tmp[[nm]],tmp$indf,mean,na.rm=TRUE)
    ss<-by(tmp[[nm]],tmp$indf,sd,na.rm=TRUE)
    z.in<-tmp[tmp$indf,]
    m<-mean(z.in[[nm]],na.rm=TRUE)
    f<-ecdf(tmp[[nm]])
    qu<-f(m)
    list(per,mm,ss,qu)
}
f('seda')
f('ses')
f("share_elem_closed_50")

##############################################
                                        #pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/seda.pdf",width=7.3,height=3)
pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/si_fig1.pdf",width=6.5,height=5)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(4.5,3,1,1))
fancy<-c(ses="Socioeconomic status",seda="Average achievement",share_elem_closed_50="Proportion of schools with\n50 percent decline in visitors")
for (nm in c("seda","ses","share_elem_closed_50")) {
    nn<-length(!is.na(geo[[nm]]))
    hist(geo[[nm]],fr=FALSE,main='',xlab='',ylab='',col='lightblue')
    mtext(side=1,line=3,fancy[nm],cex=.7)
    z.in<-geo[geo$indf & !is.na(geo[[nm]]),]
    lines(density(z.in[[nm]]),col='red',lwd=4)
    legend(ifelse(nm=='share_elem_closed_50',"topright","topleft"),bty='n',c(paste(nrow(z.in),"focal\ndistricts"),paste(nn,"districts")),fill=c('red',"lightblue"),cex=.8)
}
dev.off()

######
cor(geo[,c("seda","ses","share_elem_closed_50")],use='p')
cor(geo[geo$indf,c("seda","ses","share_elem_closed_50")],use='p')
    
## ##############################################
##comparison between CA and analytic districts
load("geo.Rdata")
geo<-geo[!is.na(geo$month) & geo$month==9 & geo$year==2020,]
load("df.Rdata")
geo$indf<-geo$nces %in% unique(df$nces)
x<-read.csv("./geo/SEDA_cov_geodist_pool_v30.csv")
x<-x[,c("leaid","stateabb","urban","perasn","perhsp","perblk","perfrl","perspeced","perell","totenrl")]
z<-merge(geo,x,by.x='nces',by.y='leaid')

L<-list()
L$ca<-z[z$stateabb=="CA",]
L$`in`<-z[z$indf,]
L$all<-z
f<-function(x) colMeans(x[,c("urban","perasn","perhsp","perblk","perfrl","perspeced","perell","totenrl")],na.rm=TRUE)
lapply(L,f)

lapply(L,function(x) summary(x$totenrl))
lapply(L,function(x) sum(x$totenrl))

## ##############################################
## ##parolin
## dim(geo)
## g1<-geo$share_elem_closed_50
## mean(geo$share_elem_closed_25,na.rm=TRUE)
## mean(geo$share_elem_closed_50,na.rm=TRUE)
## load("df.Rdata")
## n0<-nrow(df)
## zz<-df[df$nces %in% geo[,1],]
## n1<-nrow(zz)
## per.geo<-n1/n0

## geo<-geo[geo$leaid %in% unique(df$nces),]
## dim(geo)
## mean(geo$share_elem_closed_25,na.rm=TRUE)
## mean(geo$share_elem_closed_50,na.rm=TRUE)
## g2<-geo$share_elem_closed_50

## ######################################################
## #r between closure and seda seda
## cor(geo$seda,geo$share_elem_closed_50,use='p')

