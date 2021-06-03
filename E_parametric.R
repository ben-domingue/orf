##table of Ns
load("df.Rdata")
df<-df[df$ay==2019,]
z<-by(df$covid,df$id,mean,na.rm=TRUE)
tmp<-data.frame(id=names(z),notmiss=as.numeric(z))
df<-merge(df,tmp,all.x=TRUE)
df$notmiss<-ifelse(df$notmiss>0,1,0)
tab<-table(df$grade,df$notmiss)
tab<-tab/rowSums(tab)
library(xtable)
print(xtable(tab,digits=2,display=c("d",rep("f",2))
             ),include.rownames=TRUE)

load("df.Rdata")
df<-df[df$ay==2018,]
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
df$covid<-df$m %in% 5:9
z<-by(df$covid,df$id,mean,na.rm=TRUE)
tmp<-data.frame(id=names(z),notmiss=as.numeric(z))
df<-merge(df,tmp,all.x=TRUE)
df$notmiss<-ifelse(df$notmiss>0,1,0)
tab<-table(df$grade,df$notmiss)
tab<-tab/rowSums(tab)

##################################################
f<-function(df,y,maxtime=9*30) {
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    print(yy)
    df$del<-difftime(df$t,yy,units='days')
    df<-df[df$del>0 & df$del<(maxtime),]
    ######################################
    ##next year
    summerend<-strptime(paste(y+1,"-09-01",sep=''),format="%Y-%m-%d")
    df$nextyear.t<-as.numeric(difftime(df$t,summerend,units='days'))
    df$nextyear<-ifelse(df$nextyear.t>0,1,0)
    ##summer
    summer<-strptime(paste(y+1,"-06-11",sep=''),format="%Y-%m-%d")
    df$summer.t<-as.numeric(difftime(df$t,summer,units='days'))
    df$summer<-ifelse(df$summer.t>0,1,0)
    #fake covid id
    ##march 11 2020
    fc<-strptime(paste(y+1,"-03-11",sep=''),format="%Y-%m-%d")
    df$fc.t<-as.numeric(difftime(df$t,fc,units='days'))
    df$fake.covid<-ifelse(df$fc.t>0,1,0)
    ######################################
    library(lfe)
    df$del<-as.numeric(df$del)
    df$del0<-ifelse(df$fake.covid==0,df$del,0)
    df$del1<-ifelse(df$fake.covid==1 & df$summer==0,df$fc.t,0)
    df$del2<-ifelse(df$summer==1 & df$nextyear==0,df$summer.t,0)
    df$del3<-ifelse(df$nextyear==1,df$nextyear.t,0)
    #m<-felm(score~del0+fake.covid+del1+summer+del2+nextyear+del3|title+id,df)
    m<-felm(score~del0+fake.covid+del1|title+id,df)
    c(as.numeric(t(summary(m)$coef[,1:2])),nrow(m$resid))
}


load("df.Rdata")
#ids<-unique(df$id[df$covid])
#df<-df[df$id %in% ids,]
id2019<-df$id[df$ay==2019 & df$covid]
df<-df[df$id %in% unique(id2019),]
L<-list()
for (gr in 1:4) {
    tmp<-df[df$grade==gr,]
    x2018<-f(2018,df=tmp)
    x2019<-f(2019,df=tmp)
    l<-list(`2018`=x2018,`2019`=x2019)
    L[[as.character(gr)]]<-l
}


L2<-list()
for (i in 1:length(L)) L2[[i]]<-do.call("rbind",L[[i]])
L2<-tab<-do.call("rbind",L2)
z<-numeric()
for (i in c(1,3,5,7)) {
    del<-L2[i,5]-L2[i+1,5]
    z[i]<-del/sqrt(L2[i,6]^2+L2[i+1,6]^2)
}
2*pnorm(abs(z),lower.tail=FALSE)
      
##figure
f<-function(est) {
    y<-2019
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    summerend<-strptime(paste(y+1,"-09-01",sep=''),format="%Y-%m-%d")
    summer<-strptime(paste(y+1,"-06-11",sep=''),format="%Y-%m-%d")
    fc<-strptime(paste(y+1,"-03-11",sep=''),format="%Y-%m-%d")
    d1<-difftime(fc,yy,units='days')
    d2<-difftime(summer,fc,units='days')
    d3<-difftime(summerend,summer,units='days')
    t0<-0:(as.numeric(d1))
    t1<-0:(as.numeric(d2))
    t2<-0:(as.numeric(d3))
    t3<-0:90
    z<-list()
    z[[1]]<-cbind(t0,t0*est[1])
    z[[2]]<-cbind(t1+d1,est[3]+t1*est[5])
    #z[[3]]<-cbind(t2+d1+d2,est[7]+t2*est[9])
    #z[[4]]<-cbind(t3+d1+d2+d3,est[11]+t3*est[13])
    z<-do.call("rbind",z)
    z
}

pdf("/home/bd/Dropbox/projects/orf/docs/orf_covid_tables_figures/main_fig4.pdf",width=6.5,height=6)
par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=rep(.5,4))
for (i in 1:length(L)) {
    x<-L[[i]]
    x0<-f(x[[1]])
    x1<-f(x[[2]])
    plot(x0,pch=19,col='black',cex=.2,ylim=c(-1,35),xlab="Days since September 1 of AY",ylab="Oral reading fluency (words per minute)")
    points(x1,col='blue',pch=19,cex=.2)
    abline(v=192,col='red')
    legend("topleft",bty='n',paste("Grade",names(L)[i]))
    print(100*c(x0[nrow(x0),2],x1[nrow(x1),2]))
}
legend("bottomright",bty='n',fill=c("black","blue"),legend=c("2018-19","2019-20"))
dev.off()


## The accumulated learning loss for second and third graders would leave them 7.3 and
## 7.7 WPM behind their expected level respectively (representing 26 percent and 33 percent of the
## expected yearly gains).
##example
## 3824 3166 
## 2831 2101 
## 2360 1589 
## 1617 1641 
## > legend("bottomright",bty='n',fill=c("black","blue"),legend=c("2018-19","2019-20"))
## > 2831-2101
## [1] 730
## > 730/2831
## [1] 0.26


##getting SD, see email from Reid on 4-2-2021, subject "Cost-Benefit Education Research?"
load("df.Rdata")
#ids<-unique(df$id[df$covid])
#df<-df[df$id %in% ids,]
id2019<-df$id[df$ay==2019 & df$covid]
df<-df[df$id %in% unique(id2019),]
df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
df<-df[df$ay==2018,]
df<-df[df$m %in% 4:5,]
by(df$score,df$grade,sd,na.rm=TRUE)
##effect size units
7.3/30
7.7/30









## load("df.Rdata")
## id2019<-df$id[df$ay==2019 & df$covid]
## #id2020<-df$id[df$ay==2020]
## #ids<-intersect(id2019,id2020)#ids<-unique(df$id[df$covid])
## df<-df[df$id %in% id2019,]
## ##
## L<-list()
## for (gr in 1:5) L[[paste(2018,gr)]]<-f(2018,df=df[df$grade==gr,])
## tab1<-do.call("cbind",L)
## L<-list()
## #L$`2019`<-f(2019,df=df)
## for (gr in 1:5) L[[paste(2019,gr)]]<-f(2019,df=df[df$grade==gr,])
## tab2<-do.call("cbind",L)
## tab<-rbind(t(tab1),t(tab2))

## nms<-c("t0","covid","t1")#,"summer","t2","nextyear","t3")
## zz<-cbind(nms,paste(nms,"se"))
## zz<-c(as.character(t(zz)),"N")
## colnames(tab)<-zz
## ## library(xtable)
## ## print(xtable(tab,digits=3,display=c("s",rep(c("f","e"),7),"d")
## ##              ),include.rownames=TRUE)

## ##stratified by ability
## load("df.Rdata")
## id2019<-df$id[df$ay==2019 & df$covid]
## df<-df[df$id %in% id2019,]
## df$m<-as.numeric(sapply(strsplit(as.character(df$t),"-"),"[",2))
## L<-list()
## for (gr in 1:5) {
##     tmp<-df[df$grade==gr,]
##     z<-tmp[tmp$m %in% 9:11,]
##     qu<-quantile(z$score,(1:2)/3,na.rm=TRUE)
##     ll<-split(z,z$id)
##     z<-data.frame(id=names(ll),max.score=sapply(ll,function(x) max(x$score,na.rm=TRUE)))
##     tmp<-merge(tmp,z)
##     tmp$group<-cut(tmp$max.score,c(-Inf,qu,Inf),labels=1:3)
##     l<-list()
##     for (grp in 1:3) {
##         x2018<-f(2018,df=tmp[tmp$group==grp,])
##         x2019<-f(2019,df=df[tmp$group==grp,])
##         l[[grp]]<-list(`2018`=x2018,`2019`=x2019)
##     }
##     L[[as.character(gr)]]<-l
## }


## #pdf("/home/bd/Dropbox/Apps/Overleaf/ORF-covid/par.pdf",width=4,height=6)
## par(mfrow=c(3,2),mar=c(3,3,1,1),mgp=c(2,1,0))
## txt<-strsplit(rownames(tab)," ")
## txt<-sapply(txt,"[",2)
## nn<-(nrow(tab)/2)
## for (i in 1:nn) {
##     x0<-f(tab[i,])
##     x1<-f(tab[i+nn,])
##     plot(x0,pch=19,col='darkgray',cex=.2,ylim=c(-15,35))
##     points(x1,col='red',pch=19,cex=.2)
##     legend("topleft",bty='n',paste("grade",txt[i]))
## }
## #dev.off()
