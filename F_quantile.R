f<-function(df,y,maxtime=90) {
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    print(yy)
    df$del<-difftime(df$t,yy,units='days')
    df<-df[df$del>0 & df$del<(maxtime),]
    df$del<-as.numeric(df$del)
    ######################################
    library(lfe)
    m0<-felm(score~del|id+title,df)
    library(quantreg)
    ##demean
    mm<-by(df$score,df$id,mean,na.rm=TRUE)
    tmp<-data.frame(id=names(mm),mm=as.numeric(mm))
    df<-merge(df,tmp)
    df$score.demean<-df$score-df$mm
    out<-list()
    for (tau in seq(0.05,0.95,by=0.2)) {
        print(tau)
        m<-rq(score.demean~0+del+factor(title),data=df,tau=tau)
        out[[as.character(tau)]]<-c(tau,coef(m)[1])
    }
    list(coef(m0)[1],do.call("rbind",out))
}

load("df.Rdata")
id2020<-df$id[df$ay==2020 & df$covid]
df<-df[df$id %in% id2020,]

L<-list()
for (gr in 2:3) {
    tmp<-df[df$grade==gr,]
    #x2019<-f(2019,df=tmp)
    x2020<-f(2020,df=tmp)
    #L[[as.character(gr)]]<-c(x2019,x2020)
}
tab<-do.call("rbind",L)
