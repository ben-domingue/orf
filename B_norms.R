#Table 1  doi:10.1598/RT.59.7.3
#Oral reading fluency norms: A valuable assessment tool for reading teachers
##g2 fall-winter median: 51-72, growth of 21
##g3 fall-winter median: 71-92, growth of 21

load("df.Rdata")
df<-df[df$ay==2019,]
f<-function(df,y,maxtime=30*4) {
    yy<-strptime(paste(y,"-09-01",sep=""),format="%Y-%m-%d")
    print(yy)
    df$del<-difftime(df$t,yy,units='days')
    df<-df[df$del>0 & df$del<(maxtime),]
    ######################################
    library(lfe)
    df$del<-as.numeric(df$del)
    m<-felm(score~del|id+title,df)
    c(as.numeric(t(summary(m)$coef[,1:2])))#,nrow(m$resid))
}
est<-list()
for (i in 2:3) est[[as.character(i)]]<-f(df[df$grade==i,],y=2019)[1]

21/unlist(est)
