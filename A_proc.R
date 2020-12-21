"final_dataset_for_ben_post_2017.csv"->fn
read.csv(fn)->x

df<-data.frame(id=x$anon_id,grade=x$grade_of_student,title=x$title,score=x$raw_reading_rate,nces=x$nces_code)

t<-strsplit(x$time_uploaded,"T")
t<-sapply(t,"[",1)
t<-strptime(t,format="%Y-%m-%d")
df$t<-t

##march 11 2020
df$covid<-df$t>"2020-03-11"

##which ay
yy<-strptime(paste(2017:2020,"-09-01",sep=""),format="%Y-%m-%d")
del<-list()
for (i in 1:length(yy)) del[[i]]<-difftime(df$t,yy[i],units='days')
del<-do.call("cbind",del)
f<-function(x) {
    z<-which(x>0 & x<365)
    if (length(z)==0) NA else z
}
z<-apply(del,1,f)
n<-sapply(z,length)
if (!all(n==1)) stop()
z<-ifelse(is.na(z),5,z)
df$ay<-c(2017:2020,NA)[z]

df$grade<-as.numeric(ifelse(df$grade=='K',0,df$grade))

df<-df[df$grade<8,]
df<-df[!is.na(df$ay),]

save(df,file="df.Rdata")
