##geo data. https://nces.ed.gov/programs/edge/Geographic/RelationshipFiles
x<-read.csv("./geo/grf19_lea_county.csv",sep="|")
names(x)<-tolower(names(x))
n<-nchar(x$stcounty)
county<-state<-character()
for (i in 1:nrow(x)) {
    county[i]<-substr(x$stcounty[i],n[i]-2,n[i])
    state[i]<-substr(x$stcounty[i],1,n[i]-3)
}
x$statefips<-as.numeric(state)
x$countyfips<-as.numeric(county)
x<-x[,c("leaid","name_lea19","stcounty","statefips","countyfips")]


##mobility data, https://osf.io/zpshm/
m<-read.csv("./geo/2020_schools_county_csv.csv")
geo<-merge(x,m,all=TRUE)


x<-read.csv("./geo/seda_geodist_pool_cs_v30.csv",header=TRUE)
x<-x[x$subgroup=='all',]

tmp<-x[,c("leaidC","mn_avg_ol")]
names(tmp)<-c('leaid','seda')
geo<-merge(geo,tmp,all=TRUE)

geo$leaid -> geo$nces

x<-read.csv("./geo/SEDA_cov_geodist_pool_v30.csv")
x<-x[,c("leaid","sesavgall")]
names(x)[2]<-'ses'
geo<-merge(geo,x,all=TRUE)

##loking at states that are represented
load("df.Rdata")
z<-geo[!is.na(geo$month) & geo$month==9 & geo$year==2020,]
z<-z[z$nces %in% unique(df$nces),]
length(unique(z$state_abb))

##getting rid of dupes
geo<-geo[,c("nces","month","state_abb","countyfips","year","seda","ses","share_elem_closed_50","share_elem_closed_25")]
id<-paste(geo$nces,geo$month,geo$year)
L<-split(geo,id)
f<-function(x) {
    cm<-colMeans(x[,c("seda","ses","share_elem_closed_50","share_elem_closed_25")],na.rm=TRUE)
    c(nces=unique(x$nces),month=unique(x$month),year=unique(x$year),cm)
}
L<-lapply(L,f)
geo<-data.frame(do.call("rbind",L))

##
save(geo,file="geo.Rdata")

z<-geo[,c("seda","sesavgall","share_elem_closed_50")]
cor(z,use='p')

    
##
x<-geo[geo$month==9,]
plot(x$seda.mean,x$share_elem_closed_50) #Share of elementaryschools in geographic area with at least a 50%year-over-year decline in average visitors per day in given month
