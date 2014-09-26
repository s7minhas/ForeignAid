rm(list=ls())

#######  Load S Scores ####### 
setwd("~/Documents/DataSets")
sscores<-read.table("EUG_1816_2008.out", header=T)
load("EUgene.rda")


summary(sscores)
sscores$s_un_reg[which(sscores$s_un_reg==-9)]<-NA
sscores$s_un_glo[which(sscores$s_un_glo==-9)]<-NA
sscores$s_wt_reg[which(sscores$s_wt_reg==-9)]<-NA
sscores$s_wt_glo[which(sscores$s_wt_glo==-9)]<-NA 

contig<-data[, c('ccode1', 'ccode2', 'year', 'contig')]
contig$contigland<-contig$contig
contig$contigland[which(contig$contig>3)]<-0
contig$contigland[which(contig$contig<=3)]<-1
contig$contigland[which(contig$contig==-9)]<-NA

contigextrapolate<-rbind(data.frame(contig[which(contig$year==2002),c('ccode1', 'ccode2', 'contigland')], year=2003), data.frame(contig[which(contig$year==2002),c('ccode1', 'ccode2', 'contigland')], year=2004), data.frame(contig[which(contig$year==2002),c('ccode1', 'ccode2', 'contigland')], year=2005), data.frame(contig[which(contig$year==2002),c('ccode1', 'ccode2', 'contigland')], year=2006), data.frame(contig[which(contig$year==2002),c('ccode1', 'ccode2', 'contigland')], year=2007), data.frame(contig[which(contig$year==2002),c('ccode1', 'ccode2', 'contigland')], year=2008))
contig1<-rbind(contig[, c('ccode1', 'ccode2', 'contigland', 'year')], contigextrapolate)

sscores1<-merge(sscores, contig1, by=c("ccode1", "ccode2", "year"), all=T)
 
summary(sscores1)
unique(sscores1$year[which(is.na(sscores1$s_un_glo))])
 
####### Load CINC data ####### 
cinc<-read.csv('NMC_v4_0.csv')
cinc1<-cinc[, c('ccode', 'year', 'cinc')]
names(cinc1)[1]<-"ccode2"
cinc1$cinc[which(cinc1$cinc==-9)]<-NA
# check - cinc1[which(cinc1$cinc>.2),]

#######  Load Ally Data ####### 
setwd("/Users/cindycheng/Documents/Papers/ForeignAid/version4.1_comma")
ally <- read.csv("alliance_v4.1_by_directed_yearly.csv")
ally1<- ally[ally$year>=1970, c('ccode1', 'ccode2', 'year', 'defense', 'neutrality', 'nonaggression', 'entente')]

ally1$ally<-rowSums(ally1[, 4:7])
ally1$allydum<-0
ally1$allydum[which(ally1$ally>0)]<-1

 
 
#######  Load Major Powers Data ####### 
setwd("~/Downloads")
major<-read.csv("majors2011.csv")
year<-seq(1945, 20012)

b<-c()
for (i in 1:length(levels(major$stateabb))){
a<-cbind(levels(major$stateabb)[i], major$ccode[i], year)
b<-rbind(b, a)
}

 
majpwr<-data.frame(b)
majpwr$majpwr<-1
names(majpwr)[c(1, 2)]<-c("stateabb", "ccode2")
majpwr$year<-as.numeric(as.character(majpwr$year))
 
majpwr$majpwr[c(which(majpwr$stateabb=="GMY" & majpwr$year<1992), which(majpwr$stateabb=="JPN" & majpwr$year<1992), which(majpwr$stateabb=="CHN" & majpwr$year<1951))]<-0
majpwr<-majpwr[-which(majpwr$stateabb=="AUH"|majpwr$stateabb=="ITA"),]
majpwr1<-majpwr[, -1]
majpwr1$ccode2<-as.numeric(as.character(majpwr1$ccode2))


############################################
########### Merge Data #################
############################################


setdiff(sscores$ccode2, cinc1$ccode2) # 347  - Kosovo
setdiff(cinc1$ccode2, sscores$ccode2) # 240 - Hanover 245 - Bavaria 267 - Baden 269 - Saxony 271 - Wuertemburg 273 - Hesse Electoral 275 - Hesse Grand Ducal 280 - Mecklenburg Schwerin 300 - Austria Hungary 327 - Papal States 329 - Two Sicilies 332 - Modena 335 - Parma 337 - Tuscany 511 - Zanzibar 730 - Korea

data<-merge(sscores1, cinc1, by=c("ccode2", "year"), all.x=T)
data1<-merge(data, ally1, by=c("ccode1", "ccode2", "year"), all=T)
data2<-merge(data1, majpwr1, by =c("ccode2", "year"), all=T)
data2$majpwr[which(is.na(data2$majpwr))]<-0
data2$allydum[which(is.na(data2$allydum))]<-0
data2$pol_rel = 0
data2$pol_rel[which(data2$majpwr==1 | data2$contigland==1)]<-1
data2<-data2[which(data2$year<2001&data2$year>1969),]
summary(data2)


 
####### Real Dealio############
##### Create Threat Environment Variable/Replicate Threat Variable
 
year1<-seq(1970, 2008)
stateA<-unique(data2$ccode1)
cinq<-list()
cinq1<-list()
cinqsum<-list()
for (i in 1:length(stateA)){
for (k in 1: length(year1)){
cinq1[[k]]<-data2$cinc[which(data2$ccode1==stateA[i] & data2$year==year1[k] & data2$s_wt_glo<median(data2$s_wt_glo[which(data2$year==year1[k])], na.rm=T) & data2$pol_rel==1& data2$allydum == 0 )]	
}
cinq[[i]]<-cinq1
cinqsum[[i]]<-lapply(cinq1, sum, na.rm=T)
}
cinqsum1<-unlist(cinqsum)
 
 
# Create Standardized Threat Environment Variable 

cinqZ<-list()
cinqZ1<-list()
cinqZsum<-list()
maxtotal<-c()
for (i in 1:length(stateA)){
maxtotal[i]<-max(unlist(cinqsum[[i]]), na.rm=T)	
for (k in 1:length(year1)){
cinqZ1[[k]]<-cinq[[i]][[k]]/maxtotal[i]
}
cinqZ[[i]]<-cinqZ1
}
cinqZuse<-unlist(cinqZ)
 
 
##### Create Herfindal Index
 
# Calculate Max for the standardized varibales

 
maxS<-list()
S<-list()
lengthU<-list()
lengthS<-list()
for ( i in 1:length(cinq)){
maxS[[i]]<-lapply(cinqZ[[i]], max, na.rm=TRUE)
lengthS[[i]]<-lapply(cinqZ[[i]], length)
} 
  

maxS1<-unlist(maxS)
maxS2<-maxS1
maxS2[which(is.infinite(maxS1))]<-0
lengthS1<-unlist(lengthS)
restS<-check1-maxS2


square<-function(x){
	return(x^2)
}
 
herfOS1<-list() 
herfOS<-list() # O: Original, S: Standardized
for ( i in 1:length(cinq)){
	herfOS1[[i]]<-lapply(cinqZ[[i]],square )
	herfOS[[i]]<-lapply(herfOS1[[i]],sum )
}




# create index
index1<-list()
for( i in 1:length(unique(stateA))){
index1[[i]]<-cbind(rep(stateA[i], length(seq(1970, 2008))), seq(1970, 2008))
}
index<-do.call(rbind,index1)
 

herf<-data.frame(ccode1=index[,1],year=index[,2], herfOS= unlist(herfOS))

strategic<-merge(data2, herf, by=c("ccode1", "year") )
strategic$strategic <- strategic$s_wt_glo * strategic$herfOS

summary(strategic)

strategic[which(strategic$s_wt_glo==1 & strategic$herfOS==1),]
summary(herf)
herf$herfOS[which(herf$ccode==2)]
plot(1970:2008, herf$herfOS[which(herf$ccode==2)], type="l")
plot(1970:2000, herf$herfOS[which(herf$cow==365)], type="l")
plot(1970:2000, herf$herfOS[which(herf$cow==666)], type="l")
plot(1970:2000, herf$herfOS[which(herf$cow==710)], type="l")
plot(1970:2000, herf$herfOS[which(herf$cow==740)], type="l")
plot(1970:2000, herf$herfOS[which(herf$cow==713)], type="l")
plot(1970:2000, herf$herfOS[which(herf$cow==820)], type="l")

summary(data2)
summary(cinc)
herf$herfOS[which(herf$cow==666)]
herf[which(herf$year==2001),4:length(herf)]<-NA

setwd("~/Documents/DataSets")
write.csv(strategic, "strategic.csv")

 

#### Take a look at countries included in threat environment
 
exthreat<-function(dataset, state1, state2, year, sscore, polrel, atop){
year1<-seq(1980, 2000)
stateA<-unique(state1)
threat1<-list()
threat<-list()
for (i in 1:length(stateA)){
for (k in 1: length(year1)){
threat1[[k]]<-state2[which(state1==stateA[i] & year==year1[k] & sscore<median(sscore[which(year==year1[k])], na.rm=T) & polrel==1& atop!=1 )]	
}
threat[[i]]<-threat1
}
return(threat)
} 
 
all<-exthreat(data2, data2$ccode1, data2$ccode2, data2$year, data2$s_wt_glo, data2$pol_rel, data2$atopally)
 
test1<-exthreat(test$ccode1, test$ccode2, test$year, test$s_wt_glo, test$pol_rel, test$atopally)
 
save(all, file="extthreatcountry_aug13.Rdata")
setwd("~/Documents/PhD Extracurriculars/RA Work/Grieco Research/Summer 2012/data/ATOP3_0EUGdta")
load("extthreatcountry.Rdata")


data2[which(data2$ccode1==200 & data2$year==1998 & data2$s_wt_glo<median(data2$s_wt_glo[which(data2$year==1998)], na.rm=T) & data2$pol_rel==1& data2$allydum == 0 ),]

data2[which(data2$ccode1==2 & data2$year==1976 & data2$s_wt_glo<median(data2$s_wt_glo[which(data2$year==1976)], na.rm=T) & data2$pol_rel==1& data2$allydum == 0 ),]

 


 