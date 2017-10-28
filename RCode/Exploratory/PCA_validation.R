rm(list =ls())

if (Sys.info()['user']=="cindycheng"){
  pathCode="~/Documents/Papers/ForeignAid/RCode";
  pathResults = '~/Dropbox/ForeignAid/Results'
  pathData = '~/Dropbox/ForeignAid/data'}
 
if (Sys.info()['user'] == 'cindy'){
  pathCode="/home/cindy";
  pathResults = "/home/cindy"
}

if (Sys.info()['user'] == 's7m'){
  pathCode="~/Research/ForeignAid/RCode";
  pathResults = "~//Dropbox/Research/ForeignAid/Results"
  pathData = "~//Dropbox/Research/ForeignAid/data"
}

# load packages
source(paste0(pathCode, "/setup.R"))

# load data
load(paste0(pathResults, '/PCA/PCA_FullData_midWarArmsSum.rda'))
load(paste0(pathResults, '/PCA/PCA_FullData_allyIGOUN.rda'))
load(paste0(pathData, '/components/EUgene.rda'))


######################################################

######################################################


######  Extract PCA data  ##
PCA_All = PCA_FullData[[1]]
names(PCA_All)[4:length(PCA_All)] = c("PCA", "PCA_upper", "PCA_lower")
PCA_All$dcode = paste(PCA_All$ccode1, PCA_All$ccode2, sep = "_")

 
# # make unique dyad ID
countries = union(PCA_All$ccode1, PCA_All$code2)

dyadAll = expand.grid(countries, countries)  
dyadAll$Var1 = as.character(dyadAll$Var1); dyadAll$Var2 = as.character(dyadAll$Var2)
dyadAll= dyadAll[-which(dyadAll[,1] == dyadAll[,2]),]
dyadAll$dname = paste(dyadAll$Var1, dyadAll$Var2, sep = "_")

dyadAll$dyadID = 0
for ( i in 1:dim(dyadAll)[1] ) {
  if(dyadAll$dyadID[i] ==0){   
    dyadAll$dyadID[which(dyadAll$Var1 %in% c(dyadAll[i,]) & dyadAll$Var2 %in% c(dyadAll[i,]))] = i
  }
}

PCA_All$dyadID = dyadAll$dyadID[match(PCA_All$dcode, dyadAll$dname)]
PCA_All$cname_1=countrycode(PCA_All$ccode1, 'cown', 'country.name')
PCA_All$cname_2=countrycode(PCA_All$ccode2, 'cown', 'country.name')
PCA_All$PCAStd = PCA_All$PCA/max(PCA_All$PCA, na.rm = T) 
############################################################

############################################################
 

# subset Eugene Data data 

sscores = data[, c('ccode1', 'ccode2', 'year', 'tau_glob', 's_un_glo', 's_wt_glo')]
sscores$s_wt_glo[which(sscores$s_wt_glo == -9)]<-NA
sscores$s_un_glo[which(sscores$s_un_glo == -9)]<-NA
sscores$tau_glob[which(sscores$tau_glob == -9)]<- NA
 

############################################################

############################################################

# Merge Datsets
validate = merge(PCA_All, sscores, by = c('ccode1', 'ccode2', 'year'), all = T)
names(validate)[4] = "PCA"
 
summary(validate) 
# map to 0 1 scale, divide by max 
validate$PCAStd = validate$PCA - min(validate$PCA, na.rm = T)
validate$PCAStd  = validate$PCAStd /max(validate$PCAStd , na.rm = T)
 

############################################################

############################################################

 
# Evaluate PCA vis a vis S-scores and Tau - B

# s-scores
sWt_p = lm(s_wt_glo ~ PCAStd  , data = validate)
sWt_y_p = lm(s_wt_glo ~ PCAStd + as.factor(year) , data = validate)
 
s_p = lm(s_un_glo ~ PCAStd  , data = validate)
s_y_p = lm(s_un_glo ~ PCAStd + as.factor(year) , data = validate)

# tau - B
t_p = lm(tau_glob ~ PCAStd + as.factor(year), data = validate)
t_y_p =lm(tau_glob ~ PCAStd, data = validate)
 
 
# Compare against relationship between S-Scores and Tau-B
summary(lm(s_wt_glo~ tau_glob + as.factor(year), data = validate))
summary(lm(s_wt_glo~ tau_glob + as.factor(year) + as.factor(ccode1), data = validate))
summary(lm(s_wt_glo~ tau_glob, data = validate))

summary(lm(s_un_glo~ tau_glob + as.factor(year), data = validate))
summary(lm(s_un_glo~ tau_glob, data = validate))
summary(lm(s_un_glo~ tau_glob, data = sscores))
summary(lm(tau_glob~ s_un_glo, data = sscores))
 
texreg(list(s_p, s_y_p, sWt_p,sWt_y_p, t_p, t_y_p),      
       dcolumn=FALSE,
       custom.model.names=c("Unweighted S Scores","Unweighted S Scores","Weighted S Scores", "Weighted S Scores", "Tau-B", "Tau-B"),
      )
 

############################################################

############################################################ 


# Evaluate the PCA for different triads of countries
# Political Strategic Interest

setwd(pathGraphics)
pdf("dyadic_USIsraelIran_allyIGOUN.pdf")
plotSub(c(2, 630, 666), PCA_All , ylab = "Political Strategic Interest")
dev.off()


setwd(pathGraphics)
pdf("dyadic_ChinaJapNK_allyIGOUN.pdf")
plotSub(c(731, 710, 740), PCA_All  , ylab = "Political Strategic Interest")
dev.off()

setwd(pathGraphics)
pdf("dyadic_USIndPak_allyIGOUN.pdf")
plotSub(c(750, 770, 2), PCA_All, ylab = "Political Strategic Interest")
dev.off()



# Military Strategic Interest 
setwd(pathGraphics)
pdf("dyadic_USIsraelIran_midWarArmsSum.pdf")
plotSub(c(2, 630, 666), PCA_All , ylab = "Military Strategic Interest")
dev.off()


setwd(pathGraphics)
pdf("dyadic_ChinaJapNK_midWarArmsSum.pdf")
plotSub(c(732, 710, 740), PCA_All  , ylab = "Military Strategic Interest")
dev.off()


setwd(pathGraphics)
pdf("dyadic_USIndPak_midWarArmsSum.pdf")
plotSub(c(750, 770, 2), PCA_All, ylab = "Military Strategic Interest")
dev.off()
 
  
plotSub(c(750, 710, 770), PCA_All)
plotSub(c(2, 101, 40),  PCA_All)

 