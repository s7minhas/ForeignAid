if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
  source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

library(texreg)


plotSub <-function(csk, data, ylab, stratName){

  # convert country codes to country names and 2-country combination 
  countrynames = countrycode(csk, 'cown', 'country.name')
  countrynames[which(csk==731)] = 'North Korea'
  dyadComb = t(combS(countrynames, 2))
  dyad = data.frame(paste(dyadComb[,1], dyadComb[,2], sep = "-"), dyadComb,  t(combS(csk, 2)))
  names(dyad) = c("dyadName", "cname_1", "cname_2", 'ccode1', 'ccode2')
  
  triad<- data[which(data$ccode1 %in% csk & data$ccode2 %in% csk), c('ccode1', 'ccode2', 'year', stratName)]
  triad = merge(triad, dyad, by = c("ccode1", "ccode2"))

 
  yrLabels = seq(min(triad$year), max(triad$year), 7)  
  ggplot(triad, aes(x=year, y=eval(parse(text = stratName)), color=dyadName)) +
    geom_line() +
    geom_point() + 
    scale_y_continuous(ylab) +
    scale_x_continuous('', breaks=yrLabels, labels=yrLabels) +
    theme(
      axis.ticks=element_blank(),
      legend.position="bottom",
      legend.title=element_blank(),
      panel.border=element_blank()
      )}

 
############################
# Load cosineDist
load(file = paste0(pathTnsr, '/tnsrSpace/cosineDist_agree2_idealPtun_DefEntSum_igoWt.rda'))

# load EUgene data
load(paste0(pathData, '/components/EUgene.rda'))


############################################################
# subset Eugene Data data 

sscores = data[, c('ccode1', 'ccode2', 'year', 'tau_glob', 's_un_glo', 's_wt_glo')]
sscores$s_wt_glo[which(sscores$s_wt_glo == -9)]<-NA
sscores$s_un_glo[which(sscores$s_un_glo == -9)]<-NA
sscores$tau_glob[which(sscores$tau_glob == -9)]<- NA
sscores = sscores[which(sscores$year>1974),] 

############################################################

# Merge Datsets
validate = merge(cosDistU1, sscores, by = c('ccode1', 'ccode2', 'year'), all = T)
 
############################################################

############################################################

 
# Evaluate PCA vis a vis S-scores and Tau - B
 
# s-scores
sWt_p = lm(s_wt_glo ~ stratInt  , data = validate)
sWt_y_p = lm(s_wt_glo ~ stratInt + as.factor(year) , data = validate)
 
s_p = lm(s_un_glo ~ stratInt  , data = validate)
s_y_p = lm(s_un_glo ~ stratInt + as.factor(year) , data = validate)

# tau - B
t_p = lm(tau_glob ~ stratInt + as.factor(year), data = validate)
t_y_p =lm(tau_glob ~ stratInt, data = validate)
 
 
# # Compare against relationship between S-Scores and Tau-B
# summary(lm(s_wt_glo~ tau_glob + as.factor(year), data = validate))
# summary(lm(s_wt_glo~ tau_glob + as.factor(year) + as.factor(ccode1), data = validate))
# summary(lm(s_wt_glo~ tau_glob, data = validate))

# summary(lm(s_un_glo~ tau_glob + as.factor(year), data = validate))
# summary(lm(s_un_glo~ tau_glob, data = validate))
# summary(lm(s_un_glo~ tau_glob, data = sscores))
# summary(lm(tau_glob~ s_un_glo, data = sscores))
 
texreg(list(s_p, s_y_p, sWt_p,sWt_y_p, t_p, t_y_p),      
       dcolumn=FALSE,
       custom.model.names=c("Unweighted S Scores","Unweighted S Scores","Weighted S Scores", "Weighted S Scores", "Tau-B", "Tau-B"),
      )
 

############################################################

############################################################ 
load(paste0(pathTnsr, '/tnsrSpace/cosineDist_idealPtun_defEntSum.rda'))
 

# Evaluate the PCA for different triads of countries
# Political Strategic Interest

setwd(pathGraphics)
pdf("dyadic_USIsraelIran_tnsr.pdf")
plotSub(c(2, 630, 666), cosDistU1 , ylab = "Political Strategic Interest", stratName = 'stratInt')
dev.off()
hist(cosDistU1$stratInt)
plotSub
setwd(pathGraphics)
pdf("dyadic_ChinaJapNK_tnsr.pdf")
plotSub(c(731, 710, 740), cosDistU1  , ylab = "Political Strategic Interest", stratName = 'stratInt')
dev.off()

setwd(pathGraphics)
pdf("dyadic_USIndPak_tnsr.pdf")
plotSub(c(2, 750, 770),cosDistU1, ylab = "Political Strategic Interest", stratName = 'stratInt')
dev.off()
midsAgg[which(midsAgg$ccode1 == 770 & midsAgg$ccode2 == 750),]
midsAgg[which(midsAgg$ccode1 == 770 & midsAgg$ccode2 == 2),]



# check raw data
load( paste0(pathTnsr, 'ally.rda') )
load( paste0(pathTnsr, 'un.rda') )
load( paste0(pathTnsr, 'unIdealPt.rda') )
load( paste0(pathTnsr, 'igo.rda') )
load( paste0(pathTnsr, 'mids.rda') )
load( paste0(pathTnsr, 'sipri.rda') )

plotSub(c(2, 750, 770),igo, ylab = "Political Strategic Interest", stratName = 'igoWeighted')
plotSub(c(2, 770, 750),defEntSumAlly, ylab = "Political Strategic Interest", stratName = 'defEntSum')
plotSub(c(2,750, 770),unIdeal, ylab = "Political Strategic Interest", stratName = 'idealpointdistance')
plotSub(c(2,750, 770),un, ylab = "Political Strategic Interest", stratName = 'agree3un')
 
