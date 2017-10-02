if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/noImputationData.rda'))
load(paste0(pathData, '/iData_v2.rda'))
regData = iData[[5]]
regData$id = with(regData, paste(ccodeS, ccodeR, sep='_'))
regData$year = num(regData$year)
regData$ccodeS = num(regData$ccodeS)
################################################################

################################################################
## mod formula
vars=c(
	'LstratMu', # state interest measure
	'Lno_disasters', 'colony', 'Lpolity2',
	'LlnGdpCap', 'LlifeExpect', 'Lcivwar'
	)

## Run model on full sample
### Results consistent across various specifications
modForm=formula(paste0(
	'commitUSD13 ~ ', paste(vars, collapse=' + '), 
	'+ (1|id) + (1|year)'))	# Sender + year random effects

# Cross val by country
yrs = sort(unique(regData$year))
# coefData=NULL
# for(ii in seq_along(yrs)){
# 	slice = regData[regData$year!=yrs[ii],]
# 	slice$year = factor(slice$year)
# 	slice$id = factor(slice$id)		
# 	slice$commitUSD13 = log(slice$commitUSD13 + 1)
# 	mod = lmer(modForm, data=slice) # random effects estimation	
# 	coefs = summary(mod)$coefficients[1:(length(vars)+1),]
# 	coefs = cbind(Year=yrs[ii], coefs)
# 	coefData = rbind(coefData, coefs)
# 	print(char(yrs[ii]))
# }

# Coefficient plot
varNames = c(
	'Pol. Strat. Distance$_{sr,t-1}$',
	'No. Disasters$_{r,t-1}$',    
	'Former Colony$_{sr,t-1}$',
	'Polity$_{r,t-1}$',
	'Log(GDP per capita)$_{r,t-1}$',
	'Life Expectancy$_{r,t-1}$',
	'Civil War$_{r,t-1}$'
	)

# timeCross=ggcoefplot(
# 	coefData=coefData, 
# 	vars=c('LstratMu','Lno_disasters'), 
# 	varNames=varNames[c(1,2)],	
# 	Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
# 	facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
# 	facetName='Year', 
# 	facetBreaks=seq(yrs[1],yrs[length(yrs)],5),
# 	facetLabs=seq(yrs[1],yrs[length(yrs)],5)
#   ) + theme_bw() + theme(
#   axis.ticks=element_blank(),
#   panel.border=element_blank(),
#   legend.position='none',
#   strip.text.x = element_text(size = 9, color='white' ),
#   strip.background = element_rect(fill = "#525252", color='#525252') 
#   )
# tikz(file=paste0(pathGraphics, '/timeCross.tex'), width=8, height=4, standAlone=F)
# timeCross
# dev.off()

# Cross val by sender cntry
cntries = unique(regData$ccodeS)
coefData=NULL
for(ii in seq_along(cntries)){
	slice = regData[regData$ccodeS != cntries[ii],]
	slice$year = factor(slice$year)
	slice$id = factor(slice$id)	
	slice$commitUSD13 = log(slice$commitUSD13 + 1)
	mod=lmer(modForm, data=slice) # random effects estimation	
	coefs = summary(mod)$coefficients[1:(length(vars)+1),]
	coefs = cbind(LeftOut=cntries[ii], coefs)
	coefData = rbind(coefData, coefs)
	print(cntries[ii])
}	

# Coefficient plot
panel$CNTRY_NAME[panel$CNTRY_NAME=='Germany Federal Republic'] = 'Germany'
cntryCross=ggcoefplot(coefData=coefData, 
	vars=c('LstratMu','Lno_disasters'), 
	varNames=varNames[c(1,2)],	
	Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
	facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
	facetName='LeftOut',
	facetBreaks=unique(coefData[,'LeftOut']),
	facetLabs=panel$CNTRY_NAME[match(unique(coefData[,'LeftOut']), panel$ccode)]
  ) + theme_bw() + theme(
  panel.border = element_blank(),
  axis.ticks=element_blank(),
  legend.position = 'none',
  strip.text.x = element_text(size = 9, color='white' ),
  axis.text.x = element_text(angle=45, size=6, hjust=1),
  strip.background = element_rect(fill = "#525252", color='#525252') 
  )
tikz(file=paste0(pathGraphics, '/cntryCross.tex'), width=8, height=4, standAlone=F)
cntryCross
dev.off()
################################################################