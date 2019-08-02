if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))

# create dv lags
iData = lapply(iData, function(df){

  # add total aid variable
  df$totAid = log(df$commitment_amount_usd_constant_sum -
    exp(df$humanitarianTotal) -
    exp(df$civSocietyTotal) +3)

  # gen some ids
  df$id = with(df, paste0(ccodeS, 9999, ccodeR) )
  df$id = num(df$id)
  df$idYr = with(df, paste0(id, year))
  df$idYr = num(df$idYr)

  # lag
  df = lagData(df, 
    'idYr','id', 
    c(
      'commitment_amount_usd_constant_sum',      
      'humanitarianTotal', 
      'civSocietyTotal',
      'totAid'
      )
    )

  return(df) })

# mod specs
dv = c( 'totAid' )
baseSpec = paste(
	c( 
    'LtotAid',
    'LstratMu', 'Lno_disasters', 
		'LstratMu * Lno_disasters', 'colony', 
		'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
		'Lcivwar' ), collapse=' + ' )
reStruc = '(1|id) + (1|year)'
feStruc = 'factor(id) + factor(year) - 1'

# set up formulas
reModSpec = formula(paste0(dv, '~', baseSpec, '+', reStruc))
################################################################

################################################################
# run random effect models
## total aid
cl=makeCluster(5) ; registerDoParallel(cl)
totAidModRE = foreach(df=iData, .packages=c('lme4')) %dopar% {
	lmer(reModSpec, data=df) }
stopCluster(cl)	
totAidSummRE = rubinCoef(totAidModRE)
################################################################

#########################################################
# sub effects
regData = iData[[1]]
noDisast = 4
i = 1

mod = totAidModRE[[1]]
modTitle = 'Total Aid'
var = 'LstratMu'

# Create scenario matrix
stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
disRange=with(data=regData, seq(
  min(Lno_disasters), noDisast, 2) )  
scen = with(data=regData, 
  expand.grid(
    1, 
    median(LtotAid, na.rm=TRUE),
    stratRange, disRange, 
    median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
    median(LlnGdpCap,na.rm=TRUE), 
    median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
    ) )

# Add interaction term
scen = cbind( scen, scen[,3]*scen[,4] )
colnames(scen) = names(fixef(mod))
scen = data.matrix(scen)
pred = scen %*% mod@beta
draws = mvrnorm(10000, mod@beta, vcov(mod))
sysUncert = scen %*% t(draws)
sysInts95 = t(apply(sysUncert, 1, function(x){
  quantile(x, c(0.025, 0.975), na.rm=TRUE) }))
sysInts90 = t(apply(sysUncert, 1, function(x){
  quantile(x, c(0.05, 0.95), na.rm=TRUE) }))

# Combine for plotting
ggData=data.frame(
    cbind(pred, sysInts95, sysInts90, 
      scen[,var], scen[,'Lno_disasters'])
    )
names(ggData)=c('fit', 'sysLo95', 'sysHi95', 
  'sysLo90', 'sysHi90', var, 'Lno_disasters')

# Plot rel at various cuts of disasters
disRange=with(data=regData, seq(
  min(Lno_disasters), noDisast, 2) )    
ggDataSmall = ggData[which(ggData$Lno_disasters %in% disRange),]
actData = regData[,c('LstratMu', 'Lno_disasters')]
actData = actData[actData$Lno_disasters %in% seq(0,noDisast,2),]
actData = actData[
  actData$LstratMu>=stratQts[1] & actData$LstratMu<=stratQts[2],]

# change facet labels
ggDataSmall$Lno_disasters = paste(
  ggDataSmall$Lno_disasters, 'Disasters$_{r,t-1}$')
actData$Lno_disasters = paste(
  actData$Lno_disasters, 'Disasters$_{r,t-1}$')

# viz
facet_labeller = function(string){ TeX(string) }
tmp=ggplot(ggDataSmall, aes(x=LstratMu, y=fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=sysLo90, ymax=sysHi90), alpha=.6) +
  geom_ribbon(aes(ymin=sysLo95, ymax=sysHi95), alpha=.4) +
  geom_rug(
    data=actData, 
    aes(x=LstratMu,y=min(ggDataSmall$fit)), sides='b', alpha=.1) +
  facet_grid(
    ~Lno_disasters, 
    labeller=as_labeller(facet_labeller, default = label_parsed)) +
  labs(
    x=TeX('Strategic Distance$_{sr,t-1}$'),
    y=TeX("Log(Aid)$_{t}$"),
    title=modTitle
    ) +
  theme(
    axis.ticks=element_blank(), 
    panel.border = element_blank() )
ggsave(tmp, file=paste0(
  pathGraphics, '/simTotAidPlot_lagDV.pdf'), width = 7, height = 2.5)
#########################################################

################################################################
# vars
cntrlVars=c(
  'LtotAid',
  'Lno_disasters', 'colony', 'Lpolity2',
  'LlnGdpCap', 'LlifeExpect', 'Lcivwar' )
cntrlVarNames = c(
  'Total Aid$_{sr,t-1}$',
  'No. Disasters$_{r,t-1}$',    
  'Former Colony$_{sr,t-1}$',
  'Polity$_{r,t-1}$',
  'Log(GDP per capita)$_{r,t-1}$',
  'Life Expectancy$_{r,t-1}$',
  'Civil War$_{r,t-1}$'
  )
varsInt=c(
  'LstratMu', cntrlVars[2], 'LstratMu:Lno_disasters', cntrlVars[-2])
varNamesInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames[2],
  'Strategic Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$', 
  cntrlVarNames[-2])

# 
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255))

summarizeMods = function(mods, dirtyVars, cleanVars){
  modSumm = lapply(1:length(mods), function(i){
    summ = mods[[i]]
    summ$dv = names(mods)[i]
    summ$up95 = with(summ, beta + qnorm(.975)*se)
    summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se)
    summ$lo90 = with(summ, beta - qnorm(.95)*se)
    summ = summ[summ$var!='(Intercept)',]    
    summ$varClean = cleanVars[match(summ$var, dirtyVars)]
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"
    return(summ) }) %>% do.call('rbind', .)
  modSumm$varClean = factor(modSumm$varClean, levels=rev(cleanVars))
  return(modSumm) }

#
intModSumm = summarizeMods(list(totAidSummRE), varsInt, varNamesInt)
################################################################

################################################################
# Model results
plotRes = function(modSumm){
  # fix some labels
  xlabels = TeX(char(modSumm$varClean))
  xlabels[char(modSumm$varClean)==varNamesInt[3]] = expression( atop(
      'Strategic Distance' ['sr,t-1'],
      'x No. Disasters' ['r,t-1'] ) )

  # viz
  ggplot(modSumm, aes(x=varClean, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors) +
    scale_x_discrete('',labels=xlabels) +    
    coord_flip() +
    # facet_wrap(~dvClean, ncol=4, scales='free_x') +
    labs( y='' ) +
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' ) }

intGG = plotRes(intModSumm)
ggsave(intGG, 
  file=paste0(pathGraphics, '/totAidv2_Coef.pdf'), width=5, height=4)
################################################################