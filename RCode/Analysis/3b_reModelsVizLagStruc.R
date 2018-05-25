if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/iDataDisagg.rda'))

# load model
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal', 'notHumanitarianTotal')
dvNames = paste(c('Humanitarian', 'Development', 'Civil Society', 'Non-Humanitarian'), 'Aid')
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255))

################################################################
for(i in 1:length(dvs)){
  stratMuMods = lapply(c('',paste0(2:6, '_')), function(x){
    pth = paste0(pathResults, '/', dvs[i], '_fullSamp_gaussian_re_LstratMu_',x,'.rda')
    load(pth) ; return(mods) })
  names(stratMuMods) = paste0('Lag ', 1:6)

  modSumm = do.call('rbind', lapply(1:length(stratMuMods), function(i){
    x = stratMuMods[[i]]
    x = rubinCoef(x)
    summ = x[c(2,8),,drop=FALSE]
    summ$lag = names(stratMuMods)[i]
    summ$up95 = with(summ, beta + qnorm(.975)*se) ; summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se); summ$lo90 = with(summ, beta - qnorm(.95)*se)  
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"  
    summ$varFacet = c('Strategic Distance', 'No. Disasters')
    return(summ) }))

  tmp=ggplot(modSumm, aes(x=lag, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors) +
    ggtitle(dvNames[i]) +
    facet_wrap(~varFacet, nrow=2, scales='free_x') +
    ylab('') + xlab('') + 
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' )
  ggsave(tmp, file=paste0(pathGraphics, '/', dvs[i], '_noInt_lagEffect.pdf'), width=8, height=5)
}
################################################################

################################################################
for(i in 1:length(dvs)){
  stratMuMods = lapply(c('',paste0(2:6, '_')), function(x){
    pth = paste0(pathResults, '/', dvs[i], '_fullSamp_gaussian_re_LstratMu_', x, 'interaction.rda')
    load(pth) ; return(mods) })
  names(stratMuMods) = paste0('Lag ', 1:6)

  modSumm = do.call('rbind', lapply(1:length(stratMuMods), function(i){
    x = stratMuMods[[i]]
    x = rubinCoef(x)
    summ = x[c(2,3,9),,drop=FALSE]
    summ$lag = names(stratMuMods)[i]
    summ$up95 = with(summ, beta + qnorm(.975)*se) ; summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se); summ$lo90 = with(summ, beta - qnorm(.95)*se)  
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"  
    summ$varFacet = c('Strategic Distance', 'No. Disasters', 'Strategic Distance x No. Disasters')
    return(summ) }))

  tmp=ggplot(modSumm, aes(x=lag, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors) +
    ggtitle(dvNames[i]) +
    facet_wrap(~varFacet, nrow=3, scales='free_x') +
    ylab('') + xlab('') + 
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' )
  ggsave(tmp, file=paste0(pathGraphics, '/', dvs[i], '_int_lagEffect.pdf'), width=8, height=7)
}
################################################################