source('setupAppendix.R')
loadPkg('xtable')
################################################################

################################################################
# Load reg data
load('../main/intake/iDataDisagg_wLags_v3.rda')

# create dv lags
iData = lapply(iData, function(df){
  # gen some ids
  df$id = with(df, paste0(ccodeS, 9999, ccodeR) )
  df$id = num(df$id)
  df$idYr = with(df, paste0(id, year))
  df$idYr = num(df$idYr)
  
  # lag
  df = lagData(df, 
               'idYr','id', 
               c(
                 'humanitarianTotal', 
                 'developTotal',
                 'civSocietyTotal'
               )
  )
  
  return(df) })
################################################################

################################################################
# organize labels for table
cntrlVars=c(
  'Lno_disasters', 'colony', 'Lpolity2',
  'LlnGdpCap', 'LlifeExpect', 'Lcivwar' )
cntrlVarNames = c(
  'No. Disasters$_{r,t-1}$',    
  'Former Colony$_{sr,t-1}$',
  'Polity$_{r,t-1}$',
  'Log(GDP per capita)$_{r,t-1}$',
  'Life Expectancy$_{r,t-1}$',
  'Civil War$_{r,t-1}$'
)

varsInt=c(
  'LstratMu', cntrlVars[1], 'LstratMu:Lno_disasters', cntrlVars[-1])
varNamesInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames[1],
                'Strategic Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$', 
                cntrlVarNames[-1])

varDef = cbind(varsInt, varNamesInt)
################################################################

################################################################
# Load reg data
dvs = c(
  'humanitarianTotal',
  'developTotal',
  'civSocietyTotal'
)
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
baseSpec = paste(
  c(
    'LstratMu', 'Lno_disasters', 
    'LstratMu * Lno_disasters', 'colony', 
    'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
    'Lcivwar'
  ), collapse=' + ' )
feStruc = '+ factor(ccodeS) - 1'

# set up formulas
feModSpecs = lapply(dvs, function(y){
  formula(paste0(y, '~', paste0('L',y), '+', baseSpec, feStruc)) })
################################################################

################################################################
# run models
if(!file.exists('mods/fe_mods_table.rda')){
  cl=makeCluster(5) ; registerDoParallel(cl)
  
  # run fixed effect models
  ## humanitarian model
  humModFE= foreach(df=iData) %dopar% {
    lm(feModSpecs[[1]], data=df) }
  
  ## civ society model
  civModFE = foreach(df=iData) %dopar% {
    lm(feModSpecs[[2]], data=df) }
  
  ## dev model
  devModFE = foreach(df=iData) %dopar% {
    lm(feModSpecs[[3]], data=df) }
  
  #
  stopCluster(cl)
  
  # org for coef plot in re_fe_compare.R
  feMods = lapply(
    list(humModFE,devModFE,civModFE),
    function(impMods){
      coefSumm=lapply(impMods, function(mod){
        summ = summary(mod)$'coefficients'  
        return(summ)
      })
      return(rubinCoef(coefSumm, 'fe'))
    } )
  save(feMods, file='mods/fe_mods_table.rda')  
} else { load('mods/fe_mods_table.rda') }
################################################################

################################################################
# coef summaries
feMods = lapply(feMods, function(mod){
  mod$pval = 2*pnorm(-abs(mod$t))
  return(mod) } )
################################################################

################################################################
# make table for fe results
varDefTab = varDef
names(feMods) = dvNames
shhh=lapply(seq_along(feMods), function(ii){
  x= feMods[[ii]]
  rownames(x) = x$var
  x = x[,-which(names(x) %in% c('t','var'))]
  x = x[c(1:3,nrow(x),4:(nrow(x)-1)),]
  x = data.matrix(x)
  x = round(x, 2)
  colnames(x) = c('Estimate', 'Std. Error', 'P-value')
  x = x[varDef[,1],]
  rownames(x) = varDef[,2]
  out = print.xtable(
    xtable( x,
            caption=paste0(
              'Fixed effect regression results for ', 
              names(feMods)[ii],'.' ),
            align=c('lccc')
    ),
    sanitize.rownames.function=identity,
    file=paste0('floats/tableA',ii+1,'.tex')
  )
  return(out)
} )
################################################################