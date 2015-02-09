#######################################################################
# GG coefficient plot
ggcoefplot = function(coefData, vars, varNames, estimates, serrors,
	Noylabel=FALSE, coordFlip=TRUE, 
	specY=FALSE, ggylims=NULL, ggybreaks=NULL, revVar=TRUE,
	facet=FALSE, facetName=NULL, facetDim=NULL, facetBreaks=NULL, facetLabs=NULL,
	facetColor=FALSE, colorGrey=FALSE, grSTA=0.8, grEND=0.2, allBlack=FALSE){
  
  # Calculate confidence intervals
  relevRows = which(rownames(coefData) %in% vars)
  estimates = coefData[relevRows, 'Estimate']
  serrors = coefData[relevRows, 'Std. Error']
  facetVar = coefData[relevRows, facetName]
  if(facet){ 
  	VARS = data.frame(cbind(x=varNames, y=vars))
  	temp = rownames(coefData[relevRows,])
  	varNames = char(VARS$x[match(temp, VARS$y)])
  }
  ninetyfive_upper_CI = estimates + qnorm(.975)*serrors
  ninetyfive_lower_CI = estimates - qnorm(.975)*serrors
  ninety_upper_CI = estimates + qnorm(.95)*serrors
  ninety_lower_CI = estimates - qnorm(.95)*serrors
  if(facet){
    coefData = cbind(varNames, estimates, serrors, ninetyfive_upper_CI,
                      ninetyfive_lower_CI, ninety_upper_CI, ninety_lower_CI, facetVar)
    } else {
	    coefData = cbind(varNames, estimates, serrors, ninetyfive_upper_CI,
	                      ninetyfive_lower_CI, ninety_upper_CI, ninety_lower_CI)    	
    }
  
  # Reorganize data as numeric
  ggcoefData = data.frame(coefData, row.names=NULL)
  if(facet){
    names(ggcoefData) = c("Variable", "Mean", "SEs", "upper95", "lower95",
                           "upper90","lower90", 'Facet')
    } else {
	    names(ggcoefData) = c("Variable", "Mean", "SEs", "upper95", "lower95",
	                           "upper90","lower90")    	
    }
  ggcoefData$Mean = num(ggcoefData$Mean)
  ggcoefData$lower90 = num(ggcoefData$lower90)
  ggcoefData$upper90 = num(ggcoefData$upper90)
  ggcoefData$lower95 = num(ggcoefData$lower95)
  ggcoefData$upper95 = num(ggcoefData$upper95)
  if(facet){ggcoefData$Facet = num(ggcoefData$Facet)}
  if(revVar){ggcoefData$Variable = factor(ggcoefData$Variable, levels=rev(unique(varNames)))
  	} else {ggcoefData$Variable = factor(ggcoefData$Variable, levels=unique(varNames))}
  
  # Add in variable for colors
  ggcoefData$sig = NULL
  ggcoefData$sig[ggcoefData$lower90 > 0 & ggcoefData$lower95 < 0] = "Positive at 90"
  ggcoefData$sig[ggcoefData$lower95 > 0] = "Positive"
  ggcoefData$sig[ggcoefData$upper90 < 0 & ggcoefData$upper95 > 0] = "Negative at 90"
  ggcoefData$sig[ggcoefData$upper95 < 0] = "Negative"
  ggcoefData$sig[ggcoefData$lower90 < 0 & ggcoefData$upper90 > 0] = "Insig"
  coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
                    "Negative"= rgb(222, 45, 38, maxColorValue=255),
                    "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
                    "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
                    "Insig" = rgb(150, 150, 150, maxColorValue=255))
  
  if(facet){
  if(!facetColor){coefp = ggplot(ggcoefData, aes(as.factor(Facet), Mean, color = sig))}
  if(facetColor){coefp = ggplot(ggcoefData, aes(as.factor(Facet), Mean, color = as.factor(Facet)))}
  if(allBlack){coefp = ggplot(ggcoefData, aes(as.factor(Facet), Mean),color = 'black')}  
  coefp = coefp + geom_linerange(aes(ymin=lower95, ymax=upper95), alpha = .3, size = 0.3)
  coefp = coefp + geom_linerange(aes(ymin=lower90, ymax=upper90),alpha = 1, size = 1)
  coefp = coefp + geom_hline(aes(yintercept=0), linetype=2, color = "black")
  coefp = coefp + geom_point(aes(as.factor(Facet),Mean), size=4, shape=20)
  coefp = coefp + geom_errorbar(aes(ymin=lower95,ymax=upper95),linetype = 1,width = 0.1)
  if(specY){coefp = coefp + scale_y_continuous(limits=ggylims, breaks=ggybreaks)}
  if(!colorGrey){
    if(!facetColor){coefp = coefp + scale_colour_manual(values = coefp_colors)}
    if(facetColor){coefp = coefp + scale_colour_brewer()}
    } else { coefp = coefp + scale_colour_grey(start=grSTA,end=grEND) }
  coefp = coefp + xlab("") + ylab("") 
  if(!facetColor){coefp = coefp + facet_wrap(~Variable, scales="free_y", nrow=facetDim[1],ncol=facetDim[2])}
  if(facetColor){coefp = coefp + facet_wrap(~Variable, nrow=facetDim[1],ncol=facetDim[2])}  	
  coefp = coefp + scale_x_discrete(breaks=facetBreaks, labels=facetLabs)
  if(Noylabel){coefp = coefp + theme(axis.text.y = element_blank())}
  if(coordFlip){coefp = coefp + coord_flip()} else {
  	coefp = coefp + theme(axis.text.x=element_text(angle=45, hjust=1)) }
  coefp = coefp + theme(legend.position='none', legend.title=element_blank(),
    axis.ticks=element_blank(), panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())
  coefp 
  	} else {
	  coefp = ggplot(ggcoefData, aes(as.factor(Variable), Mean, color = sig))
	  if(allBlack){coefp = ggplot(ggcoefData, aes(as.factor(Variable), Mean), color = 'black')}  	  
	  coefp = coefp + geom_linerange(aes(ymin=lower95, ymax=upper95), alpha = .3, size = 0.3)
	  coefp = coefp + geom_linerange(aes(ymin=lower90, ymax=upper90),alpha = 1, size = 1)
	  coefp = coefp + geom_hline(aes(yintercept=0), linetype=2, color = "black")
	  coefp = coefp + geom_point(aes(as.factor(Variable),Mean), size=4, shape=20)
	  coefp = coefp + geom_errorbar(aes(ymin=lower95,ymax=upper95),linetype = 1,width = 0.1)
	  if(specY){coefp = coefp + scale_y_continuous(limits=ggylims, breaks=ggybreaks)}
	  if(!colorGrey){
	  	coefp = coefp + scale_colour_manual(values = coefp_colors)
	  	} else { coefp = coefp + scale_colour_grey(start=grSTA,end=grEND) }
	  coefp = coefp + xlab("") + ylab("") 
	  if(Noylabel){coefp = coefp + theme(axis.text.y = element_blank())}
	  if(coordFlip){coefp = coefp + coord_flip()} else {
	  	coefp = coefp + theme(axis.text.x=element_text(angle=45, hjust=1)) }
	  coefp = coefp + theme(legend.position='none', legend.title=element_blank(),
	    axis.ticks=element_blank(), panel.grid.major=element_blank(),
	    panel.grid.minor=element_blank(), panel.border = element_blank(),
	    axis.line = element_line(color = 'black'))
	  coefp 
  	}
}
#######################################################################

#######################################################################
# gg simulation plot
ggsimplot = function(sims=10000, simData, vars, vi, vRange, ostat=median, 
  betas, vcov, sigma, intercept=TRUE, ylabel, xlabel,
  specX=FALSE, ggxlims=NULL, ggxbreaks=NULL,
  specY=FALSE, ggylims=NULL, ggybreaks=NULL){
  # Set up scenario
  scenCol = length(vars); scenRow = length(vRange)
  scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
  colnames(scenario) = c(vars)
  scenario[,vi] = vRange
  
  viPos = which(vi==vars)
  ovals = apply(simData[,vars[-viPos]], 2, ostat)
  scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
  if(intercept){scenario = cbind('(Intercept)'=1, scenario)}
  vars2 = colnames(scenario)
  
  draws = mvrnorm(n = sims, betas[vars2], vcov[vars2,vars2])
  modelPreds = draws %*% t(scenario)
  modelExp = apply(modelPreds, 2, function(x) FUN=rnorm(sims, x, sigma))
  
  ggData = t(apply(modelExp, 2, 
     function(x){ mean = mean(x) ;
       qlo95 = quantile(x, 0.025) ; qhi95 = quantile(x, 0.975) ;
       qlo90 = quantile(x, 0.05) ; qhi90 = quantile(x, 0.95) ;
       rbind(mean, qlo95, qhi95, qlo90, qhi90) } ))
  ggData = data.frame(ggData)
  colnames(ggData) = c('Fit', 'Lo95', 'Hi95', 'Lo90', 'Hi90')
  ggData$x = as.factor(vRange)
  
  temp = ggplot(ggData)
  temp = temp + geom_errorbar(aes(x=x, ymax=Hi90, ymin=Lo90), width=0.2) 
  temp = temp + geom_errorbar(aes(x=x, ymax=Hi95, ymin=Lo95), width=.5) 
  temp = temp + geom_point(aes(x=x, y=Fit))
  temp = temp + theme(panel.border = element_blank(), 
    axis.line = element_line(), axis.ticks = element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
    axis.title.x = element_text(vjust=-0.2),
    axis.title.y = element_text(vjust=0.2))
  temp = temp + xlab(xlabel) + ylab(ylabel)
  temp = temp + theme(panel.border = element_blank(), 
    axis.line = element_line(), axis.ticks = element_blank(),
    panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
    axis.title.x = element_text(vjust=-0.2),
    axis.title.y = element_text(vjust=0.2))
  if(specX){temp = temp + scale_x_discrete(limits=ggxlims, breaks=ggxbreaks)}
  if(specY){temp = temp + scale_y_continuous(limits=ggylims, breaks=ggybreaks)}
  temp}
#######################################################################

# Table summaries for lm objects
lmtableSM = function(coefs, vnames, modelResults, modelSumm, modelNames, digs=3){
  noModels=length(modelSumm)
  tableResults = matrix('', nrow=2*length(coefs), ncol=1+noModels)
  tableResults[,1] = rep(coefs,2)
  colnames(tableResults) = c('Variable',paste('Model',1:noModels))
  for(ii in 2:ncol(tableResults)){
    temp = modelSumm[[ii-1]]
    df = summary(modelResults[[ii-1]])[['df']][2]
    temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
    estims = temp[1:length(coefs),'Estimate']
    estims = round(as.numeric(char(estims)),digs)
    tvals = abs(temp[1:length(coefs),'t value'])
    tvals = round(as.numeric(char(tvals)),digs)
    estims = ifelse(tvals>=qt(0.95,df) & !is.na(tvals) & tvals<qt(0.975,df), 
      paste('$', estims,'^{\\ast}$',sep=''), estims)
    estims = ifelse(tvals>=qt(0.975,df) & !is.na(tvals), 
      paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
    tableResults[1:length(coefs),ii] = estims
    serrors = temp[(length(coefs)+1):nrow(tableResults),'Std. Error']
    serrors = round(as.numeric(char(serrors)),digs)
    serrors = paste('(',serrors,')',sep='')
    serrors = ifelse(serrors=='(NA)','',serrors)
    tableResults[(length(coefs)+1):nrow(tableResults),ii] = serrors
  }

  # Reorganizing rows and variable labels
  tableFinal = NULL
  for(ii in 1:length(coefs)){
    temp = cbind('', t(tableResults[ii+length(coefs),2:ncol(tableResults)]))
    tableFinal = rbind(tableFinal, tableResults[ii,], temp) }
  tableFinal[,'Variable'] = vnames[match(tableFinal[,'Variable'],coefs)]
  tableFinal[,'Variable'][is.na(tableFinal[,'Variable'])] = ''

  # Adding other info
  sSize = cbind('n', t(as.vector(mapply(x=modelResults,
      function(x) FUN=summary(x)[['df']][2] + summary(x)[['df']][1] ))))
  gSize = cbind('N', t(as.vector(mapply(x=modelResults, function(x)
    FUN=summary(x)[['df']][1]-length(attr(summary(x)[['terms']],'term.labels'))+1 ))))
  rSQ = cbind('$R^{2}$', t(as.vector(mapply(x=modelResults,
      function(x) FUN=round(summary(x)[['r.squared']],2) ))))
  arSQ = cbind('Adj. $R^{2}$', t(as.vector(mapply(x=modelResults,
      function(x) FUN=round(summary(x)[['adj.r.squared']],2) ))))
  rmse = round(mapply(x=modelResults, function(x) 
    FUN=sqrt(mean(summary(x)[['residuals']]^2))),2)
  frmse = cbind('RMSE', t(rmse))

  tableFinal = rbind(tableFinal, sSize, gSize, rSQ, arSQ, frmse)
  colnames(tableFinal)[2:(noModels+1)] = modelNames
  tableFinal
}