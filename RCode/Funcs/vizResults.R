#######################################################################
# GG coefficient plot
ggcoefplot = function(coefData, vars, varNames, estimates, serrors,
	Noylabel=FALSE, coordFlip=TRUE, 
	specY=FALSE, ggylims=NULL, ggybreaks=NULL, revVar=TRUE,
	facet=FALSE, facetName=NULL, facetDim=NULL, facetBreaks=NULL, facetLabs=NULL,
	facetColor=FALSE, colorGrey=FALSE, grSTA=0.8, grEND=0.2, xAngle=45){
  
  # Calculate confidence intervals
  relevRows = which(rownames(coefData) %in% vars)
  estimates = coefData[relevRows, 'Estimate']
  serrors = coefData[relevRows, 'Std. Error']
  facetVar = coefData[relevRows, facetName]
  if(facet){ 
  	VARS = data.frame(cbind(x=varNames, y=vars))
  	temp = rownames(coefData[relevRows,])
  	varNames = as.character(VARS$x[match(temp, VARS$y)])
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
  	coefp = coefp + theme(axis.text.x=element_text(angle=xAngle, hjust=1)) }
  coefp = coefp + theme(legend.position='none', legend.title=element_blank(),
    axis.ticks=element_blank(), panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())
  coefp 
  	} else {
	  coefp = ggplot(ggcoefData, aes(as.factor(Variable), Mean, color = sig))
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
	  	coefp = coefp + theme(axis.text.x=element_text(angle=xAngle, hjust=1)) }
	  coefp = coefp + theme(
      legend.position='none', legend.title=element_blank(),
	    axis.ticks=element_blank(), panel.border = element_blank()
	    # ,axis.line = element_line(color = 'black')
      )
	  coefp 
  	}
}
#######################################################################

#######################################################################
# gg simulation plot
ggsimplot = function(
  modelResults, sims=10000, simData, vars, vi, ostat=median, 
  actual=TRUE, brk=0.1, vRange=NULL,
  sigma=FALSE, intercept=TRUE, ylabel, xlabel,
  specX=FALSE, ggxlims=NULL, ggxbreaks=NULL,
  specY=FALSE, ggylims=NULL, ggybreaks=NULL, plotType='errorBar'){

  # Prep data
  simData=na.omit(simData[,vars])

  # Pull out model results
  vcov=vcov(modelResults)
  betas=modelResults@beta; names(betas)=rownames(vcov)
  RSS=sum(resid(modelResults)^2)
  dfResid=nrow(simData)-length(betas) - length(modelResults@u) + 1
  if(sigma){sigma = sqrt(RSS/dfResid)} else {sigma = 0}

  # Set up scenario
  if(is.null(vRange)){
    if(!actual){vRange=seq(min(simData[,vi]), max(simData[,vi]), brk)
      } else { vRange=sort(unique(simData[,vi])) } } 
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
  
  if(plotType=='errorBar' | plotType=='ribbon'){
    ggData = t(apply(modelExp, 2, 
       function(x){ mean = mean(x) ;
         qlo95 = quantile(x, 0.025) ; qhi95 = quantile(x, 0.975) ;
         qlo90 = quantile(x, 0.05) ; qhi90 = quantile(x, 0.95) ;
         rbind(mean, qlo95, qhi95, qlo90, qhi90) } ))
    ggData = data.frame(ggData)
    colnames(ggData) = c('Fit', 'Lo95', 'Hi95', 'Lo90', 'Hi90')
    
    if(plotType=='errorBar'){
        ggData$x = as.factor(vRange)
        temp = ggplot(ggData)
        temp = temp + geom_errorbar(aes(x=x, ymax=Hi90, ymin=Lo90), width=0.2) 
        temp = temp + geom_errorbar(aes(x=x, ymax=Hi95, ymin=Lo95), width=.5) 
        temp = temp + geom_point(aes(x=x, y=Fit))
        if(actual){temp = temp + geom_rug(sides='b', position='jitter')}
        if(specX){temp = temp + scale_x_discrete(limits=ggxlims, breaks=ggxbreaks)}
        }
    
    if(plotType=='ribbon'){
      ggData$x=vRange
      temp <- ggplot(ggData, aes(x=x, y=Fit, ymin=Lo95, ymax=Hi95))
      temp <- temp + geom_line() + geom_ribbon(alpha=0.3)
      temp <- temp + geom_ribbon(aes(ymin=Lo90, ymax=Hi90), alpha=0.5)
      if(actual){temp = temp + geom_rug(sides='b', position='jitter')}
      if(specX){temp = temp + scale_x_continuous(limits=ggxlims, breaks=ggxbreaks)}
    }
  }

  if(plotType=='distribution'){
    colnames(modelExp)=1:ncol(modelExp)
    modelExp2=melt(modelExp)[,-1]
    ggMeans = ddply(modelExp2, .(X2), summarise, sMean=mean(value))
    ggDensity = ddply(modelExp2, .(X2), .fun=function(x){
      tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
      q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
      q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
      data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

    ggMeans$X2 = as.factor(ggMeans$X2)
    ggDensity$X2 = as.factor(ggDensity$X2)

    temp = ggplot()
    temp = temp + geom_line(data=ggDensity, aes(x=x,y=y,color=X2))
    temp = temp + geom_vline(data=ggMeans,
      aes(xintercept=sMean, color=X2),linetype='solid',size=1)
    temp = temp + geom_ribbon(data=subset(ggDensity,q95),
      aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.5)
    temp = temp + geom_ribbon(data=subset(ggDensity,q90),
      aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.9)
    temp = temp + theme(legend.position='none')
    if(specX){temp = temp + scale_x_continuous(limits=ggxlims, breaks=ggxbreaks)}
  }
  
  if(specY){temp = temp + scale_y_continuous(limits=ggylims, breaks=ggybreaks)}
  temp = temp + xlab(xlabel) + ylab(ylabel)
  temp = temp + theme(
    panel.border = element_blank(), 
    # axis.line = element_line(), 
    axis.ticks = element_blank(),
    axis.title.x = element_text(vjust=-0.2),
    axis.title.y = element_text(vjust=0.2))
  temp
}
#######################################################################

#######################################################################
# Par mfrow equiv for ggplot
multiplot <- function(plots, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  # plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
#######################################################################