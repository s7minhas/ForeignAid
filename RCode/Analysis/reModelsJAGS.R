if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Dropbox/Documents/Papers/ForeignAid1/RCode/setup.R') }
pathGraphics = '~/Research/ForeignAid/Presentations/Graphics/'
################################################################
# Load reg data
setwd(pathData)
load('IData.rda')

 
regData = data.frame(IData)
 
# Adjust covariates
regData$LmilMu = regData$LmilMu + abs(regData$LmilMu)

# Only include senders with at least n receivers in every year
regData$tmp=1
agg=summaryBy(tmp ~ ccodeS + year, data=regData, FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=paste0(agg[which(agg$tmp>=10),1], agg[which(agg$tmp>=10),2])
regData = regData[which(regData$cyearS %in% toKeep),]

# Remove senders that only have datapoints for a certain time period
agg=summaryBy(tmp ~ ccodeS,
	data=unique(regData[,c('ccodeS', 'year', 'tmp')]), FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=agg[which(agg$tmp>=7),1]
regData = regData[which(regData$ccodeS %in% toKeep),]

 
hist(exp(regData$commitUSD13))

 

################################################################

################################################################
## setup objects for jags

# data
forJags = list( aid = regData[,'commitUSD13'],
				LstratMu = regData[, 'LstratMu'],
				colony = regData[, 'colony'],
				Lpolity2 = regData[, 'Lpolity2'],
				LlnGdpCap = regData[, 'LlnGdpCap'],
				LlifeExpect = regData[, 'LlifeExpect'],
				Lno_disasters = regData[, 'Lno_disasters'],
				Lcivwar = regData[, 'Lcivwar'],
				N = dim(regData)[1])

# set seeds for each chain
n.chains = 3
initsList =  vector(mode="list", n.chains)

initsList[[1]]<- list(
				.RNG.seed=609,
     			.RNG.name="base::Wichmann-Hill")
initsList[[2]]<- list(
				.RNG.seed=2,
     			.RNG.name="base::Wichmann-Hill")
initsList[[3]]<- list(
				.RNG.seed=2048,
     			.RNG.name="base::Wichmann-Hill")
 

################################################################

################################################################
### Zero - inflated poisson ### 
# zeros trick in JAGS:  https://hypergeometric.wordpress.com/2014/02/17/the-zero-crossings-trick-for-jags-finding-roots-stochastically/
 
library(runjags)
setwd(paste0(pathCode, '/Analysis'))
zip <- run.jags(model="zip.bug",
				monitor = c("beta","eta", "p"),
				data=forJags,
                n.chains = n.chains,
                inits = initsList,
                burnin = 5000,
                sample = 10000,
                adapt = 500,
                thin = 10,
                method = 'rjparallel',
                keep.jags.files = TRUE)

 
b <- extractFromCodaNChains(zip,"beta")
g <- extractFromCodaNChains(zip,"gamma")

t(apply(b, 2, extractCI))
t(apply(g, 2, extractCI))
traplot(out_keep)


################################################################

################################################################
### Hurdle Poisson ### 

setwd(paste0(pathCode, '/Analysis'))
hurdle <- run.jags(model="hurdle.bug",
				monitor = c("beta","eta", "p"),
				data=forJags,
                n.chains = n.chains,
                inits = initsList,
                burnin = 5000,
                sample = 10000,
                adapt = 500,
                thin = 10,
                method = 'rjparallel',
                keep.jags.files = TRUE)






