rm(list = ls())
source('~/Documents/Papers/ForeignAid/RCode/Data/BuildNetworkData.R', chdir = TRUE)
 
############ Aid Data ###############
load(paste0(pathData, '/aidData.rda'))
 
# clean data
aid.nd = cleanBipDyad(aidData, sender = 'cnameS', receiver = 'cnameR', time = 'year', value = 'commitUSD09')

# put data in adjacency matrics, one per year
aidMat = adjMatTS(aid.nd)

# put one of the years into a bipartite network
aidNet1973 = bipartite.network(aidMat[[11]], mode = c("reciever", "donor"), directed = F)

# plot network
pdf("~/Downloads/aid1973")
ggnet(aidNet1973, segment.size = edge.weights(aidMat[[11]], .6),segment.alpha = .5,node.group = get.vertex.attribute(aidNet1973, "mode"), size = 8.5, label.nodes = c(row.names(aidMat[[11]]), names(aidMat[[11]])), label.size =2, segment.color = rgb(94, 87, 87, .5, maxColorValue=255) , arrow.size=.25) # we can probably adjust the label colors later)
dev.off()
?network

############ Strategic Data ###############

load(paste0(pathResults, '/gbmeLatDist/unDist.rda'))
unDist = data.frame(res)
unDist$unDistInv = 1/unDist$unDist
unDistMat = DyadBuildNames(variable = 'unDistInv', dyadData = unDist, cntry1 = 'ccode1', cntry2 = 'ccode2', time = 'year', pd = 1970:2010, panel = panel, directed = F )

unDistMat1970 = unDistMat[['1970']]
unDistMat1970  = unDistMat1970 [-which(rowSums(unDistMat1970 )==0), -which(colSums(unDistMat1970 )==0)]
unDistNet1970 = makeNetwork(unDistMat1970, directed = F)
ggnet(unDistNet1970, label.nodes = c(network.vertex.names(unDistNet1970)), label.size =2)


plotNetwork <- function(MatTS, year, complete = F, directed = F){
	MatYear = MatTS[[year]]
	if (complete == F){
	MatYear = MatYear[-which(rowSums(MatYear)==0), -which(colSums(MatYear)==0) ]
	}
	MatYearNet = makeNetwork(MatYear, directed)
	
	return(MatYearNet)
	
}
ggnet(unDistNet2010, lable.nodes = c(network.vertex.names(unDistNet2010)))
unDistNet2010 = plotNetwork(unDistMat, year ='2010')
network.vertex.names(unDistNet2010)

# # add country names 
# unDist1 = merge(panel[, c('cname', 'year', 'ccode')], unDist, by.x = c('ccode', 'year'), by.y = c('ccode1', 'year'), all.y = T)
# unDist1  = rename(unDist1, ccode1 = ccode, cname1 = cname)
# unDist2 = merge(panel[, c('cname', 'year', 'ccode')], unDist1, by.x = c('ccode', 'year'), by.y = c('ccode2', 'year'), all.y = T)
# unDist2  = rename(unDist2, ccode2 = ccode, cname2 = cname)
 


load(paste0(pathResults, '/gbmeLatDist/allyDist.rda'))
allyDist = data.frame(res)
length(sort(unique(allyDist$ccode1)))
length(sort(unique(allianceFINAL$ccode_1)))
sort(row.names(data.frame(allyMats[[1]])))


load(paste0(pathResults, '/gbmeLatDist/igoDist.rda'))
igoDist = data.frame(res)
sort(unique(igoDist$ccode2))
sort(unique(igoDataFINAL$ccode_2))

