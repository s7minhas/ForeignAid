
##### Load Packages and Functions #####

library(networkDynamic)
library(network)
library(sna)
library(ggplot2)

source('~/Documents/Papers/ForeignAid/RCode/bipartite.R')
source('~/Documents/Papers/ForeignAid/RCode/ggnet.R')


###### Load Data #########
setwd("~/Dropbox/ForeignAid/Data")
load("aidData.rda")
aidData$Country<-factor(aidData$Country) # clean up unused factors
 
 
###### Clean Data
aid.nd<-aidData[-which(is.na(aidData$aid)), 1:4] #nd for network dynamic
names(aid.nd)<-c("tail", "head", "time", "aid")
aid.nd<-aid.nd[, c(3, 1, 2, 4)]
aid.nd$time<-aid.nd$time - 1970 
levels(aid.nd$tail)<-c(as.character(seq(1, 43)))
levels(aid.nd$head)<-c(as.character(seq(1+43, 157+43))) 
aid.nd$tail<-as.numeric(as.character(aid.nd$tail))
aid.nd$head<-as.numeric(as.character(aid.nd$head))



##############################
######## Make Network ##############
################################ 


######### Dynamic (time) network over time ##########

# grab time, sender, reciever information
aidEdge<-as.matrix(data.frame(aid.nd[,1:3], row.names=NULL))
nt<-length(unique(aidNet$tail))

# create empty network
emptyAid<-network.initialize(200, bipartite=nt, directed = F)

# initialize dynamic data frame
aidNet<-networkDynamic(base.net = emptyAid, edge.toggles = aidEdge)


# plot network
gplot(network(network.extract(aidNet, onset =41, terminus=41)), gmode="twomode", displaylabels=T) # can access different years by adjusting 'onset' and 'terminus'


### Note: I still need to figure out how to make weighted matrix. Have figured out how to add weight attribute (below) but not how to transform a network with weight attributes to a weighted adjacency matrix. Once I figure this out, can make ggplot of network
# add weight attributes
for (i in 1:max(aidData$year)) {
if (i !=max(aidData$year)) {activate.edge.attribute(aidNet, 'weight', aid.nd[aid.nd$time==i,4], onset = i, terminus = i +1)}
else if (i ==max(aidData$year)){
activate.edge.attribute(aidNet, 'weight', aid.nd[aid.nd$time==i,4], onset = i, terminus = i)}}



########## ggplot of yearly network  #######

# grab data for year 2011
t2011<-aid.nd[which(aid.nd$time==41 & aid.nd$aid>0),c(2:4)]

# create adjacency matrix; add names
t2011.adj<-matrix(0, length(levels(aidData$Sender)), length(levels(aidData$Country) ))
for ( i in 1:NROW(t2011)) t2011.adj[t2011[i, 1], t2011[i,2]-43]<-t2011[i,3]
t2011.adj<-data.frame(t2011.adj)
row.names(t2011.adj)<-levels(aidData$Sender)
names(t2011.adj)<-levels(aidData$Country)

# create network object
t2011.net = bipartite.network(t2011.adj, modes = c("donor", "receiver"))

# plot network object in ggplot2
ggnet(t2011.net, segment.size = edge.weights(t2011.adj, .25),segment.alpha = .5,node.group = get.vertex.attribute(t2011.net, "mode"), size = 5, label.nodes = c(levels(aidData$Sender), levels(aidData$Country)), label.size =2) # we can probably adjust the label colors later
 
    
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
####### Extra/training data ####### 
# toy example

n<-6
edges<-rbind(
c(1,4,1),
c(1,5,2),
c(4,1,1),
c(5,1,2),
c(2,5,1),
c(5,2,1),
c(3,5,1),
c(3,6,2),
c(6,3,2)
)
attr(edges,"n")<-n
attr(edges,"vnames")<-c(letters[1:3],LETTERS[4:6])
attr(edges,"bipartite")<-3
edges
gplot(edges,displaylabels=TRUE,gmode="twomode")  #Plot
as.sociomatrix.sna(edges)                        #Convert to matrix

 
# Aid/one year

t2011<-aid.nd[which(aid.nd$time==41),c(2:4)]
empty2011 <-network.initialize(224, bipartite=43, directed = F)
t2011.net<-network.edgelist(t2011, empty2011, ignore.eval=F)
gplot(t2011.net, gmode="twomode", displaylabels=T)


t2011<-aid.nd[which(aid.nd$time==41),c(1:3)]
empty2011 <-network.initialize(224, bipartite=43, directed = F)
t2011.net<-networkDynamic( base.net=empty2011, edge.toggles = t2011 )
gplot(network(t2011.net), gmode="twomode", displaylabels=T) #34, 23, 22, 26

# Aid/multiple years
m.years<-as.matrix(data.frame(aid.nd[which(aid.nd$time==41|aid.nd$time==40),1:3], row.names=NULL))
emptyM <-network.initialize(224, bipartite=43, directed = F)
m.year.net<-networkDynamic(base.net = emptyM, edge.toggles = m.years)

activate.edge.attribute(m.year.net, 'weight', aid.nd[aid.nd$time==41,4], onset =41, terminus =41)
 
gplot(network.extract(m.year.net, onset =40, terminus=41), gmode="twomode")
gplot(network(network.extract(m.year.net, onset =41, terminus=41)), gmode="twomode", displaylabels=T)
 



 

 



 