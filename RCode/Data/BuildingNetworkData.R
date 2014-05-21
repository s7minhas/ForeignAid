rm(list=ls())
##### Load Packages and Functions #####
source('~/Documents/Papers/ForeignAid/RCode/setup.R')
source('~/Documents/Papers/ForeignAid/RCode/bipartite.R')
source('~/Documents/Papers/ForeignAid/RCode/ggnet.R')

library(networkDynamic)
library(network)
library(sna)
library(ggplot2)




###### Load Data #########
setwd("~/Dropbox/ForeignAid/Data")
load("aidData.rda")

class(aidData$Sender)
 
###### Clean Data
aidData$Country<-as.factor(aidData$Country)

nd <- length(unique(aidData$Sender)) # number of unique donors ; note that the number of donors after you get rid of all the NAs (as in aid.nd below) shrinks to 32
nr <- length(unique(aidData$Country)) # number of unique recipients ; number does not shrink in aid.nd

aidData<-aidData[aidData$aid>0,]
aid.nd<-aidData[which(!is.na(aidData$aid)), 1:4] #nd for network dynamic
names(aid.nd)<-c("tail", "head", "time", "aid")
aid.nd<-aid.nd[, c(3, 1, 2, 4)]
aid.nd$time<-aid.nd$time - 1970 
levels(aid.nd$tail)<-c(as.character(seq(1, nd )))
levels(aid.nd$head)<-c(as.character(seq(1+nd, nr+nd))) 
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

########### all data across time
for(k in 1:nd){
#require(network)
#source('~/Documents/Papers/ForeignAid/RCode/ggnet.R')
data<-aid.nd[which(aid.nd$time==k),c(2:4)]
 
# create adjacency matrix; add names
data.adj<-matrix(0, length(levels(aidData$Sender)), length(levels(aidData$Country) ))
for ( i in 1:NROW(data)) {
data.adj[data[i, 1], c(data[i,2]-nd)]<-data[i,3]
}
data.adj<-data.frame(data.adj)
row.names(data.adj)<-levels(aidData$Sender)
names(data.adj)<-levels(aidData$Country)


# create network object
data.net= bipartite.network(data.adj, modes = c("donor", "receiver"))


# plot network object in ggplot2
mypath <- file.path('~/Dropbox/ForeignAid/Graphics/', paste(paste("network_ggplot", k+1970, sep=""), sep=".", "pdf"))
plots<-ggnet(data.net, segment.size = edge.weights(data.adj, .6),segment.alpha = .5,node.group = get.vertex.attribute(data.net, "mode"), size = 5, label.nodes = c(levels(aidData$Sender), levels(aidData$Country)), label.size =1.5, segment.color = rgb(94, 87, 87, .5, maxColorValue=255) , arrow.size=.25) # we can probably adjust the label colors later)
ggsave(filename = mypath, plots)
}   
 
####### all data, without nodes with no edges    
    
for(k in 1:nd){
data<-aid.nd[which(aid.nd$time==k),c(2:4)]

# create adjacency matrix; add names
data.adj<-data.frame(matrix(0, length(unique(data$tail)), length(unique(data$head) )))
row.names(data.adj)<-levels(aidData$Sender)[unique(data$tail)]
names(data.adj)<-levels(aidData$Country)[c(unique(data$head)-nd)]


data$tail<-as.factor(data$tail)
data$head<-as.factor(data$head)
levels(data$tail)<-levels(aidData$Sender)[as.numeric(levels(data$tail))]
levels(data$head)<-levels(aidData$Country)[c(as.numeric(levels(data$head))-42)]
data$tail<-as.character(data$tail)
data$head<-as.character(data$head)

for ( i in 1:NROW(data)){ data.adj[which(row.names(data.adj) == data[i, 1]), which(names(data.adj)==data[i, 2])]<-data[i,3]}
 
 
# create network object
data.net = bipartite.network(data.adj, modes = c("donor", "receiver"))
 
     
mypath <- file.path('~/Dropbox/ForeignAid/Graphics/', paste(paste("network_ggplot_nodeswithedges", k+1970, sep=""), sep=".", "pdf"))
plots<-ggnet(data.net, segment.size = edge.weights(data.adj, .6),segment.alpha = .5,node.group = get.vertex.attribute(data.net, "mode"), size = 8.5, label.nodes = c(levels(aidData$Sender), levels(aidData$Country)), label.size =2, segment.color = rgb(94, 87, 87, .5, maxColorValue=255) , arrow.size=.25) # we can probably adjust the label colors later)
ggsave(plots, filename = mypath)
 }       
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
empty2011 <-network.initialize(nd+nr, bipartite=nd, directed = F)
t2011.net<-network.edgelist(t2011, empty2011, ignore.eval=F)
gplot(t2011.net, gmode="twomode", displaylabels=T)


t2011<-aid.nd[which(aid.nd$time==41),c(1:3)]
empty2011 <-network.initialize(nd+nr, bipartite=nd, directed = F)
t2011.net<-networkDynamic( base.net=empty2011, edge.toggles = t2011 )
gplot(network(t2011.net), gmode="twomode", displaylabels=T) #34, 23, 22, 26
 

# Aid/multiple years
m.years<-as.matrix(data.frame(aid.nd[which(aid.nd$time==39|aid.nd$time==41|aid.nd$time==40),1:3], row.names=NULL))
emptyM <-network.initialize(nd+nr, bipartite=nd, directed = T)
m.year.net<-networkDynamic(base.net = emptyM, edge.toggles = m.years)


plot(network.extract(m.year.net, onset =41, terminus=41))
list.edge.attributes.active(m.year.net, at = 41)
activate.edge.attribute(m.year.net, 'weight', aidData[aidData$year==2011,4], onset =41, terminus =41
get.edge.attribute.active(m.year.net, 'weight', onset = 41, terminus = 41)

gplot(network(network.extract(m.year.net, onset =41, terminus=41, rule = 'any') , ignore.values = F), gmode="twomode")
 
#set.network.attribute(m.year.net,  'vertex.names', id) 
#activate.vertex.attribute(m.year.net, 'vertex.names', value = c(levels(aidData$Sender), levels(aidData$Country)), onset = 41, terminus=41)

 
# 2011 graph 
id = c(levels(aidData$Sender), levels(aidData$Country))
t2011<-network.extract(m.year.net, onset =41, terminus=41, rule = 'any')
t2011.net<-network(network.extract(m.year.net, onset =41, terminus=41), ignore.eval=F, names.eval ="weight")
plot(t2011)

 

pdf("t2011_ND.pdf")
gplot(t2011.net, gmode="twomode", displaylabels=T, label.cex=.5) 
dev.off()


 

 # grab data for year 2011
t2011<-aid.nd[which(aid.nd$time==1),c(2:4)]
 
 
# create adjacency matrix; add names
t2011.adj<-matrix(0, length(levels(aidData$Sender)), length(levels(aidData$Country) ))
for ( i in 1:NROW(t2011)) t2011.adj[t2011[i, 1], c(t2011[i,2]-nd)]<-t2011[i,3]
t2011.adj<-data.frame(t2011.adj)
row.names(t2011.adj)<-levels(aidData$Sender)
names(t2011.adj)<-levels(aidData$Country)

# create network object
t2011.net = bipartite.network(t2011.adj, modes = c("donor", "receiver"))

# plot network object in ggplot2
setwd('~/Dropbox/ForeignAid/Graphics')
pdf("network_ggplot")
ggnet(t2011.net, segment.size = edge.weights(t2011.adj, .6),segment.alpha = .5,node.group = get.vertex.attribute(t2011.net, "mode"), size = 5, label.nodes = c(levels(aidData$Sender), levels(aidData$Country)), label.size =1.5, segment.color = rgb(94, 87, 87, .5, maxColorValue=255) , arrow.size=.25) # we can probably adjust the label colors later
dev.off() 
 


## delete nodes without edges

t2011<-aid.nd[which(aid.nd$time==24),c(2:4)]



# create adjacency matrix; add names
t2011.adj<-data.frame(matrix(0, length(unique(t2011$tail)), length(unique(t2011$head) )))
row.names(t2011.adj)<-levels(aidData$Sender)[unique(t2011$tail)]
names(t2011.adj)<-levels(aidData$Country)[c(unique(t2011$head)-nd)]

t2011$tail<-as.factor(t2011$tail)
t2011$head<-as.factor(t2011$head)
levels(t2011$tail)<-levels(aidData$Sender)[as.numeric(levels(t2011$tail))]
levels(t2011$head)<-levels(aidData$Country)[c(as.numeric(levels(t2011$head))-42)]
t2011$tail<-as.character(t2011$tail)
t2011$head<-as.character(t2011$head)


 
for ( i in 1:NROW(t2011)){ t2011.adj[which(row.names(t2011.adj) == t2011[i, 1]), which(names(t2011.adj)==t2011[i, 2])]<-t2011[i,3]}
 
 
# create network object
t2011.net = bipartite.network(t2011.adj, modes = c("donor", "receiver"))


 



 