if(Sys.info()["user"]=="janus829"){
	source("~/Desktop/Research/ForeignAid/RCode/setup.R")
}

################################################################
setwd(pathData)
load('aidData.rda')
################################################################

################################################################
# Removes nodes with no edges and add column names from panel
prepMat=function(x){
	active=unique( c( which(rowSums(x)!=0),which(colSums(x)!=0) ) )
	x=x[active, active]
	colnames(x)=panel$CNTRY_NAME[match(colnames(x), panel$ccode)]
	rownames(x)=panel$CNTRY_NAME[match(rownames(x), panel$ccode)]
	return(x)
}
################################################################

################################################################
igraphAid=function(yr){
	mat=prepMat(aidMats[[yr]])
	aidNet=graph.adjacency(mat, mode='directed', weighted=TRUE)

	plot.igraph(aidNet,
	    main='', 
		# layout.kamada.kawai layout.fruchterman.reingold, layout.circle	    
	    layout=layout.kamada.kawai,
	    vertex.size=3,
	    vertex.label.dist=0.5, vertex.label.cex=.5,
	    vertex.color='gray73', vertex.label.color='black',
	    edge.width=E(aidNet)$weight^(1/4), 
	    # edge.color= E(Tbit)$color,
	    edge.curved=F
	)
}

aidMats=lapply(aidMats, function(x){ log(x+1) })
igraphAid('1973')
################################################################

################################################################
# Trying out bipartite package
webAid=function(yr){
	mat=prepMat(aidMats[[yr]])
	plotweb(mat)	
}

webAid('1975')
webAid('2005')
################################################################