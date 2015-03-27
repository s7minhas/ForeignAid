if(Sys.info()["user"]=="janus829"){
	source("~/Research/ForeignAid/RCode/setup.R")
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
yr='2000'
mat=prepMat(aidMats[[yr]])
rec=1:15
# rec=sample(1:ncol(mat), 1:7, replace=FALSE)
sen=1:5
# sen=sample(1:nrow(mat), 5, replace=FALSE)
library(RColorBrewer)
cols=brewer.pal(length(sen), 'Paired')
# webAid=function(yr, topRec=20, topSen=10){
	recCntries=names(sort(apply(mat, 2, sum), decreasing=TRUE))[rec]
	senCntries=names(sort(apply(mat, 1, sum), decreasing=TRUE))[sen]	
	mat=mat[senCntries,recCntries]
	plotweb(t(mat), col.interaction=cols)	
# }

# webAid('1975')

# Plot for a single country over time
cntryNet=function(sen, rec, yrs, colPal='Blues'){
	allCntries=unique(unlist(lapply(aidMats, function(x) colnames(prepMat(x)))))
	matYr=matrix(0, nrow=length(allCntries), ncol=length(yrs), 
		dimnames=list(allCntries, yrs))

	for(ii in seq_along(yrs)){
		mat=prepMat(aidMats[[ yrs[ii] ]])
		vec = mat[sen,]
		recCntries=names(sort(vec, decreasing=TRUE))[rec]
		matYr[recCntries,ii] = vec[recCntries]
	}

	notZero=names(which(rowSums(matYr)!=0))
	matYr = matYr[notZero,]
	rownames(matYr)[rownames(matYr)=='USSR'] = 'Russia'
	cols=brewer.pal(length(yrs), colPal)
	plotweb(matYr, 
		# sequence=list( colnames(matYr), NULL ),
		method='normal',
		col.interaction = cols, 
		col.high = cols,
		col.low = 'darkgrey',
		bor.col.high = "black",
		bor.col.low = "black"
		)
}

cntryNet(sen="United States", rec=1:5, yrs=char(seq(1975,2005,5)) )
# cntryNet(sen="France", rec=1:5, yrs=char(seq(1975,2005,5)) )
################################################################