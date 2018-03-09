if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

# inference on mult eff
csim <- function(x,y){
  c <- x %*% y / (sqrt( x%*%x * y%*%y  ))
  return(c) }

csimMat <- function(x) {
  cos <- matrix(NA, nrow=ncol(x),ncol=ncol(x),dimnames=list(colnames(x),colnames(x)))
  for(i in 1:ncol(x)) {
      for(j in i:ncol(x)) {
      vec1 <- x[which(x[,i] & x[,j]),i]
      vec2 <- x[which(x[,i] & x[,j]),j]  
      cos[i,j]= csim(vec1,vec2)
      cos[j,i]=cos[i,j]
    } }
  return(cos) }

tensorCsim <- function(x, mode, years){

	# find cosine distance for each tensor object
	listCsim = lapply(1:length(x), function(y){
			tensorObj = x[[y]]
			U = tensorObj$U[[mode]]
			
			cosDist = melt(csimMat(t(U)))
			cosDist$year = years[y]
			return(cosDist)
			})

		# turn list into dataframe
		dfCsim = do.call(rbind, listCsim)

		# match back ccode ids
		dfCsim$pos_year_1 <- paste0(dfCsim$Var1, dfCsim$year)
		dfCsim$pos_year_2 <- paste0(dfCsim$Var2, dfCsim$year)

		dfCsim$ccode1<- cowMat$ccode[match(dfCsim$pos_year_1, cowMat$pos_year)]
		dfCsim$ccode2<- cowMat$ccode[match(dfCsim$pos_year_2, cowMat$pos_year)]
	
		dfCsim = dfCsim[which(dfCsim$Var1 != dfCsim$Var2), ]
		return(dfCsim)}


############################
# Load tucker tensor decomposition
# load(paste0(pathTnsr, 'tnsrSpace/tnsr_agree2_DefEntSum_igoWt.rda'))
load(paste0(pathTnsr, 'tnsrSpace/tnsr_idealPtun_defEntSum_igoWt_fatalMids.rda'))

# load raw matrices
load( paste0(pathTnsr,'amenData_all_rescaled.rda') )

# create codebook for cowcodes
years <- 1975:2005
cowMat <- do.call(rbind, lapply(1:length(amData), function(x){
		slice <- amData[[x]]
		ccode <- as.numeric(rownames(slice))
		pos <- 1:length(ccode)
		df <- data.frame(ccode, pos, year = years[[x]])}))
cowMat$pos_year <- paste0(cowMat$pos, cowMat$year)


############################
# calculate cosine distance
cosDistU1 = tensorCsim(res, 1, 1975:2005)
names(cosDistU1)[3] = 'stratInt'

 

# match countryname
cosDistU1$cname_1 <- panel$cname[match(cosDistU1$ccode1, panel$ccode)]
cosDistU1$cname_2 <- panel$cname[match(cosDistU1$ccode2, panel$ccode)]

 

#save(cosDistU1, file = paste0(pathTnsr, '/tnsrSpace/cosineDist_agree2_DefEntSum_igoWt.rda'))
save(cosDistU1, file = paste0(pathTnsr, '/tnsrSpace/cosineDist_idealPtun_defEntSum_igoWt_fatalMids.rda'))

# check
#cosDistU2 = tensorCsim(res, 1) 
#identical(cosDistU1, cosDistU2)





