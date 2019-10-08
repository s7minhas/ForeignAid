################################################################
# Helper functions
trim = function (x) gsub("^\\s+|\\s+$", "", x)

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x)) }

num = function(x){ as.numeric(as.character(x)) }

char = function(x){as.character(x)}

pasteVec = function(x,y){ as.vector(outer(x,y,paste0)) }
################################################################

################################################################
# Log transformations for vars with negative values
logNeg = function(z){
	x = z[!is.na(z)]; y = x
	y[x>0] = log(x[x>0]); y[x<0] = -log(abs(x[x<0])); y[x==0] = 0
	z[!is.na(z)] = y; z
}

# Rescaling variables
rescale = function(x,new_max,new_min){
 xResc = (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }
 ################################################################

################################################################
# Convert to cname
cname = function(x){
	x = as.character(x)
	toupper(countrycode(x, 'country.name', 'country.name')) }
################################################################

################################################################
# 
coefp_colors = c(
  "Positive"=rgb(54, 144, 192, maxColorValue=255), 
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255)
)

#
coefp_colors_grey = coefp_colors
coefp_colors_grey[1:2] = 'grey30'
coefp_colors_grey[3:4] = 'grey50'
################################################################