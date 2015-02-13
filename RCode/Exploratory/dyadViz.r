if (Sys.info()['user']=="cindycheng"){
  pathCode="~/Documents/Papers/ForeignAid/RCode";
  pathResults = '~/Dropbox/ForeignAid/Results'
  pathData = '~/Dropbox/ForeignAid/data'}
 
source(paste0(pathCode, "/setup.R"))


##############################################################

##############################################################
## Make vizualzations for PCA 

# load data
load('~/Dropbox/ForeignAid/Results/PCA/PCA_FullData_allyIGOUN.rda')
load('~/Dropbox/ForeignAid/Results/PCA/PCA_FullData_midWarArmsSum.rda')

# extract data
PCA = PCA_FullData[[1]]

# extract country names
CNTRY_NAME1 = panel$CNTRY_NAME[match(unique(PCA$ccode1[which(PCA$year==2005)]), panel$ccode)]
CNTRY_NAME2 = panel$CNTRY_NAME[match(unique(PCA$ccode2[which(PCA$year==2005)]), panel$ccode)]
 

# create plot coordinates
xcoord = as.factor(rep(seq(1, 192, by = 1), rep(192, 192)))
levels(xcoord)= CNTRY_NAME1 

ycoord = as.factor(rep(seq(1, 192, by = 1 ), 192))
levels(ycoord)= CNTRY_NAME2 

# combine plot coordinates with PCA results for year 2005
PCA2005 = PCA[which(PCA$year == 2005),]
PCAplot = data.frame(xcoord, ycoord)
PCAplot = PCAplot[-which(PCAplot$xcoord == PCAplot$ycoord),]
PCAplot$PCA =  PCA$dils.link.coefficients[which(PCA$year==2005)]
PCAplot$PCA = PCAplot$PCA/max(PCAplot$PCA )
 
# create plot
p = ggplot(PCAplot, aes(xcoord, ycoord))


# save plot for military strategic interest
setwd(pathGraphics)
pdf("dyadViz_midWarArmsSum.pdf")
p + geom_point(aes(colour = PCA)) + theme(text = element_text(size=5),axis.text.x = element_text(angle=90, vjust=1))+labs(x = "countries", y = "countries")
dev.off()
dev.off()

# save plot for political strategic interest
setwd(pathGraphics)
pdf("dyadViz_allyIGOUN.pdf")
p + geom_point(aes( colour = PCA))  + theme(text = element_text(size=5),axis.text.x = element_text(angle=90, vjust=1))+labs(x = "countries", y = "countries")
dev.off()

