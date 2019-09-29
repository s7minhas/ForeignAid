source('setupAppendix.R')
loadPkg('tidyr')
################################################################

################################################################
# load underlying data for plot
# for more details on assembly of this dataset, see
# https://github.com/s7minhas/ForeignAid/blob/master/RCode/Analysis/robustChecks/bjps_reviewers.R
load('intake/rawData.rda')
################################################################

################################################################
regData$Lno_disastersDum = ifelse(regData$Lno_disasters == 0, 0, 1)

plotData = regData[, c('cnameS', 'cnameR', 'year', 'Lno_disasters', 'Lno_disastersDum', 'LstratMu', 'education', 'health', 'waterSanitation', 'otherSocialInfrastructureAndServices', 
                       'economicInfrastructureAndServices', 'industryMiningConstruction', 
                       'environmentalProtection', 'otherDevelopmentAid', 'debtRelief')]



plotData = gather(plotData, aidType, aidValue, education:debtRelief)
plotData$Lno_disastersDum = as.factor(plotData$Lno_disastersDum) 
plotData$LstratMuQuintile = ifelse(plotData$LstratMu < quantile(plotData$LstratMu, .1, na.rm = TRUE), 10,
                                   ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .1, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .2, na.rm = TRUE), 20,  
                                          ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .2, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .3, na.rm = TRUE), 30,
                                                 ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .3, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .4, na.rm = TRUE), 40,
                                                        ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .4, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .5, na.rm = TRUE), 50,
                                                               ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .5, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .6, na.rm = TRUE), 60,
                                                                      ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .6, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .7, na.rm = TRUE), 70,
                                                                             ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .7, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .8, na.rm = TRUE), 80,
                                                                                    ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .8, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .9, na.rm = TRUE), 90, 100)))))))))
plotData = plotData[-which(is.na(plotData$LstratMu)),]


# restrict sample so that there are about the same number of observations of countries that have and have not experienced natural disasters
prop.table(table(plotData$Lno_disasters))
# .24+.13+.06
plotData = plotData[which(plotData$Lno_disasters<4),]
plotData$Lno_disasters[which(plotData$Lno_disasters %in% c(1,2,3))] = 1

plotDataAgg = plotData %>% group_by(aidType, Lno_disasters, LstratMuQuintile) %>% summarise(aidValue = sum(aidValue, na.rm = TRUE)) %>% data.frame()



disast_labels = c('0 Disasters', 
                  '1 - 3 Disasters' )
names(disast_labels) = 0:1
v <- ggplot(plotDataAgg, aes(x = LstratMuQuintile, y = aidValue, fill = aidType)) + 
  geom_area()+
  labs(x = 'Strategic Distance', y = 'Value of Aid Committments')+
  theme(legend.position="bottom")+ 
  guides(fill=guide_legend(nrow=3,byrow=TRUE, title.position = 'top'))+
  scale_fill_discrete(name="Type of Development Aid Committment",
                      labels=c("Debt Relief", 
                               "Economic Infrastructure and Services", 
                               "Education",
                               "Environmental Protection",
                               "Health",
                               "Industry, Mining and Construct.",
                               "Other Development Aid",
                               "Other Social Infrastructure and Services",
                               "Water Sanitation"))


v=v+facet_grid(.~Lno_disasters, labeller = labeller(Lno_disasters = disast_labels ))
ggsave(v, 
  file = 'floats/figureA11.pdf',
  width=6, height=4
)
################################################################