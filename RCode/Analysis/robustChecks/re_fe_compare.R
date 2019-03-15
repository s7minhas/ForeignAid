if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, "/iDataDisagg_v3.rda"))
################################################################

################################################################
# RE model
humMod = lmer(
	humanitarianTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		(1|id) + (1|year), 
	data=iData[[1]] 
	)

civMod = lmer(
	civSocietyTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		(1|id) + (1|year), 
	data=iData[[1]] 
	)

devMod = lmer(
	developTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		(1|id) + (1|year), 
	data=iData[[1]] 
	)

humModFE = lm(
	humanitarianTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		factor(id) + factor(year) - 1, 
	data=iData[[1]]
	)

civModFE = lm(
	civSocietyTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		factor(id) + factor(year) - 1, 
	data=iData[[1]]
	)

devModFE = lm(
	developTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		factor(id) + factor(year) - 1, 
	data=iData[[1]]
	)

round(
	summary(humMod)$'coefficients'[
	c(2:3,nrow(summary(humMod)$'coefficients')),],
	3)
round(
	summary(humModFE)$'coefficients'[
	c(1:2,nrow(summary(humModFE)$'coefficients')),],
	3)

round(
	summary(civMod)$'coefficients'[
	c(2:3,nrow(summary(civMod)$'coefficients')),],
	3)
round(
	summary(civModFE)$'coefficients'[
	c(1:2,nrow(summary(civModFE)$'coefficients')),],
	3)

round(
	summary(devMod)$'coefficients'[
	c(2:3,nrow(summary(devMod)$'coefficients')),],
	3)
round(
	summary(devModFE)$'coefficients'[
	c(1:2,nrow(summary(devModFE)$'coefficients')),],
	3)

