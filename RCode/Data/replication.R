# paper says the analysis goes from 1970 to 1995 but the data only goes up to 1990
# the paper says 'other' religion includes hindu, aethism and animist religions; can't find a column for animist religion, used 'spirit80', 'tribal80', 'non_li80', 'new_li80', couldn't get the right results


if(Sys.info()["user"]=="cindycheng"){
	source("~/Documents/Papers/ForeignAid/RCode/setup.R") }

setwd(paste0(pathData, '/Replication'))


alesina0 = read.csv("Alesina_Dollar_2000.csv", skip = 2, stringsAsFactors = F, na.strings =  c("#N/A", "#NUM!"))
religion = read.csv("religion.csv", skip = 2, stringsAsFactors = F, na.strings = "#N/A")

religionraw = read.csv("religionraw.csv", skip = 1, stringsAsFactors = F, na.strings = c("#N/A", "."))
# religionraw$other = rowSums(cbind(religionraw$Atheist80, religionraw$Hindu80, religionraw$Spirit80), na.rm = F)
# religionraw$other = rowSums(cbind(religionraw$Atheist80, religionraw$Hindu80, religionraw$Tribal80), na.rm = F)
# religionraw$other = rowSums(cbind(religionraw$Atheist80, religionraw$Hindu80, religionraw$Non_li80), na.rm = F)
religionraw$other = rowSums(cbind(religionraw$Atheist80, religionraw$Hindu80, religionraw$New_li80), na.rm = F)

religion$other = religionraw$other

# hindu animist, aethist
 
alesina = merge(alesina0, religion, by = "X", all = T)
 

alesina1 = alesina[which(alesina$X.2 > 1969),]


 
alesina1$LNRGDPPCSQ = alesina1$LNRGDPPC^2
alesina1$GASPOLRev = c(alesina1$GASPOL - 8)*-1

model1 = lm(LNOECD1 ~ LNRGDPPC + LNRGDPPCSQ + LNPOP + LNPOPSQ + OPEN + GASPOLRev + FRUSA + FRJPN + LNCOLS + DUMEGY + DUMISR + Musl + Romcath + othcath +DUM70 + DUM75 + DUM80  + DUM90, data = alesina1)
summary(model1)

unique(alesina1$X)
alesina[which(alesina$X == "IRQ"),]



sort(names(alesina))




class(alesina1$RGDPPPC)
alesinaMA = do.call(rbind,lapply(split(alesina1, alesina1$X), function(x, n = 5){
	 xTS = ts(x, start = c(1970), frequency = 1/5)
	 xMA = do.call(cbind, apply(xTS, 2, function(x){ forecast::ma(x, n = 5) }))
	 return(xMA)
	}))

 
alesinaMA  = data.frame(alesinaMA )
names(alesinaMA) = names(alesina)
class(alesinaMA) 

 
head(alesina)
unique(alesina$X.2)