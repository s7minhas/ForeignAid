source('setup.R')


 
 
sender = 'United States'
receiver = 'Iran'


# ---------- All sector disaggregation ---------
# Create Figure 1
load(paste0(pathData, '/aidDataDisagg.rda'))
aidDataDisaggLong = reshape(aidData[, c('Receiver', 'Sender', 'year', names(aidData)[grep('Total', names(aidData))][1:3])],
							varying = names(aidData)[grep('Total', names(aidData))][1:3],
							v.names = 'aidAmt',
							timevar = 'aidType',
							times = names(aidData)[grep('Total', names(aidData))][1:3],
							direction = 'long' )

subAidDataRaw = aidDataDisaggLong[which(aidDataDisaggLong$Receiver == receiver & aidDataDisaggLong$Sender == sender),]

subAidDataRaw = subAidDataRaw[which(subAidDataRaw$year > 1974),]
subAidDataRaw$aidAmt = subAidDataRaw$aidAmt/1000000

subAidDataRaw$aidLabel = as.factor(subAidDataRaw$aidType) 
levels(subAidDataRaw$aidLabel) = c('Civil Society Aid', 'Development Aid', 'Humanitarian Aid')
subAidDataRaw$aidLabel = factor(subAidDataRaw$aidLabel, levels(subAidDataRaw$aidLabel)[c(3,1,2)])

# viz 
usiran = ggplot(subAidDataRaw, aes(x = year, y = aidAmt, group= aidLabel)) +
	geom_line(aes( linetype = aidLabel), size = 1.5, alpha = .65, position=position_jitter(w=0.00, h=.05))+
	geom_point()+
	guides(linetype=guide_legend(title=""))+
	theme(
		axis.title = element_text(size = 16),
		axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
		axis.text.y = element_text(size = 12),
		legend.position = 'bottom' ,
		legend.text = element_text(size = 12),
		legend.title = element_text(size = 14),
		panel.border = element_blank(),
		axis.ticks = element_blank()
		) +
	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	labs(
		y = "Committed Amount\n(USD Constant Dollars, millions)",
		x = 'Year', shape = "Sector" , color = "Sector")+
	theme(legend.key.width = unit(3,"cm"))
 
ggsave(usiran, file=paste0(pathGraphics, '/US_Iran_aid.pdf'), width=12, height=8)
pathGraphics = '~/Research/10739895rngwzsfjtzbw/graphics'
ggsave(usiran, file=paste0(pathGraphics, '/US_Iran_aid.pdf'), width=12, height=8)