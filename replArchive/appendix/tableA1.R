source('setupAppendix.R')
loadPkg('texreg')
#############################################################

############################################################
# see https://github.com/s7minhas/ForeignAid/blob/master/RCode/Exploratory/PCA_validation.R
# for details on the construction of this .rda
# referenced code just involves merging variables together
load('intake/validate.rda')
############################################################

############################################################
# Evaluate PCA vis a vis S-scores and Tau - B
# s-scores
sWt_p = lm(s_wt_glo ~ PCAStd  , data = validate)
sWt_y_p = lm(s_wt_glo ~ PCAStd + as.factor(year) , data = validate)

s_p = lm(s_un_glo ~ PCAStd  , data = validate)
s_y_p = lm(s_un_glo ~ PCAStd + as.factor(year) , data = validate)

# tau - B
t_p = lm(tau_glob ~ PCAStd + as.factor(year), data = validate)
t_y_p =lm(tau_glob ~ PCAStd, data = validate)

# Compare against relationship between S-Scores and Tau-B
texreg(list(s_p, s_y_p, sWt_p,sWt_y_p, t_p, t_y_p),      
       dcolumn=FALSE,
       custom.model.names=c(
         "Unweighted S Scores","Unweighted S Scores","Weighted S Scores",
         "Weighted S Scores", "Tau-B", "Tau-B"),
       file='floats/tableA1.tex'
)
############################################################