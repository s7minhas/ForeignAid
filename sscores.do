
clear
cd "/Users/cindycheng/Documents/Papers/ForeignAid/version4.1_comma"

insheet using "alliance.csv"

 
keep if year==1995|year ==1996


 

 
scompute ccode1 ccode2, id(v year) svar(ententeweight nnweight defenseweight) combine
 

weight(defense_weight nn_weight entente_weight)
