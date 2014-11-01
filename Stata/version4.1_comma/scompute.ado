*! scompute
*! Omar M.G. Keshk and Kevin Sweeney
*! Version 1.0 (March 2004)





program define scompute, rclass 
version 8
set more off
syntax varlist(min= 2 max = 2), id(string) svar(varlist) [weightvar(namelist) newfile(string) dropmiss(string) uwb combine]
marksample touse
*markout `touse'


local wegcnt: word count `weightvar'
local svarcnt: word count `svar'

if ("`svar'"~="" & "`weightvar'"~=""){
	if (`wegcnt'>`svarcnt'){
	dis _n(5)
	display in r _col(10) "ERROR: Number of Weight(s) specified (" `wegcnt' ")  is greater than number of svars (" `svarcnt' ") specified."
	exit
	}
	else if (`wegcnt' < `svarcnt') {
	dis _n(5)
	display in r _col(10) "ERROR: Number of Weight(s) specified (" `wegcnt' ")  is less than number of svars (" `svarcnt' ") specified."
	exit
	}
}

if("`combine'"~="" & `svarcnt' == 1){
	dis _n(5)
	display in red _col(10) "ERROR: Option combine can only be used if more than one svar is being calculated."
	exit
	}

if("`weightvar'"~="" & "`uwb'" ~=""){
	dis _n(5)
	display in red _col(10) "ERROR: Options weightvar and uwb cannot be specified together."
	exit
	}
if("`dropmiss'"=="yes" & "`uwb'"~=""){
	dis _n(5)
	dis in r _col(10) "ERROR: UWB option is redundant with the dropmiss(yes) option"
	dis in r _col(13) "since all observations with missing values are not estimated"
	exit
	}

if `wegcnt' == 0{
	 SC1  `0' 
	}
if `wegcnt'==1 & `svarcnt' == 1{
	 SC1  `0'
	}
if `wegcnt'>1 & `svarcnt'> 1{
	SC2 `0' 
	}

return local weightvar `"`weightvar'"'
return local svar      `"`svar'"'
return local id         = `"`id'"'

	if "`newfile'"~=""{
	return local filename = `"`newfile'"'
	}
	else{
	return local filename = "S_`svar'_computed.dta"
	}
	
	if "`combine'"~=""{
	return local combine = "Option combine specified, svars combined into a single svar"
	}


end


program define SC1, rclass
syntax varlist(min= 2 max = 2), id(string) svar(varlist) [weightvar(namelist) newfile(string) combine dropmiss(string) uwb ]
marksample touse
set more off
pause on
dis _n
dis _n

 

capture erase S_`svar'_computed.dta


qui save tempfile, replace  



local Mfile = "$S_FN"


tempfile `newfile'

qui gen tempy = _n 	

sort `id'   

tempvar idvar
qui by `id': gen `idvar' = 1 if _n == 1 /* & `touse' */
qui replace `idvar' = sum(`idvar')   
 

qui sum `idvar'	 			
local idvarcnt = r(max)	  


qui sort tempy	

qui drop tempy

tokenize `varlist'

local ccode1 = "`1'"



local ccode2 = "`2'"

		if("`weightvar'"~= ""){
			qui keep `ccode1' `ccode2' `id' `svar' `weightvar' `idvar' `touse'
			}
			else{
			qui keep `ccode1' `ccode2' `id' `svar' `idvar' `touse'
		}


	
	if ("`weightvar'"~=""){
		tempvar wvar
		qui gen double `wvar' = `weightvar'  
		}
	if ("`weightvar'"==""){
		local tempwvar = "yes"    
		}



qui save "`Mfile'", replace

local svarcntr = 1


local svarloop: word 1 of `svar'

while("`svarloop'"~=""){

	local idvari = 1			
		while (`idvari' <= `idvarcnt'){


		qui keep if `idvar' == `idvari'	 
        	
		qui sum `svarloop'  

		qui gen  max = r(max)
		qui gen  min = r(min)
		
		qui gen dmax = max-min 

		
		if ("`tempwvar'" ~= ""){
			tempvar wvar
			qui gen `wvar' = .
			}

		
		qui keep `ccode1' `ccode2' `id' `svarloop' `idvar' `wvar' dmax  `touse' 




		qui reshape wide `svarloop', i(`ccode2') j(`ccode1')


		 
		qui unab orgnames: _all

	
		qui order `ccode2' `wvar' `id' dmax 

		
		qui unab newordnames: _all 
		
		tokenize `newordnames'  		       	 
		
		local i = 1
				while (`i' <= 4 ){ 		 

				gettoken `svarloop' newordnames: newordnames /*gettoken `svarloop' names: names  */      
				macro shift  			
			
				local i = `i' + 1
				}
		
		tokenize `newordnames'
		local newordnamess = " "
		while ("`1'"~=""){
			if index("`1'", "__")==0{
			local newordnamess "`newordnamess' `1'"
			macro shift
			}
			else{
			macro shift
			}
		}
		local newordnames "`newordnamess'"
		


		
		qui local namescnt: word count `newordnames'  


	local names1loop: word 1 of `newordnames'    		

						    				
	local loopcntr1 = 1			

			if ("`tempwvar'" ~= ""){
			qui unab throwaway: `wvar'  
			
			qui capture drop `wvar'    
			}
			
			tempvar rwvar
			qui capture gen `rwvar' = `wvar'


		while("`names1loop'") ~= "" { 
		
			local temp = `loopcntr1' + 1 


	

			local names2loop: word `temp'  of `newordnames' 


			
			local loopcntr2 = `temp' 		 


			 
			qui gen double tempvar = `names1loop'	





				while("`names2loop'") ~= ""{ 


				
					markout `touse' `names1loop' `names2loop'  


					
							
					if ("`tempwvar'"~=""){  /* default uwb */


						if ("`dropmiss'"=="" & "`uwb'"==""){


							tempvar wvar
							qui sum `touse' if `touse'~=0

							qui gen `wvar'= 1/r(N)  if `touse'~=0   
							local missing = ""
							}
							else if ("`dropmiss'"=="" & "`uwb'"~=""){
							tempvar wvar
							qui sum `touse' 

							qui gen `wvar'= 1/r(N)   


							local missing = ""
							}
					}

					if ("`dropmiss'" ==  "dyadic" & "`uwb'"==""){
							tempvar wvar
							qui sum `touse' if `touse'~=0
							qui gen `wvar'= 1/r(N) if `touse'~=0  
							tempvar dyadtouse1 dyadtouse2
							qui gen byte `dyadtouse1' = 1
							qui gen byte `dyadtouse2' = 1
							markout `dyadtouse1' `names1loop'
							markout `dyadtouse2' `names2loop'
							local namelength = length("`svarloop'")
							local totallength1 = length("`names1loop'")
							local totallength2 = length("`names2loop'")
							local endtoken1 = substr("`names1loop'",(`namelength'+1),(`totallength1'-`namelength'))
							local endtoken2 = substr("`names2loop'",(`namelength'+1),(`totallength2'-`namelength'))	
							qui capture drop `dy1' `dy2'
							tempvar dy1 dy2
							qui gen `dy1' = `ccode2' if `dyadtouse1'==0 & `ccode2'==`endtoken2'
							qui gen `dy2' = `ccode2' if `dyadtouse2'==0 & `ccode2'==`endtoken1'
							qui sum `dy1' 
								if (r(N)!=0){
								qui sum `dy1'
								local ldy1 = r(max)
								qui sum `dy2'
								local ldy2 =r(max)
									if (`ldy1' < `ldy2'){
									local dyadmiss = "if `ccode2'==`ldy1'"
									}
									else{
									local dyadmiss = "if `ccode2'==`ldy2'"
									}
								}
						}
		
					if ("`dropmiss'" ==  "dyadic" & "`uwb'"~=""){
							tempvar wvar
							qui sum `touse' 
							qui gen `wvar'= 1/r(N)   
							tempvar dyadtouse1 dyadtouse2
							qui gen byte `dyadtouse1' = 1
							qui gen byte `dyadtouse2' = 1
							markout `dyadtouse1' `names1loop'
							markout `dyadtouse2' `names2loop'
							local namelength = length("`svarloop'")
							local totallength1 = length("`names1loop'")
							local totallength2 = length("`names2loop'")
							local endtoken1 = substr("`names1loop'",(`namelength'+1),(`totallength1'-`namelength'))
							local endtoken2 = substr("`names2loop'",(`namelength'+1),(`totallength2'-`namelength'))	
							qui capture drop `dy1' `dy2'
							tempvar dy1 dy2
							qui gen `dy1' = `ccode2' if `dyadtouse1'==0 & `ccode2'==`endtoken2'
							qui gen `dy2' = `ccode2' if `dyadtouse2'==0 & `ccode2'==`endtoken1'
							qui sum `dy1' 
								if (r(N)!=0){
								qui sum `dy1'
								local ldy1 = r(max)
								qui sum `dy2'
								local ldy2 =r(max)
									if (`ldy1' < `ldy2'){
									local dyadmiss = "if `ccode2'==`ldy1'"
									}
									else{
									local dyadmiss = "if `ccode2'==`ldy2'"
									}
								}
						}				
						
					if ("`dropmiss'" == "yes"){
					tempvar wvar
					qui sum `touse' if `touse'~=0
					qui gen `wvar'= 1/r(N) if `touse'~=0  
					tempvar remove
					qui capture drop `remove'
					qui gen byte `remove'= 1
					markout `remove' `names1loop' `names2loop'
					qui tab `remove'
					if (r(r)~=1){
						qui replace `wvar'=.
						}
					if (r(r)==1 & "`weightvar'"~=""){
						qui replace `wvar' = `rwvar' 
						qui replace `wvar' = . if `touse'==0					
						}
					}
					
					if  ("`weightvar'" ~="" & "`dropmiss'"~="yes"){
						qui replace `wvar' = `rwvar' 
						qui replace `wvar' = . if `touse'==0
						}
					tempvar one onea two twoa three 
					qui gen double `one' = (`wvar' / dmax) * (abs(`names1loop'-`names2loop')) 
					qui sum `one'  
					qui gen double `onea' = r(sum) 
					qui sum `wvar'  
					local weightsum = r(sum)
					qui gen double `two' = r(sum) 
					qui gen double `twoa' = `onea'/`two'  
					qui gen double `three' = 1-2*(`twoa') 
						if ("`dyadmiss'"~=""){ 
						qui capture replace tempvar = . in `temp' `dyadmiss'
						}
						else{
						qui capture replace  tempvar = `three' in `temp' 
						}
 					local loopcntr2 = `loopcntr2' + 1	
					local names2loop : word `loopcntr2' of `newordnames'	
						if ("`tempwvar'"~=""){
							drop `one' `onea' `two' `twoa' `three'	`wvar' 
							}
							else{
							drop `one' `onea' `two' `twoa' `three' 
							}
					qui replace `touse' = 1            
					local dyadmiss = ""		    
					local allmiss = ""
					local temp = `temp' + 1
					}
			qui capture replace `names1loop' = tempvar
			qui drop tempvar

	local loopcntr1 = `loopcntr1' + 1 				
	
	local names1loop : word `loopcntr1' of `newordnames'
	}

	if ("`tempwvar'" ~= ""){
	tempvar wvar
	qui gen `wvar' = .
	qui rename `wvar' `throwaway'
	}
	qui order `orgnames'
	
	if ("`tempwvar'" ~= ""){
	qui capture drop `wvar'
	}

	qui reshape long `svarloop', i(`ccode2') j(`ccode1')

	qui drop if `ccode2'<=`ccode1'
	qui sort `ccode1' `ccode2' `id' 
	qui keep `ccode1' `ccode2' `id' `svarloop'
	qui order `ccode1' `ccode2' `id' `svarloop'
	qui rename `svarloop' S_`svarloop'
	qui sum `id'
	display in txt _col(35) "`svarloop' similarity score for year `r(min)' is done"

			qui capture append using S_`svarloop'
			qui capture save S_`svarloop', replace
			

	local idvari = `idvari' + 1
		

	qui  use "`Mfile'", replace
		if("`weightvar'" ~= ""){
		tempvar wvar
		qui capture gen `wvar' = `weightvar'
		}
	}
dis

local svarcntr = `svarcntr' + 1
local svarloop: word `svarcntr' of `svar'
}

tokenize `svar'
	while("`1'" ~=""){
	qui use S_`1', replace
	qui sort `ccode1' `ccode2' `id'
	qui save S_`1', replace
	macro shift
	}
	

tokenize `svar'
qui use S_`1', replace
macro shift 
	while ("`1'"~=""){
	qui sort `ccode1' `ccode2' `id'
	qui merge `ccode1' `ccode2' `id' using S_`1'
	qui drop _merge
	macro shift
	}
	

if("`newfile'"~=""){
	qui sort  `id' `ccode1' `ccode2' 
	qui save "`newfile'", replace
	}
	else{
	qui sort  `id' `ccode1' `ccode2' 
	qui save "S_`svar'_computed.dta", replace
	}

local z = "$S_FN"
if "`combine'"~=""{
	tokenize `svar'
	qui gen S_all = S_`1'
	qui drop S_`1'
	macro shift
	local i = 1
		while ("`1'"~=""){
		qui replace S_all = S_all + S_`1'
		qui drop S_`1'
		local i = `i' + 1
		macro shift
		}
	qui replace S_all = S_all/`i'
	qui save "$S_FN", replace
	}
		
 
tokenize `svar'
	while( "`1'"~=""){
	qui capture erase S_`1'.dta
	macro shift
	}
	
qui capture erase "`Mfile'"
dis _n 
dis  in txt _col(41) "DONE"
end


program define SC2, rclass
syntax varlist(min= 2 max = 2), id(string) svar(varlist) [weightvar(namelist) newfile(string) combine dropmiss(string)]
marksample touse
*markout `touse'
set more off

capture erase S_`svar'_computed.dta


qui save tempfile, replace  

local w = "`weightvar'"


local Mfile = "$S_FN"


tempfile `newfile'

qui gen tempy = _n 			

sort `id'   

tempvar idvar
qui by `id': gen `idvar' = 1 if _n == 1 & `touse' 
qui replace `idvar' = sum(`idvar')   
 

qui sum `idvar'	 			
local idvarcnt = r(max)	  


qui sort tempy				

qui drop tempy

tokenize `varlist'

local ccode1 = "`1'"

local ccode2 = "`2'"





tempfile MMfile
qui capture gen na =.
qui capture gen nb = .
qui save "`MMfile'", replace


local wvarcntr = 1                 
local wvarloop: word 1 of `w'    
local svarcntr = 1
local svarloop: word 1 of `svar'

while ("`wvarloop'"~="" | "`svarloop'" ~= ""){
qui use "`MMfile'", replace

		if("`wvarloop'" == ""){
			qui keep `ccode1' `ccode2' `id' `svar' `idvar' `touse'
			}
			else{
			qui keep `ccode1' `ccode2' `id' `svar' `wvarloop' `idvar' `touse'
			}
			
		if ("`wvarloop'"~="na" & "`wvarloop'"~="nb"){ 
			tempvar wvar
			qui gen double `wvar' = `wvarloop'
			}
			else if ("`wvarloop'"=="na"){           				  
			local tempwvar = "na"    
			}
			else if ("`wvarloop'"=="nb"){              				  
			local tempwvar = "nb"    
			}
	qui save "`Mfile'", replace  




		local idvari = 1			
			while (`idvari' <= `idvarcnt'){
			qui keep if `idvar' == `idvari'	 
	
			qui sum `svarloop'  
			qui gen  max = r(max)
			qui gen  min = r(min)
		
			qui gen dmax = max-min 
			
				if ("`tempwvar'" ~= ""){
				tempvar wvar
				qui gen `wvar' = .
				}
				
			qui keep `ccode1' `ccode2' `id' `svarloop' `idvar' `wvar' dmax  `touse' 
			qui reshape wide `svarloop', i(`ccode2') j(`ccode1')


		
			
		  
		
			qui unab orgnames: _all			
	
			qui order `ccode2' `wvar' `id' dmax 
							       
			qui unab newordnames: _all  	  

		 
		
			tokenize `newordnames'  		       	 
		
			local i = 1

					while (`i' <= 4 ){ 		 

					gettoken `svarloop' newordnames: newordnames /*gettoken `svarloop' names: names  */      
			
					macro shift  			
			
					local i = `i' + 1
					}
	
		tokenize `newordnames'
		local newordnamess = " "
		while ("`1'"~=""){
			if index("`1'", "__")==0{
			local newordnamess "`newordnamess' `1'"
			macro shift
			}
			else{
			macro shift
			}
		}
		
		local newordnames "`newordnamess'"
		qui local namescnt: word count `newordnames'   
					    



	

		local names1loop: word 1 of `newordnames'    		

						    				
		local loopcntr1 = 1	

			if ("`tempwvar'" ~= ""){
			qui unab throwaway: `wvar'
			qui capture drop `wvar'
			}	
					
			tempvar rwvar
			qui capture gen `rwvar' = `wvar'


			while("`names1loop'") ~= "" { 	
			tempvar permtouse
			qui gen byte `permtouse' = 1
			markout `permtouse' `newordnames'

		
				local temp = `loopcntr1' + 1            
	

				local names2loop: word `temp'  of `newordnames' 

			
				local loopcntr2 = `temp' 		 

			 
				qui gen double tempvar = `names1loop'	
					
				
					

					while("`names2loop'") ~= ""{ 

					
					markout `touse' `names1loop' `names2loop'  
					if ("`tempwvar'"~=""){ 
					
						if ("`dropmiss'"=="" & "`wvarloop'"~="nb"){

							tempvar wvar
							qui sum `touse' if `touse'~=0
							qui gen `wvar'= 1/r(N)  if `touse'~=0   
							local missing = ""
							}
							else if ("`dropmiss'"=="" & "`tempwvar'"~="na"){
							tempvar wvar
							qui sum `touse' 
							qui gen `wvar'= 1/r(N)   
							local missing = ""
							}
					}
					if ("`dropmiss'" ==  "dyadic" & "`wvarloop'"~="nb"){
					
							tempvar wvar
							qui sum `touse' if `touse'~=0
							qui gen `wvar'= 1/r(N) if `touse'~=0  
							tempvar dyadtouse1 dyadtouse2
							qui gen byte `dyadtouse1' = 1
							qui gen byte `dyadtouse2' = 1
							markout `dyadtouse1' `names1loop'
							markout `dyadtouse2' `names2loop'
							local namelength = length("`svarloop'")
							local totallength1 = length("`names1loop'")
							local totallength2 = length("`names2loop'")
							local endtoken1 = substr("`names1loop'",(`namelength'+1),(`totallength1'-`namelength'))
							local endtoken2 = substr("`names2loop'",(`namelength'+1),(`totallength2'-`namelength'))	

							qui capture drop `dy1' `dy2'
							tempvar dy1 dy2
							qui gen `dy1' = `ccode2' if `dyadtouse1'==0 & `ccode2'==`endtoken2'

							qui gen `dy2' = `ccode2' if `dyadtouse2'==0 & `ccode2'==`endtoken1'

							qui sum `dy1' 
								if (r(N)!=0){
								qui sum `dy1'
								local ldy1 = r(max)
								qui sum `dy2'
								local ldy2 =r(max)
									if (`ldy1' < `ldy2'){
									local dyadmiss = "if `ccode2'==`ldy1'"

									}
									else{
									local dyadmiss = "if `ccode2'==`ldy2'"
									}
								}
						}
		
					if ("`dropmiss'" ==  "dyadic" & "`wvarloop'"~="na"){
							tempvar wvar
							qui sum `touse' 
							qui gen `wvar'= 1/r(N)
							tempvar dyadtouse1 dyadtouse2
							qui gen byte `dyadtouse1' = 1
							qui gen byte `dyadtouse2' = 1
							markout `dyadtouse1' `names1loop'
							markout `dyadtouse2' `names2loop'
							local namelength = length("`svarloop'")
							local totallength1 = length("`names1loop'")
							local totallength2 = length("`names2loop'")
							local endtoken1 = substr("`names1loop'",(`namelength'+1),(`totallength1'-`namelength'))
							local endtoken2 = substr("`names2loop'",(`namelength'+1),(`totallength2'-`namelength'))	
							qui capture drop `dy1' `dy2'
							tempvar  dy1 dy2
							qui gen `dy1' = `ccode2' if `dyadtouse1'==0 & `ccode2'==`endtoken2'

							qui gen `dy2' = `ccode2' if `dyadtouse2'==0 & `ccode2'==`endtoken1'

							qui sum `dy1' 
								if (r(N)!=0){
								qui sum `dy1'
								local ldy1 = r(max)
								qui sum `dy2'
								local ldy2 =r(max)
									if (`ldy1' < `ldy2'){
									local dyadmiss = "if `ccode2'==`ldy1'"
									}
									else{
									local dyadmiss = "if `ccode2'==`ldy2'"
									}
								}
						}				
						
					if ("`dropmiss'" == "yes"){
					tempvar wvar
					qui sum `touse' if `touse'~=0
					qui gen `wvar'= 1/r(N) if `touse'~=0  
					tempvar remove
					qui capture drop `remove'
					qui gen byte `remove'= 1
					markout `remove' `names1loop' `names2loop'
					qui tab `remove'
						if (r(r)~=1){
						qui replace `wvar'=.
						}
						if (r(r)==1 & "`wvarloop'"~="nb"){
						qui sum `touse' if `touse'~=0
						qui replace `wvar'= 1/r(N) if `touse'~=0 						
						}
						if (r(r)==1 & "`wvarloop'"~="na"){
						tempvar wvar
						qui sum `touse' 
						qui replace `wvar'= 1/r(N)  						
						}
					}
					if ("`wvarloop'"~="na" & "`wvarloop'"~="nb" & "`dropmiss'"~="yes"){
						qui replace `wvar' = `rwvar'
						}
			
					tempvar one onea two twoa three 
					qui gen double `one' = (`wvar' / dmax) * (abs(`names1loop'-`names2loop')) 
					qui sum `one'  
					qui gen double `onea' = r(sum) 
					qui sum `wvar'  
					local weightsum = r(sum)
					qui gen double `two' = r(sum) 
					qui gen double `twoa' = `onea'/`two'  
					qui gen double `three' = 1-2*(`twoa') 
					 
						if ("`dyadmiss'"~=""){ 
						qui capture replace tempvar = . in `temp' `dyadmiss'
						}
						else{
						qui capture replace  tempvar = `three' in `temp' 
						}
					
 					local loopcntr2 = `loopcntr2' + 1	
					local names2loop : word `loopcntr2' of `newordnames'	
						if ("`tempwvar'"~=""){
							drop `one' `onea' `two' `twoa' `three'	`wvar' 
							}
							else{
							drop `one' `onea' `two' `twoa' `three' 
							}
					qui replace `touse' = 1             
					local dyadmiss = ""		    
					local allmiss = ""
					local temp = `temp' + 1
					}
			qui capture replace `names1loop' = tempvar
			qui drop tempvar

	local loopcntr1 = `loopcntr1' + 1 				
	
	local names1loop : word `loopcntr1' of `newordnames'
	}
	if ("`tempwvar'" ~= ""){
	tempvar wvar
	qui gen `wvar' = .
	qui rename `wvar' `throwaway'
	}
	
qui order `orgnames'
	
	if ("`tempwvar'" ~= ""){
	qui capture drop `wvar'
	}
	
		qui reshape long `svarloop', i(`ccode2') j(`ccode1')
		qui drop if `ccode2'<=`ccode1'
		qui sort `ccode1' `ccode2' `id' 
		qui keep `ccode1' `ccode2' `id' `svarloop'
		qui order `ccode1' `ccode2' `id' `svarloop'
		qui rename `svarloop' S_`svarloop'
		qui sum `id'
		display in txt _col(35) "`svarloop' similarity score for year `r(min)' is done"
			qui capture append using S_`svarloop'
			qui capture save S_`svarloop', replace
		local idvari = `idvari' + 1
		qui  use "`Mfile'", replace
			if ("`wvarloop'"~="na" & "`wvarloop'"~="nb"){ 
			tempvar wvar
			qui gen double `wvar' = `wvarloop'
			} 
		}
	dis

local svarcntr = `svarcntr' + 1
local svarloop: word `svarcntr' of `svar'

local tempwvar = ""   

local wvarcntr = `wvarcntr' + 1
local wvarloop: word `wvarcntr' of `weightvar'
}

tokenize `svar'
	while("`1'" ~=""){
	qui use S_`1', replace
	qui sort `ccode1' `ccode2' `id'
	qui save S_`1', replace
	macro shift
	}


tokenize `svar'
qui use S_`1', replace
macro shift 
	while ("`1'"~=""){
	qui sort `ccode1' `ccode2' `id'
	qui merge `ccode1' `ccode2' `id' using S_`1'
	qui drop _merge
	macro shift
	}
	

if("`newfile'"~=""){
	qui sort `id' `ccode1' `ccode2'
	qui save "`newfile'", replace
	}
	else{
	qui sort `id' `ccode1' `ccode2'
	qui save "S_`svar'_computed.dta", replace
	}


local z = "$S_FN"
if "`combine'"~=""{
	tokenize `svar'
	qui gen S_all = S_`1'
	qui drop S_`1'
	macro shift
	local i = 1
		while ("`1'"~=""){
		qui replace S_all = S_all + S_`1'
		qui drop S_`1'
		local i = `i' + 1
		macro shift
		}
	qui replace S_all = S_all/`i'
	qui save "$S_FN", replace
	}
	
tokenize `svar'
	while( "`1'"~=""){
	qui capture erase S_`1'.dta
	macro shift
	}

	
qui capture erase "`Mfile'"
dis _n 
dis  in txt _col(41) "DONE"

end


					
					



