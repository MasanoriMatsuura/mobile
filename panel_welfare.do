**Mobile money and human capital**
**Author: Masanori Matsuura**
**2023/01/16**

clear all
set more off
/* Install reghdfe
ssc install reghdfe
* Install ftools (remove program if it existed previously)
ssc install ftools
* Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2

*install quantile regression
ssc install xtqreg

* Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from(https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/)

*install quantile regression
ssc install xtqreg
* depict lorenz curve
ssc install lorenz

* Propensity score matching
ssc install psmatch2
*/

*set the pathes
global climate = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\Do"
global BIHS18Community = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\BIHS2018\BIHSRound3\Community"
global BIHS18Female = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\BIHS2018\BIHSRound3\Female"
global BIHS18Male = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\BIHS2018\BIHSRound3\Male"
global BIHS15 = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\BIHS2015"
global BIHS12 = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\BIHS2012"
global table = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\table"
global graph = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_money\BIHS\figure"

/*creating a panel dataset*/
use 2015.dta, clear
append using 2018.dta, force

/*some cleaning*/
gen lnfrm=log(farmsize) // logarithem of farm size 100 decimal = 0.4 ha
label var lnfrm "Farm size (log)"
recode year (2015=1)(nonm=0), gen(year2015)
label var year2015 "Year 2015"

recode dvcode (10 50 55=1)(nonm=0), gen(pov_div) //division dummy
*division
recode dvcode (10=1 "Barisal" )(20=2 "Chattogram") (30=3 "Dhaka") (40=4 "Khulna") (50=5 "Rajshahi") (55=6 "Rangpur") (60=7 "Sylhet"), gen(division)
label var division "Division"
label var aginc "Farm self"
label var frmwage "Farm wage"
label var nonself "Off-farm self"
label var nonwage "Off-farm wage and salary"
label var nonearn "Non-earned"
label var ttinc "Total household income"
label var hdds "Household Dietary Diversity Score"
label var asset "Asset index"
label variable enex_d "Energy expenditure"
label variable elcex_d "Electricity expenditure"
label variable gasex_d "Gas expenditure"
label variable nonex_d "Non-clean engergy expenditure"
replace inc_div=. if inc_div==1
replace shni=. if inc_div==1
label var saving "Saving" 
foreach m in mm_p mm_o remi_d saving_d mhlth_d fhlth_d hlth_d edu_d fedu_d medu_d shock e_shock ad_size ch_size leisure_d enex_d elcex_d gasex_d nonex_d dum_el dum_gas magency {
	replace `m'=0 if `m'==.
}
recode remi (0=0)(nonm=1), gen(rem_d)


*mobile phone
label var mobile "Mobile phone"

*create peer effect variables
sort uncode year
by uncode year: egen mobile_nc=sum(mm_o) 
by uncode year: egen total_nc=count(a01)
gen mobile_union=(mobile_nc-mm_o)/(total_nc-1) //creating peer effect
label var mobile_union "Share of households adopting mobile money in the union"

*gender of household head
recode Male (1=0 "no")(0=1 "yes"), gen(female)
label var female "Female household head"

*create village-level average household characteristics
foreach m in female age_hh hh_size schll_hh farmsize asset road {
	bysort a01: egen mn_`m'=mean(`m')
}

** CRE household mean
foreach m in female age_hh schll_hh ad_size ch_size asset {
	bysort a01: egen m_`m'=mean(`m')
}

*create log
gen lnhdds=log(hdds)
gen lnexp=log(pc_expm_d+1)
gen lnfexp=log(pc_foodxm_d+1)
gen lnnexp=log(pc_nonfxm_d+1)
gen lnrem=log(remi_d+1)
gen lnsaving=log(saving_d+1)
gen lnedu=log(edu_d+1)
gen lnmed=log(medu_d+1)
gen lnfed=log(fedu_d+1)
gen lnhl=log(hlth_d+1)
gen lnmh=log(mhlth_d+1)
gen lnfh=log(fhlth_d+1)
gen lnleisure=log(leisure_d+1)
foreach m in enex_d elcex_d gasex_d nonex_d {
	gen ln_`m'=log(`m'+1)
}
foreach m in ln_enex_d ln_elcex_d ln_gasex_d ln_nonex_d {
	label var `m' " `m' (log)"
}
label var lnsaving "Saving (log)"
label var lnexp "Per capita expenditure (log)"
label var lnfexp "Per capita food expenditure (log)"
label var lnnexp "Per capita non-food expenditure (log)"
label var lnrem "Remittance (log)"
label var lnedu "Educational expenditure (log)"
label var lnmed "Male educational expenditure (log)"
label var lnfed "Female educational expenditure (log)"
label var lnhl "Health expenditure (log)"
label var lnmh "Male health expenditure (log)"
label var lnfh "Female health expenditure (log)"
label var lnleisure "Leisure expenditure (log)"

*off-farm emlpoyment/self employment (dummy)
recode nonwage (0 .=0 "No")(nonm=1 "Yes"), gen("offfarm")
label var offfarm "Off-farm employment (dummy)"
recode nonself (0 .=0 "No")(nonm=1 "Yes"), gen("nonfarmself")
label var nonfarmself "Off-farm self employment (dummy)"
gen offincome=(offrminc+offself)
replace offincome=0 if offincome==.
gen ln_offinc=log(offincome+1)
gen ln_nonwage=log(nonwage+1)
gen ln_nonself=log(nonself+1)

*per capita total income
gen pcti=ttinc/hh_size
label var pcti "Per capita total income"

*log total income, per capita total income, and poverty
gen ln_ttlinc=log(ttinc+1)
gen ln_pctinc=log(pcti+1)
label var ln_ttlinc "Total household income (log)"
label var ln_pctinc "Per capita total income (log)"

gen povertyhead=p190hcgcpi/100
label var povertyhead "Povery headcount (1/0)"

*climate shock
gen thresh_fk=hs+sds //1sd + mean threshhold of flood in Kharif
gen kfshock=s-thresh_fk
recode kfshock (0/max=1)(nonm=0), gen(floodk) //flood if more than 0
label var floodk "Flood shock in Kharif"
gen thresh_fr=hr+sdr //threshhold of flood in Rabi
gen rfshock=r-thresh_fr
recode rfshock (0/max=1)(nonm=0), gen(floodr)
label var floodr "Flood shock in Rabi"
gen flood_t=floodk+floodr
recode flood_t (0=0)(nonm=1), gen(flood)
label var flood "Flood shock"

gen thresh_kd=hs-sds //threshhold of drought in Kharif
gen kdshock=s-thresh_kd 
recode kdshock (min/0=1)(nonm=0), gen(droughtk) //drought if less than 0
label var droughtk "Drought shock in Kharif"
gen thresh_rd=hr-sdr //threshhold of drought in Rabi
gen rdshock=s-thresh_rd 
recode rdshock (min/0=1)(nonm=0), gen(droughtr) //drought if less than 0
label var droughtr "Drought shock in Rabi"

gen rainshockr=1 if floodr==1
replace rainshockr=1 if droughtr==1
replace rainshockr=0 if rainshockr==. 

gen rainshockk=1 if floodk==1
replace rainshockk=1 if droughtk==1
replace rainshockk=0 if rainshockk==. 
label var rainshockr "Rainfall shock in Rabi"
label var rainshockk "Rainfall shock in Kharif"


gen floodshock=1 if rainshockk==1|rainshockr==1
replace floodshock=0 if floodshock==.

label var sds "20-year Kharif rainfall SD"
label var sdr "20-year Rabi rainfall SD"
label var sdst "20-year Kharif temperature SD"
label var sdrt "20-year Rabi temperature SD"

gen rshock_k=(s-hs)/sds
gen rshock_r=(r-hr)/sdr
label var rshock_k "Rainfallshock in Kharif"
label var rshock_r "Rainfall shock in Rabi"

gen lkfshock=ls-thresh_fk
recode lkfshock (0/max=1)(nonm=0), gen(floodkl)
label var floodkl "Flood shock 1-year lag in Kharif"
gen lrfshock=lr-thresh_fr
recode lrfshock (0/max=1)(nonm=0), gen(floodrl)
label var floodrl "Flood shock 1-year lag in Rabi"
gen lflood_t=floodkl+floodrl
recode lflood_t (0=0)(nonm=1), gen(lflood)
label var lflood "Flood shock 1-year lag"
gen lkdshock=ls-thresh_kd
recode lkdshock (min/0=1)(nonm=0), gen(droughtkl)
label var droughtkl "Drought shock 1-year lag in Kharif"
gen lrdshock=lr-thresh_rd
recode lrdshock (min/0=1)(nonm=0), gen(droughtrl)
label var droughtrl "Drought shock 1-year lag in Rabi"

gen cv_kharif=sds/hs
gen cv_rabi=sdr/hr
gen cv_kharif_t=sdst/hst
gen cv_rabi_t=sdrt/hrt
save panel.dta, replace

*Descriptive statistics
bysort year a01: egen treat=sum(mm_o)

sort year mm_o
by year mm_o: summarize pc_foodxm_d pc_nonfxm_d ttinc rshock_k shock female age_hh hh_size schll_hh irrigation asset stshock rtshock if use==1

ttest offfarm if  year==2015, by(mobile)
ttest povertyhead if  year==2015, by(mobile)
ttest mpiscore if  year==2015, by(mobile)
ttest female if  year==2015, by(mobile)
ttest age_hh if  year==2015, by(mobile)
ttest hh_size if  year==2015, by(mobile)
ttest schll_hh if  year==2015, by(mobile)
ttest asset if  year==2015, by(mobile)
ttest farmsize if  year==2015, by(mobile)
ttest bazaar if  year==2015, by(mobile)
ttest road if  year==2015, by(mobile)

graph bar mm over(year) ytitle("Mobile phone ownership") title("mobile phone ownership from 2011 to 2019") note("Source: Bangladesh Integrated Household Survey 2015 and 2019") scheme(s1mono)
graph export $graph/phone_overtime.jpg, replace

** poverty rate by division
graph bar povertyhead if _est_est2==1, over(division) ytitle("Povevrty rate by Division") ///
        blabel(bar, size(medium) color("white") format(%9.2f) pos(inside)) ///
		scheme(s1mono)
graph export $graph\pv_div.png, replace


*the effect of flood shock on consumption, hdds, poverty, remittance by mobile money
**Difference in Differences
use panel, clear

*DID
gen did_o=mm_o*rshock_k //objective rainfall shock
label var did_o "Interaction"
gen did_s=mm_o*shock //subjective negative economic shock
label var did_s "Interaction"

*Interaction term
foreach m in female age_hh hh_size schll_hh market asset ln_farm {
	gen int_`m'_s=`m'*shock
}
foreach m in female age_hh hh_size schll_hh market asset ln_farm {
	gen int_`m'_o=`m'*rshock_k
}

label var cv_kharif "CoV of rainfall in Kharif"
label var cv_rabi "CoV of rainfall in Rabi"
label var cv_kharif_t "CoV of temperature in Kharif"
label var cv_rabi_t "CoV of temperature in Rabi"

   
xtset a01 year
global control female age_hh hh_size schll_hh market asset stshock rtshock dvcode#year

global control1 female age_hh hh_size schll_hh market asset stshock rtshock int_female_o int_age_hh_o int_hh_size_o int_schll_hh_o int_market_o int_asset_o  dvcode#year

global control2 female age_hh hh_size schll_hh market asset stshock rtshock int_female_s int_age_hh_s int_hh_size_s int_schll_hh_s int_market_s int_asset_s dvcode#year

** correlation between subjective and objective shocks and testing the exogeneity of the shocks
eststo clear
eststo: reghdfe shock mm_o mobile_union female age_hh hh_size schll_hh market asset dvcode#year, a(a01 dvcode year) vce(robust)

esttab using $table\corr.rtf, b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s(hh division control N, label("Household FE" "Division FE" "Year FE" "Control variables" "Observations"))
gen use=e(sample)

** DiD
gen end_int_s=mobile_union*shock
gen end_int_o=mobile_union*rshock_k

eststo clear
**objective shocks TWFE
eststo: reghdfe lnfexp mm_o rshock_k did_o $control, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace

eststo: reghdfe lnnexp mm_o rshock_k did_o $control, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace

eststo: reghdfe lnfexp mm_o rshock_k did_o $control1, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace
eststo: reghdfe lnnexp mm_o rshock_k did_o $control1, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace

**subjective shocks TWFE
eststo: reghdfe lnfexp mm_o shock did_s $control, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace
eststo: reghdfe lnnexp mm_o shock did_s $control, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace
eststo: reghdfe lnfexp mm_o shock did_s $control2, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace
eststo: reghdfe lnnexp mm_o shock did_s $control2, a(a01 dvcode year)  vce(robust)
quietly estadd local hh Yes, replace
quietly estadd local division Yes, replace
quietly estadd local year Yes, replace

esttab using $table\did1.rtf, order(mm_o did_o did_s rshock_k shock ) keep(mm_o rshock_k shock did_o did_s) b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s(hh division control N, label("Household FE" "Division FE" "Year FE" "Control variables" "Observations"))


** IV-FE
eststo clear
**objective
eststo: ivreghdfe lnfexp rshock_k $control (mm_o  did_o =mobile_union end_int_o), a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control (mm_o  did_o =mobile_union end_int_o), a(a01 dvcode year) robust
eststo: ivreghdfe lnfexp rshock_k $control1 (mm_o  did_o =mobile_union end_int_o), a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control1 (mm_o  did_o =mobile_union end_int_o), a(a01 dvcode year) robust
**subjective
eststo: ivreghdfe lnfexp shock $control (mm_o  did_s =mobile_union end_int_s), a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp shock $control (mm_o  did_s =mobile_union end_int_s), a(a01 dvcode year) robust
eststo: ivreghdfe lnfexp shock $control2 (mm_o  did_s =mobile_union end_int_s), a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp shock $control2 (mm_o  did_s =mobile_union end_int_s), a(a01 dvcode year) robust

**out
esttab using $table\did2.rtf, order(mm_o  did_o rshock_k did_s shock) keep(mm_o rshock_k shock did_o did_s) b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s(hh division control N, label("Household FE" "Division FE" "Year FE" "Control variables" "Observations"))

**heterogeneity
** IV-FE

** poorer vs non-poorer in 2015
gen non_pov_base=1 if pc_expm_d>2524.567 & year==2015
replace non_pov_base=2 if pc_expm_d<2524.567 & year==2015


replace non_pov_base=1 if 1884.899>pc_expm_d & year==2015
replace non_pov_base=2 if 2561.714> pc_expm_d & pc_expm_d>= 1884.899 & year==2015
replace non_pov_base=3 if 3610.671> pc_expm_d & pc_expm_d>= 2561.714 & year==2015
replace non_pov_base=4 if pc_expm_d>=3610.671 & year==2015

gen non_pov_inc=1 if ttinc>=11520 & year==2015
replace non_pov_inc=0 if ttinc<115207 & year==2015

** far division from dhaka
gen povdiv=1 if dvcode==55
replace povdiv=1 if dvcode==60
replace povdiv=1 if dvcode==50

** off-farm income or not in 2015
gen did_off=did_o*offfarm
gen rshock_off=rshock_k*offfarm

gen end_did_off=mobile_union*offfarm*rshock_k

label var did_off "Shock Off-farm employment"
label var rshock_off "Shock Off-farm employment"


**objective
** far division or near division
eststo clear
eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if povdiv==1, a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if povdiv==1, a(a01 dvcode year) robust

eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if povdiv!=1, a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if povdiv!=1, a(a01 dvcode year) robust

esttab using $table\did_pov_het_1.rtf, keep(mm_o rshock_k did_o) b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s(hh division control N, label("Household FE" "Division FE" "Year Division" "Control variables" "Observations"))


** consumption quota
eststo clear
**below in 2015
eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_base!=3 & non_pov_base!=4, a(a01 dvcode year) robust //non_pov_base!=2 & 
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if  non_pov_base!=3 & non_pov_base!=4, a(a01 dvcode year) robust //


*above in 2015
eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_base!=2 & non_pov_base!=1 , a(a01 dvcode year) robust //& non_pov_base!=4
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_base!=2 & non_pov_base!=1 , a(a01 dvcode year) robust //& non_pov_base!=4

/**2nd in 2015
eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_base!=1 & non_pov_base!=3 & non_pov_base!=4, a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_base!=1 & non_pov_base!=3 & non_pov_base!=4, a(a01 dvcode year) robust

**4th in 2015
eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_base!=2 & non_pov_base!=3 & non_pov_base!=1, a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_base!=2 & non_pov_base!=3 & non_pov_base!=1, a(a01 dvcode year) robust*/

esttab using $table\did_pov_het_2.rtf, keep(mm_o rshock_k did_o) b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s(hh division control N, label("Household FE" "Division FE" "Year Division" "Control variables" "Observations"))

** income quota
eststo clear
eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_inc!=1, a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_inc!=1, a(a01 dvcode year) robust

eststo: ivreghdfe lnfexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_inc!=0, a(a01 dvcode year) robust
eststo: ivreghdfe lnnexp rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if non_pov_inc!=0, a(a01 dvcode year) robust

esttab using $table\did_pov_het_3.rtf, keep(mm_o rshock_k did_o) b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s(hh division control N, label("Household FE" "Division FE" "Year Division" "Control variables" "Observations"))


** Mechanism: remittance
replace remi_d=0 if remi==.
replace remi_for_d=0 if remi_for_d==.
replace remi_dom_d=0 if remi_dom_d==.

label var remi_d "Value of remittance (deflated)"
label var remi_dom_d "Value of domestic remittance (deflated)"
label var remi_for_d "Value of foreign remittance (deflated)"

replace remi_n=0 if remi_n==.
gen r_remi=sqrt(remi_d) 
gen r_remi_dom=sqrt(remi_dom_d) 
gen r_remi_for=sqrt(remi_for_d)
gen ln_remi=log(remi_d+1)
gen ln_remi_dom=log(remi_dom_d+1)
gen ln_remi_for=log(remi_for_d+1)

eststo clear
** objective shocks
**Domestic
eststo: ivtobit remi_dom_d rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if use==1,  ll(0)
quietly estadd local division Yes, replace
quietly estadd local control Yes, replace
**Foreign
eststo: ivtobit remi_for_d rshock_k $control1  (mm_o  did_o =mobile_union end_int_o) if use==1 ,  ll(0)
quietly estadd local division Yes, replace
quietly estadd local control Yes, replace


*other total value and frequency
ivreghdfe remi_d rshock_k $control  (mm_o  did_o =mobile_union end_int_o) if use==1,  a(a01) robust
ivreghdfe remi_n rshock_k $control  (mm_o  did_o =mobile_union end_int_o) if use==1,  a(a01) robust
ivreghdfe remi_dom_d rshock_k $control  (mm_o  did_o =mobile_union end_int_o) if use==1,  a(a01) robust
ivreghdfe remi_for_d rshock_k $control  (mm_o  did_o =mobile_union end_int_o) if use==1,  a(a01) robust

esttab using $table\did_rem.rtf, keep(mm_o rshock_k did_o) b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s( division control N, label("Division Year" "Control variables" "Observations"))

** falsification test
eststo clear
eststo: reghdfe lnfexp mobile_union $control if mm_o==0, a(a01 dvcode year)  vce(robust)
eststo: reghdfe lnnexp mobile_union $control if mm_o==0, a(a01 dvcode year)  vce(robust)
esttab using $table\falsification.rtf, keep(mobile_union) b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons s(hh division control N, label("Household FE" "Division Year" "Control variables" "Observations"))
