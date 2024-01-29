 /*Mobile Phone and Household Welfare: Analysis*/

/*Author: Masanori Matsuura*/
clear all
set more off
ssc inst ftools
ssc inst reghdfe

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


*gini decomposition
ssc install descogini

*set the pathes
global climate = "C:\Users\user\Documents\Masterthesis\climatebang"
global BIHS18Community = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2018\dataverse_files\BIHSRound3\Community"
global BIHS18Female = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2018\dataverse_files\BIHSRound3\Female"
global BIHS18Male = "C:\Users\user\Documen ts\research\saiful\mobile_phone\BIHS\BIHS2018\dataverse_files\BIHSRound3\Male"
global BIHS15 = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2015"
global BIHS12 = "C:\Users\user\Documents\research\saiful\mobile_phone\BIHS\BIHS2012"
global table = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_phone\BIHS\table"
global graph = "C:\Users\Masanori_Matsuura\Documents\Research\mobile_phone\BIHS\figure"

/*creating a panel dataset*/
//use 2015.dta, clear
//append using 2012.dta, force
//append using 2018.dta, force
//append using 2012.dta, force
use 2012.dta, clear
append using 2018.dta, force

/*some cleaning*/
gen lnfrm=log(farmsize) // logarithem of farm size 100 decimal = 0.4 ha
label var lnfrm "Farmsize (log)"
recode year (2012=1)(nonm=0), gen(year2012) //year dummy
recode year (2018=1)(nonm=0), gen(year2018)
label var year2012 "Year 2012"
label var year2018 "Year 2018"
recode dvcode (55=1)(nonm=0), gen(Rangpur) //division dummy
label var aginc "Farm self"
label var frmwage "Farm wage"
label var nonself "Off-farm self"
label var nonwage "Off-farm wage and salary"
label var nonearn "Non-earned"
label var ttinc "Total household income"
label var hdds "Household Dietary Diversity Score"
label var asset "Asset index"
replace crp_div=. if crp_div==1
replace inc_div=. if inc_div==1
replace shnc=. if crp_div==1
replace shni=. if inc_div==1

*mobile ownership and internet access
label var mobile "MP ownership"
replace internet=0 if internet==.

*create peer effect variables
sort Village year
by Village year: egen mobile_nc=sum(mobile) 
by Village year: egen total_nc=count(a01)
gen mobile_village=(mobile_nc-mobile)/(total_nc-1) //creating peer effect
label var mobile_village "Share of households adopting mobile phone in the village"

sort Village year
by Village year: egen intrnt_nc=sum(internet) 
by Village year: egen total_intrnt_nc=count(a01)
gen intrnt_village=(intrnt_nc-internet)/(total_intrnt_nc-1) //creating peer effect
label var intrnt_village "Share of households accessing internet in the village"

sort Village year
gen inc_div_d=1 if inc_div>0
replace inc_div_d=0 if inc_div_d==.
by Village year: egen inc_nc=sum(inc_div_d) 
by Village year: egen total_inc_nc=count(a01)
gen inc_div_village=(inc_nc-inc_div_d)/(total_inc_nc-1) //creating peer effect
label var inc_div_village "Share of households adopting income diversification in the village"

*division
recode dvcode (10=1 "Barisal" )(20=2 "Chattogram") (30=3 "Dhaka") (40=4 "Khulna") (50=5 "Rajshahi") (55=6 "Rangpur") (60=7 "Sylhet"), gen(division)
label var division "Division"
*gender of household head
recode Male (1=0 "no")(0=1 "yes"), gen(female)
label var female "Female household head"

*create village-level average household characteristics
by Village year: egen vllg_fml=mean(female)
by Village year: egen vllg_age=mean(age_hh)
by Village year: egen vllg_hhsz=mean(hh_size)

by Village year: egen vllg_schl=mean(schll_hh)
by Village year: egen vllg_frm=mean(farmsize)
by Village year: egen vllg_bzr=mean(bazaar)
by Village year: egen vllg_rd=mean(road)
by Village year: egen vllg_extnsn=mean(extension)

*create log hdds and expenditure
gen lnhdds=log(hdds)
gen lnexp=log(pc_expm_d)
gen lnfexp=log(pc_foodxm_d)

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

gen d_town_m=d_town/1000
gen lnnnearn=log(nonearn+1)
gen lnnnslf=log(nonself+1)
gen lnnnwg=log(nonwage+1)
gen lnfrmwg=log(frmwage+1)
gen lnfrmslf=log(aginc+1)
label var lnnnearn "Non-earned"
label var lnnnslf "Off-farm self"
label var lnnnwg "Off-farm wage and salary"
label var lnfrmwg "Farm wage"
label var lnfrmslf "Farm self"

gen povertyhc=100*povertyhead
gen povgap=100*deppov190gcpi
gen mpi=100*mpiscore
label var d_town_m "Distance to nearest town (km)"
save panel.dta, replace
export delimited using panel.csv, replace //output as csv

*Descriptive statistics
bysort year : tab mobile  if _est_est2==1

sort year mobile
by year mobile:  summarize inc_div povertyhc povgap mpi female age_hh hh_size schll_hh farmsize lvstck town if year==2012 & mobile !=. & _est_est2==1

by year mobile:  summarize inc_div povertyhc povgap mpi female age_hh hh_size schll_hh farmsize lvstck town if year==2018 & mobile !=. & _est_est2==1

ttest nonearn  if  year==2012 & _est_est2==1, by(mobile)
ttest frmwage if  year==2012 & _est_est2==1, by(mobile)
ttest aginc if  year==2012 & _est_est2==1, by(mobile)
ttest nonself if  year==2012 & _est_est2==1, by(mobile)
ttest nonwage if  year==2012 & _est_est2==1, by(mobile)

ttest inc_div if year==2012 & _est_est2==1, by(mobile)
ttest povertyhead if  year==2012 & _est_est2==1, by(mobile)
ttest deppov190gcpi if  year==2012 & _est_est2==1, by(mobile)
ttest mpiscore if  year==2012 & _est_est2==1, by(mobile)
ttest ttinc if  year==2012 & _est_est2==1, by(mobile)
ttest pcti if  year==2012 & _est_est2==1, by(mobile)
ttest female if  year==2012 & _est_est2==1 , by(mobile)
ttest age_hh if  year==2012 & _est_est2==1, by(mobile)
ttest hh_size if  year==2012 & _est_est2==1, by(mobile)
ttest schll_hh if  year==2012 & _est_est2==1, by(mobile)
ttest asset if  year==2012 & _est_est2==1, by(mobile)
ttest farmsize if  year==2012 & _est_est2==1, by(mobile)
ttest bazaar if  year==2012 & _est_est2==1, by(mobile)
ttest town if  year==2012 & _est_est2==1, by(mobile)

ttest nonearn  if  year==2018 & _est_est2==1, by(mobile)
ttest frmwage if  year==2018 & _est_est2==1, by(mobile)
ttest aginc if  year==2018 & _est_est2==1, by(mobile)
ttest nonself if  year==2018 & _est_est2==1, by(mobile)
ttest nonwage if  year==2018 & _est_est2==1, by(mobile)

ttest inc_div if  year==2018 & _est_est2==1, by(mobile)
ttest povertyhead if  year==2018 & _est_est2==1, by(mobile)
ttest deppov190gcpi if  year==2018 & _est_est2==1, by(mobile)
ttest mpiscore if  year==2018 & _est_est2==1, by(mobile)
ttest ttinc if  year==2018 & _est_est2==1, by(mobile)
ttest pcti if  year==2018  & _est_est2==1, by(mobile)
ttest female if  year==2018 & _est_est2==1, by(mobile)
ttest age_hh if  year==2018 & _est_est2==1, by(mobile)
ttest hh_size if  year==2018 & _est_est2==1, by(mobile)
ttest schll_hh if  year==2018 & _est_est2==1, by(mobile)
ttest asset if  year==2018 & _est_est2==1, by(mobile)
ttest farmsize if  year==2018 & _est_est2==1, by(mobile)
ttest bazaar if  year==2018 & _est_est2==1, by(mobile)
ttest town if  year==2018 & _est_est2==1, by(mobile)

//povertyhead deppov190gcpi mpiscore ttinc pcti mobile_village hs hr ha hw s r a w hst hrt hat hwt ts tr ta tw Male age_hh hh_size schll_hh asset lnfrm bazaar road irrigation extension 
*mobile phone ownership overtime
graph bar mobile, over(year) ytitle("Mobile phone ownership") title("mobile phone ownership from 2011 to 2019") note("Source: Bangladesh Integrated Household Survey 2011/12, 2018/19") scheme(s1mono)
graph export $graph/phone_overtime.jpg, replace

*numbe of households in the sample using and not using mobile phones
eststo clear
sort year
by year: eststo: quietly estpost tab mobile
esttab  using $table\mobileowner_year.rtf, label nodepvar replace addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19)

*composition of income
graph pie aginc frmwage nonself nonwage nonearn if year==2012 & mobile==1, plabel(_all percent, color(white)) subtitle("2011/12") saving(pie12)
graph pie aginc frmwage nonself nonwage nonearn if year==2018 & mobile==1,  plabel(_all percent, color(white)) saving(pie18) subtitle("2018/19")
graph pie aginc frmwage nonself nonwage nonearn if year==2012 & mobile==0, plabel(_all percent, color(white)) subtitle("2011/12") saving(pie12)
graph pie aginc frmwage nonself nonwage nonearn if year==2018 & mobile==0,  plabel(_all percent, color(white)) saving(pie18) subtitle("2018/19")
graph display, scheme(s1mono) 
graph export $graph\income_dist.png, replace

graph pie aginc frmwage nonself nonwage nonearn if  mobile==1, plabel(_all percent, color(white)) subtitle("MP ownership") saving(mpo)
graph pie aginc frmwage nonself nonwage nonearn if  mobile==0,  plabel(_all percent, color(white)) saving(nono) subtitle("Non-ownership") 
gr combine mpo.gph nono.gph, title("Breakdown of household income") note(Source: "BIHS2011/12 and 2018/19 calculated by author") 
graph display, scheme(s1mono) 
graph export $graph\income_mp.png, replace


*the effect of mobile phone total income, off-farm income, poverty headcount, poverty gap, MPI score, MPI
global control "female age_hh hh_size schll_hh farmsize lvstck town dvcode#year" //

eststo clear
eststo: reghdfe inc_div mobile $control ,a(a01) vce(r)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe lnfrmslf mobile $control ,a(a01) vce(r)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe lnfrmwg mobile $control ,a(a01) vce(r)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe lnnnslf mobile $control ,a(a01) vce(r)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe lnnnwg mobile $control ,a(a01) vce(r)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe lnnnearn mobile $control ,a(a01) vce(r)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

esttab  using $table\incdiv_full.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12 and 2018/19.) keep(mobile female age_hh hh_size schll_hh farmsize lvstck town ) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles("Income diversification" "Farm self" "Farm wage" "Off-farm self" "Off-farm wage and salary" "Non-earned")star(* 0.10 ** 0.05 *** 0.01)

gen est=1 if _est_est2==1
eststo clear
eststo: reghdfe povertyhc mobile $control ,a(a01) vce(r)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace
 
eststo: reghdfe povgap mobile $control ,a(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpi mobile $control ,a(a01) vce(r)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace


esttab  using $table\2ndstage.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2019.) order(mobile $control) keep(mobile female age_hh hh_size schll_hh farmsize lvstck town ) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Income diversification" "Poverty Headcount" "Depth of poverty" "MPI score" "Total household income (log)")star(* 0.10 ** 0.05 *** 0.01)

** poverty by division
graph hbar povertyhead if _est_est2==1, ///
over(division) ///
over(year) ///
blabel(bar, size(medium) color("white") format(%9.2f) pos(inside)) ///
ytitle("Poverty rate by Division") ///
scheme(s1mono)
graph export $graph\pv_div.png, replace

graph hbar povertyhead if _est_est2==1, ///
over(year) ///
blabel(bar, size(medium) color("white") format(%9.2f) pos(inside)) ///
ytitle("Povevrty rate") ///
scheme(s1mono)
*** potential mechamism
eststo clear
eststo: reghdfe povertyhc inc_div mobile $control, a(a01) vce(robust)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace
 
eststo: reghdfe povgap inc_div mobile $control, a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpi inc_div mobile $control, a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

esttab  using $table\mechanism.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2019.) keep(mobile inc_div) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Poverty Headcount" "Depth of poverty" "MPI score" "Per capita expenditure (log)" "Per capita food expenditure (log)")star(* 0.10 ** 0.05 *** 0.01)

esttab  using $table\mechanism_full.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2019.) keep(mobile inc_div female age_hh hh_size schll_hh farmsize lvstck town) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Poverty Headcount" "Depth of poverty" "MPI score" "Per capita expenditure (log)" "Per capita food expenditure (log)")star(* 0.10 ** 0.05 *** 0.01)

eststo clear
eststo: reghdfe povertyhc lnfrmslf lnfrmwg lnnnslf lnnnwg lnnnearn mobile $control, a(a01) vce(robust)
quietly estadd local FE No, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace
 
eststo: reghdfe povgap lnfrmslf lnfrmwg lnnnslf lnnnwg lnnnearn mobile $control, a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe mpi lnfrmslf lnfrmwg lnnnslf lnnnwg lnnnearn mobile $control, a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace


esttab  using $table\mechanism_each.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2019.) keep(lnfrmslf lnfrmwg lnnnslf lnnnwg lnnnearn mobile) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Poverty Headcount" "Depth of poverty" "MPI score")star(* 0.10 ** 0.05 *** 0.01)


esttab  using $table\mechanism_each_full.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2019.) keep(lnfrmslf lnfrmwg lnnnslf lnnnwg lnnnearn mobile female age_hh hh_size schll_hh farmsize lvstck town) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "Poverty Headcount" "Depth of poverty" "MPI score")star(* 0.10 ** 0.05 *** 0.01)

**falsification test
eststo clear
eststo:reg mobile mobile_village $control if est==1, vce(robust)
eststo:reg inc_div mobile_village $control if mobile==0 & est==1, vce(robust)
eststo:reg povertyhc mobile_village $control if mobile==0 & est==1, vce(robust)
eststo:reg povgap mobile_village $control if mobile==0 & est==1, vce(robust)
eststo:reg mpi mobile_village $control if mobile==0 & est==1, vce(robust)
esttab  using $table\falcification.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2019.) keep(mobile_village) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles( "MP ownership" "Income diversification" "Poverty Headcount" "Depth of poverty" "MPI score" )star(* 0.10 ** 0.05 *** 0.01)

*heterogeneous analysis
** gender
recode division (6=1 "Yes")(nonm=0 "No"), gen(rangpur)

gen fmlmbl=female*mobile
gen twnmbl=town*mobile
gen schmbl=schll_hh*mobile
gen rngprmbl=rangpur*mobile
eststo clear
eststo: reghdfe inc_div fmlmbl mobile $control ,a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe inc_div twnmbl mobile $control ,a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe inc_div schmbl mobile $control ,a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

eststo: reghdfe inc_div rngprmbl mobile $control ,a(a01) vce(robust)
quietly estadd local FE Yes, replace
quietly estadd local year Yes, replace
quietly estadd local control Yes, replace

esttab  using $table\hetero.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19.) order(mobile schmbl rngprmbl fmlmbl twnmbl) keep(mobile fmlmbl twnmbl schmbl rngprmbl) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles("Income diversification" "Income diversification" "Income diversification" "Income diversification") star(* 0.10 ** 0.05 *** 0.01)

esttab  using $table\hetero_full.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2015, 2018/19.) order(mobile schmbl rngprmbl fmlmbl twnmbl) keep(mobile fmlmbl twnmbl schmbl rngprmbl female age_hh hh_size schll_hh farmsize lvstck town) s(FE year control N, label("Individual FE" "Year dummy" "Control variables" "Observations")) mtitles("Income diversification" "Income diversification" "Income diversification" "Income diversification") star(* 0.10 ** 0.05 *** 0.01)

*Robustness check
drop if newvar==1
teffects ipwra (inc_div $control )(mobile $control )  if est==1


twoway (kdensity _pscore if mobile==1, color(emerald))(kdensity _pscore if mobile==0), ytitle(Density) xtitle(Propensity score) legend(label(1 "Ownership") label(2 "Non-ownership")) saving(pre, replace) scheme(s1mono) 

twoway (kdensity _pscore if mobile==1 & _support==1, color(emerald))(kdensity _pscore if mobile==0 & _support==1), ytitle(Density) xtitle(Propensity score) legend(label(1 "Ownership") label(2 "Non-ownership")) saving(post, replace) scheme(s1mono) 


graph combine pre.gph post.gph, scheme(s1mono)
graph export $graph/psm_density.jpg, replace
**robust DR
eststo clear
eststo: teffects ipwra (inc_div $control )(mobile $control )  if est==1
eststo: teffects ipwra (lnfrmslf $control )(mobile $control )  if est==1
eststo: teffects ipwra (lnfrmwg $control )(mobile $control )  if est==1
eststo: teffects ipwra (lnnnslf $control )(mobile $control )  if est==1
eststo: teffects ipwra (lnnnwg $control )(mobile $control )  if est==1
eststo: teffects ipwra (lnnnearn $control )(mobile $control )  if est==1

eststo: teffects ipwra (povertyhc $control )(mobile $control )  if est==1
eststo: teffects ipwra (povgap $control )(mobile $control )  if est==1
teffects ipwra (mpi $control )(mobile $control )  if est==1, osample(mpi_violation)
eststo: teffects ipwra (mpi $control )(mobile $control )  if est==1 
esttab using $table\robust_1.rtf, b(%4.3f) se replace nogaps starlevels(* 0.1 ** 0.05 *** 0.01) label nocons 

**robust PSM-DID
drop if year2012==1 & mobile==1
psmatch2 mobile female age_hh hh_size schll_hh farmsize lvstck d_town_m i.dvcode if year2018==1, out(povertyhc) com cal(0.05)
drop if _support==0 

eststo clear   
eststo:reghdfe inc_div mobile#year2018 $control ,a(a01)  vce(robust)
eststo:reghdfe lnfrmslf mobile#year2018 $control ,a(a01)  vce(robust)
eststo:reghdfe lnfrmwg mobile#year2018 $control ,a(a01)  vce(robust)
eststo:reghdfe lnnnslf mobile#year2018 $control ,a(a01)  vce(robust)
eststo:reghdfe lnnnwg mobile#year2018 $control ,a(a01)  vce(robust)
eststo:reghdfe lnnnearn mobile#year2018 $control ,a(a01)  vce(robust)

eststo:reghdfe povertyhc mobile#year2018 $control ,a(a01) vce(robust)
eststo:reghdfe povgap mobile#year2018 $control, a(a01) vce(robust)
eststo:reghdfe mpi mobile#year2018 $control ,a(a01) vce(robust)

esttab  using $table\robust_2.rtf, b(%4.3f) se replace label nodepvar nogaps addnote(Source: Bangladesh Integrated Household Survey 2011/12, 2018/19.) mtitles("Income diversification" "Farm self" "Farm wage" "Off-farm self" "Off-farm wage" "Non-earnd" "Poverty Headcount" "Depth of poverty" "MPI score") star(* 0.10 ** 0.05 *** 0.01)
