des
summ

duplicates report ID
duplicates list ID

misstable summarize SEX AGE CASecondary_DATE ward doctorexp urban 

*****Create date and outcome variables*****

gen CASecondary_DATE_num = daily(CASecondary_DATE, "DMY")
format CASecondary_DATE_num %td

gen Counselling_DATE_num = daily(Counselling_DATE, "DMY")
format Counselling_DATE_num %td

gen diff_days = Counselling_DATE_num - CASecondary_DATE_num


***early_718 ; include prior PC
gen early_718 = .
replace early_718 = 1 if diff_days <= 56
replace early_718 = 0 if diff_days > 56

**identify prior palliative care before metastatic diagnosis
gen prior_pc = .
replace prior_pc = 1 if diff_days < 0
replace prior_pc = 0 if diff_days >= 0
replace prior_pc = 0 if missing(Counselling_DATE_num)


*****Create covariates*****
gen catype = .
replace catype = 1 if ca_lung   == 1
replace catype = 2 if ca_gyne   == 1
replace catype = 3 if ca_gi1    == 1
replace catype = 4 if ca_hepato == 1
replace catype = 5 if ca_breast == 1
replace catype = 6 if missing(catype)
label define catype 1 "lung" 2 "gyne" 3 "gi" 4 "hepato" 5 "breast" 6 "other"
label values catype catype
tab catype

gen ward_md_sx = .
replace ward_md_sx = 1 if ward == 1
replace ward_md_sx = 2 if ward == 2
replace ward_md_sx = 3 if ward != 1 & ward != 2
label define wardlbl 1 "Med" 2 "Sx" 3 "other"
label values ward_md_sx wardlbl
tab ward_md_sx

gen exp5 = .
replace exp5 = 1 if doctorexp >= 5  & doctorexp <= 9
replace exp5 = 2 if doctorexp >= 10 & doctorexp <= 14
replace exp5 = 3 if doctorexp >= 15 & doctorexp <= 19
replace exp5 = 4 if doctorexp >= 20 & doctorexp <= 24
replace exp5 = 5 if doctorexp >= 25
label define exp5lbl 1 "5–9" 2 "10–14" 3 "15–19" 4 "20–24" 5 "≥25"
label values exp5 exp5lbl
tab exp5

gen exp3 = .
replace exp3 = 1 if doctorexp >= 5  & doctorexp <= 14
replace exp3 = 2 if doctorexp >= 15 & doctorexp <= 24
replace exp3 = 3 if doctorexp >= 25
label define exp3lbl 1 "5–14" 2 "15–24" 3 "≥25"
label values exp3 exp3lbl
tab exp3

gen age65 = .
replace age65 = 0 if AGE < 65
replace age65 = 1 if AGE >= 65
label define age65lab 0 "<65" 1 ">=65"
label values age65 age65lab
tab age65

**Combine gyne into other
gen catype2 = catype
replace catype2 = 6 if catype == 2
label define catype2lbl 1 "lung" 3 "gi" 4 "hepato" 5 "breast" 6 "other"
label values catype2 catype2lbl
tab catype2

**Check doctor
egen tag = tag( DR_CODE )
tab exp5 if tag
tab exp3 if tag
bysort DR_CODE: gen n_patients = _N
summ n_patients
tab n_patients

summ n_patient if tag, detail

**Check missingness in analytic cohort
misstable summarize SEX AGE exp3 catype2 ward_md_sx urban early_718

*****Save prior-palliative-care cases for description*****
preserve
keep if diff_days < 0
save neg_interval_cases.dta, replace
restore


*****MAIN ANALYSIS DATASET*****

*****Table 1*****
destring SEX, replace

tab SEX early_718, row
tabstat AGE, by(early_718) stat(p50 p25 p75)
tab catype2 early_718, row
tab urban early_718, row
tab ward_md_sx early_718, row
tab exp3 early_718, row


*****Model 1: Unadjusted*****
poisson early_718 i.SEX, irr vce(cluster DR_CODE)
poisson early_718 c.AGE, irr vce(cluster DR_CODE)
poisson early_718 i.exp3, irr vce(cluster DR_CODE)
poisson early_718 ib1.catype2, irr vce(cluster DR_CODE)
poisson early_718 i.ward_md_sx, irr vce(cluster DR_CODE)
poisson early_718 i.urban, irr vce(cluster DR_CODE)


*****Model 2: Minimally adjusted*****
poisson early_718 i.SEX c.AGE, irr vce(cluster DR_CODE)
poisson early_718 c.AGE i.SEX, irr vce(cluster DR_CODE)
poisson early_718 i.exp3 c.AGE i.SEX, irr vce(cluster DR_CODE)
poisson early_718 ib1.catype2 c.AGE i.SEX, irr vce(cluster DR_CODE)  
poisson early_718 i.ward_md_sx c.AGE i.SEX, irr vce(cluster DR_CODE)
poisson early_718 i.urban c.AGE i.SEX, irr vce(cluster DR_CODE)


*****Model 3: Fully adjusted*****
poisson early_718 i.SEX c.AGE i.exp3 ib1.catype2 i.ward_md_sx i.urban, irr vce(cluster DR_CODE)


***** Predicted probabilities *****
margins SEX, saving(m_sex, replace)
margins exp3, saving(m_exp3, replace)
margins catype2, saving(m_catype2, replace)
margins urban, saving(m_urban, replace)
margins ward_md_sx, saving(m_ward, replace)

****Margins for SEX****
preserve
use m_sex, clear
gen group = "Sex"
gen item = ""
replace item = "Male" if _m1==1
replace item = "Female" if _m1==2
keep _margin _ci_lb _ci_ub group item
replace _margin=_margin*100
replace _ci_lb=_ci_lb*100
replace _ci_ub=_ci_ub*100
save m_sex_clean, replace
restore

****Margins for doctor experience****
preserve
use m_exp3, clear
gen group = "Doctor experience"
gen item = ""
replace item = "5–14 years" if _m1==1
replace item = "15–24 years" if _m1==2
replace item = "≥25 years" if _m1==3
keep _margin _ci_lb _ci_ub group item
replace _margin=_margin*100
replace _ci_lb=_ci_lb*100
replace _ci_ub=_ci_ub*100
save m_exp3_clean, replace
restore

****Margins for primary cancer site****
preserve
use m_catype2, clear
gen group = "Primary cancer site"
gen item = ""
replace item = "Lung" if _m1==1
replace item = "GI" if _m1==3
replace item = "Hepato-biliary" if _m1==4
replace item = "Breast" if _m1==5
replace item = "Other" if _m1==6
keep _margin _ci_lb _ci_ub group item
replace _margin=_margin*100
replace _ci_lb=_ci_lb*100
replace _ci_ub=_ci_ub*100
save m_catype2_clean, replace
restore

****Margins for place of residence****
preserve
use m_urban, clear
gen group = "Place of residence"
gen item = ""
replace item = "Rural" if _m1==0
replace item = "Urban" if _m1==1
keep _margin _ci_lb _ci_ub group item
replace _margin=_margin*100
replace _ci_lb=_ci_lb*100
replace _ci_ub=_ci_ub*100
save m_urban_clean, replace
restore

****Margins for department of care****
preserve
use m_ward, clear
gen group = "Department of care"
gen item = ""
replace item = "Medicine" if _m1==1
replace item = "Surgery" if _m1==2
replace item = "Other" if _m1==3
keep _margin _ci_lb _ci_ub group item
replace _margin=_margin*100
replace _ci_lb=_ci_lb*100
replace _ci_ub=_ci_ub*100
save m_ward_clean, replace
restore

***** Table + graph *****
use m_sex_clean, clear
append using m_exp3_clean
append using m_catype2_clean
append using m_urban_clean
append using m_ward_clean

rename _margin margin
rename _ci_lb lb
rename _ci_ub ub

gen labeltext = group + ": " + item

gen order = .
replace order = 1 if group=="Sex" & item=="Male"
replace order = 2 if group=="Sex" & item=="Female"

replace order = 3 if group=="Doctor experience" & item=="5–14 years"
replace order = 4 if group=="Doctor experience" & item=="15–24 years"
replace order = 5 if group=="Doctor experience" & item=="≥25 years"

replace order = 6 if group=="Primary cancer site" & item=="Lung"
replace order = 7 if group=="Primary cancer site" & item=="GI"
replace order = 8 if group=="Primary cancer site" & item=="Hepato-biliary"
replace order = 9 if group=="Primary cancer site" & item=="Breast"
replace order = 10 if group=="Primary cancer site" & item=="Other"

replace order = 11 if group=="Place of residence" & item=="Rural"
replace order = 12 if group=="Place of residence" & item=="Urban"

replace order = 13 if group=="Department of care" & item=="Medicine"
replace order = 14 if group=="Department of care" & item=="Surgery"
replace order = 15 if group=="Department of care" & item=="Other"

sort order
gen y = _N - _n + 1

capture label drop ylabs
forvalues i = 1/`=_N' {
    local ypos = y[`i']
    local lab = labeltext[`i']
    label define ylabs `ypos' "`lab'", add
}
label values y ylabs

twoway ///
(rcap lb ub y, horizontal) ///
(scatter y margin, msymbol(O) msize(medium)), ///
ylab(1(1)`=_N', valuelabel angle(0) labsize(small)) ///
ytitle("") ///
xtitle("Adjusted predicted probability (%)") ///
xlabel(0(5)35) ///
legend(off)

graph export "forest_plot.png", replace width(2000)


***************************************************************


****Interaction analysis: age group65****
*Age × Residence
poisson early_718 i.age65##i.urban i.SEX c.AGE i.exp3 ib1.catype2 i.ward_md_sx, irr vce(cluster DR_CODE)
testparm i.age65#i.urban

*Age × Doctor experience
poisson early_718 i.age65##i.exp3 i.SEX c.AGE i.urban ib1.catype2 i.ward_md_sx, irr vce(cluster DR_CODE)
testparm i.age65#i.exp3

*Age × Department of care
poisson early_718 i.age65##i.ward_md_sx i.SEX c.AGE i.exp3 i.urban ib1.catype2, irr vce(cluster DR_CODE)
testparm i.age65#i.ward_md_sx

*Age × Cancer site
poisson early_718 i.age65##ib1.catype2 i.SEX c.AGE i.exp3 i.urban i.ward_md_sx, irr vce(cluster DR_CODE)
testparm i.age65#i.catype2
***************************************************************

*****SENSITIVITY ANALYSIS****
**Excluding patients with palliative care prior to metastatic diagnosis
poisson early_718 i.SEX c.AGE i.exp3 ib1.catype2 i.ward_md_sx i.urban if prior_pc == 0, irr vce(cluster DR_CODE)


**Excluding the highest-volume physician
poisson early_718 i.SEX c.AGE i.exp3 ib1.catype2 i.ward_md_sx i.urban if DR_CODE != "42260", irr vce(cluster DR_CODE)

