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

**Main outcome: early palliative care within 8 weeks after metastatic diagnosis
gen early_718 = (diff_days <= 56 & diff_days >= 0)
tab early_718, missing

**identify prior palliative care before metastatic diagnosis
gen prior_pc = (diff_days < 0) if !missing(diff_days)
tab prior_pc, missing


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

destring SEX, replace

gen age65 = .
replace age65 = 0 if AGE < 65
replace age65 = 1 if AGE >= 65
label define age65lab 0 "<65" 1 ">=65"
label values age65 age65lab
tab age65

gen catype2 = catype
replace catype2 = 6 if catype == 2
label define catype2lbl 1 "lung" 3 "gi" 4 "hepato" 5 "breast" 6 "other"
label values catype2 catype2lbl
tab catype2

**Check missingness in analytic cohort
misstable summarize SEX AGE exp5 catype2 ward_md_sx urban early_718

*****Save prior-palliative-care cases for description*****
preserve
keep if diff_days < 0
save neg_interval_cases.dta, replace
restore


*****MAIN ANALYSIS DATASET*****
**Exclude prior palliative care before metastatic diagnosis

drop if diff_days < 0

*****Descriptive analysis*****
tab SEX early_718, row
tabstat AGE, by(early_718) stat(p50 p25 p75)
tab catype2 early_718, row
tab urban early_718, row
tab ward_md_sx early_718, row
tab exp5 early_718, row


*****Model 1: Unadjusted*****
poisson early_718 i.SEX, irr vce(robust)
poisson early_718 c.AGE, irr vce(robust)
poisson early_718 i.exp5, irr vce(robust)
poisson early_718 ib3.catype2, irr vce(robust)
poisson early_718 i.ward_md_sx, irr vce(robust)
poisson early_718 i.urban, irr vce(robust)

*****Model 2: Minimally adjusted*****
poisson early_718 i.SEX c.AGE, irr vce(robust)

*****Model 3: Fully adjusted*****
poisson early_718 i.SEX c.AGE i.exp5 ib3.catype2 i.ward_md_sx i.urban, irr vce(robust)

*****Predicted probabilities (margins)*****
margins i.SEX i.exp5 i.catype2 i.urban i.ward_md_sx


****Interaction analysis: age group65****
poisson early_718 i.age65##i.urban i.SEX c.AGE i.exp5 ib3.catype2 i.ward_md_sx, irr vce(robust)
testparm i.age65#i.urban

poisson early_718 i.age65##i.exp5 i.SEX c.AGE i.urban ib3.catype2 i.ward_md_sx, irr vce(robust)
testparm i.age65#i.exp5

poisson early_718 i.age65##i.ward_md_sx i.SEX c.AGE i.exp5 i.urban ib3.catype2, irr vce(robust)
testparm i.age65#i.ward_md_sx

poisson early_718 i.age65##ib3.catype2 i.SEX c.AGE i.exp5 i.urban i.ward_md_sx, irr vce(robust)
testparm i.age65#i.catype2


*****SENSITIVITY ANALYSIS****
gen early_sens = early_718
replace early_sens = 1 if diff_days < 0

tab early_718
tab early_sens
tab early_718 early_sens

poisson early_sens i.SEX c.AGE i.exp5 ib3.catype2 i.ward_md_sx i.urban, irr vce(robust)