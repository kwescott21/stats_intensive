//#############################################################################
//MSPP 1000 Statistics Course Example File

//Uses UCLA dataset

//Feel free to utilize any code/functions/logic from this file or your
//recitation files, but perform your own analysis of the results.
//#############################################################################

//Day 1

//ssc intall _______
//ssc install logout
//ssc install schemepack  //For better graphs
//set scheme s1color

//Log
capture log close 
log using "mylogfile.log", replace 

//Load data from ucla.edu website
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear

//General Stata commands for data exploration
//Browse data
br

//Count data values
count

//Describe data set
describe

//Summarize data set
summarize

//Analyze read variable
codebook read

//Analyze prog variable
codebook prog

//Analyze id variable
codebook id

//Analyze awards variable
codebook awards

//Summarize read variable
sum read

//Summarize read variable in more detail
sum read, detail

//Run statistical variables on read variable
tabstat read, stats(n, mean, median, min, max)

//Creating variables, generating string variable, and filtering data
//Analyzing prog and creating string variable
tab prog
list prog in 1/10
decode prog, generate(prog_str)

//Summarize write when filtered to vocation
sum write if prog_str == "vocation"

//Summarize write when filtered to vocation and ses
decode ses, generate(ses_str)
sum write if prog_str == "vocation" & ses_str == "high"

//Exploring different types of variables with graphics (slide 15)
//Visualize nominal data
graph bar (count), over(prog) title("Nominal: Program Type") ///
    ytitle("Count") blabel(bar)

//SES is ordinal in this dataset
tab ses
codebook ses

//Important: We can rank but CANNOT say gaps are equal
//Create an ordinal variables (low to very high)
generate read_ordinal = .
replace read_ordinal = 1 if read <= 40   //Low
replace read_ordinal = 2 if read > 40 & read <= 50  //Medium
replace read_ordinal = 3 if read > 50 & read <= 60  //High
replace read_ordinal = 4 if read > 60               //Very High

label define read_ord 1 "Low" 2 "Medium" 3 "High" 4 "Very High"
label values read_ordinal read_ord

//Show ordinal characteristics
tab read_ordinal

//Can use median and percentiles (appropriate for ordinal)
sum read_ordinal, detail  //Median makes sense
tabstat read_ordinal, stats(n mean median min max)
//NOTE: Mean is technically inappropriate but shown for comparison

//Visualize ordinal data
graph hbar (count), over(read_ordinal) title("Ordinal: Reading Levels") ///
    ytitle("Count")	

//Interval: read scores (true interval)
histogram read, normal title("INTERVAL: Reading Scores") ///
    xtitle("Score (equal intervals)") frequency	

//Create a Ratio variable from the dataset
generate age_ratio = 15 + runiform()*10  //Simulated age (15-25)

//Compare interval (read) vs ratio (age)
sum read, detail
display "READ (INTERVAL): Mean = " r(mean) ", Min = " r(min)

sum age_ratio, detail
display "AGE (RATIO): Mean = " r(mean) ", Min = " r(min)

//Show the key difference: Zero is meaningful for ratio
display "Age of 0 means no age (true zero)"
display "Reading score of 0 means nothing (arbitrary zero)"

//Show ratios work for age but not reading
display "A 20-year-old is twice as old as a 10-year-old: 20/10 = 2 (VALID)"
display "A 80 reading score is NOT twice as good as 40: 80/40 = 2 (INVALID)"	

//Range = Maximum - Minimum (slide 46)
sum read, detail

//Calculate range manually
display "Range for Reading = " r(max) - r(min)
display "Min = " r(min) ", Max = " r(max)

//Range for all test scores 
foreach var in read write math science {
    quietly: sum `var', detail
    display "`var' Range = " r(max) - r(min) 
    display "  (Min = " r(min) ", Max = " r(max) ")"
}

//IQR for all test scores (slide 47)
foreach var in read write math science {
    quietly: sum `var', detail
    display "`var' IQR = " r(p75) - r(p25) 
    display "  (Q1 = " r(p25) ", Q3 = " r(p75) ")"
}

//Standard deviation (slide 52)
foreach var in read write math science {
    quietly: sum `var'
    display "`var' SD = " r(sd) 
}

//Coefficient of variation (slide 55)
foreach var in read write math science {
    quietly: sum `var'
    local cv = (r(sd)/r(mean))*100
    display "`var': CV = " round(`cv', .1) "%"
}

//Boxplot for all subjects
graph box read write math science, ///
    title("Dispersion Across Subjects") ///
    ytitle("Test Scores") ///
    note("Box = IQR, Line = Median, Whiskers = Range")
//#############################################################################

//Day 2

//Histogram of read
hist read

//Browse data
br

//Histogram for read
hist read if prog_str == "vocation"

//Histogram for read filtered for ses
hist read if ses_str == "high", normal

//Histogram for ses with normal distribution and 25 bin size
hist read if ses_str == "high", bin(25) normal

//Histogram for read with normal distribution and freq
hist read if ses_str == "high", normal freq

//Generate new columns with null values
gen type_name=.

//Set column to 1 for Ozone (03)
replace type_name = 1 if name == "Ozone (O3)" 

//Set column to 0 for all other options
replace type_name = 0 if name != "Ozone (O3)" 

//Browse data after additions
br

//Histogram for new variable being equal to 1
hist datavalue if type_name == 1, normal

//#############################################################################

//Day 3

//Clear all data
clear all

//Set folder location
cd "/Users/keithwescott/Documents/01_Stats_Intensive_2024/Datasets/Course Data"

//Import .csv file
import delimited "Air_Quality.csv", clear

//Run 'Normal histogram'
hist datavalue if name == "Ozone (O3)", normal

//Run histogram with additional filtering
hist datavalue if name == "Ozone (O3)" & geotypename == "Borough" , normal

//Run histograms with even more additional filtering (compare Brooklyn and the Bronx)
hist datavalue if name == "Ozone (O3)" & geotypename == "Borough" & geoplacename == "Brooklyn", normal

hist datavalue if name == "Ozone (O3)" & geotypename == "Borough" & geoplacename == "Bronx", normal

//creating a sample
sample 1000, count

//Browse data after sampling
br

//#############################################################################

//Day 4

//Clear all data
clear all

//Set folder location
cd "/Users/keithwescott/Documents/01_Stats_Intensive_2024/Datasets/Course Data"

//Import .csv file
import delimited "Salary_dataset.csv", clear

//Browse data
br

//Summarize data set
summarize

//histogram of salary
hist salary

//ttest of salary
ttest salary == 80000

//Count data values
count

//Describe data set
describe

//-----------------------------------------------------------------------------

//Clear all data
clear all

//Set folder location
cd "/Users/keithwescott/Documents/01_Stats_Intensive_2024/Datasets/Course Data"

//Import .csv file
import delimited "Air_Quality.csv", clear

//Browse data
br

//histogram
hist datavalue

//ttest
ttest datavalue = 30

//Run 'Normal histogram'
hist datavalue if name == "Ozone (O3)", normal

//ttest
ttest datavalue = 30 if name == "Ozone (O3)"

//#############################################################################

//Day 5

//Clear all data
clear all

//Set folder location
cd "/Users/keithwescott/Documents/01_Stats_Intensive_2024/Datasets/Course Data"

//Import .csv file
import delimited "Salary_dataset.csv", clear

//Browse data
br

//Count data values
count

//Describe data set
describe

//Summarize data set
summarize

//Scatter plots years experience & salary
scatter yearsexperience salary 
scatter yearsexperience salary, title("Years Exp. & Salary")

scatter yearsexperience salary, title("Years Exp. & Salary") ///
subtitle("Sample of 30 workers", size(small)) ///
ytitle("Years Exp.") xtitle("Salary") ///
msymbol(diamond) msize(vsmall) scheme(economist)

//Linear regression
twoway (scatter yearsexperience salary) (lfit yearsexperience salary)

twoway  (scatter yearsexperience salary, mcolor(black) msymbol(circle)) ///
		(lfit yearsexperience salary, lcolor(black) lpattern(solid)) ///
		(qfit yearsexperience salary, lcolor(black) lpattern(dash)), ///
		ytitle(Years of Exp.) ///
		xtitle(Salary) ///
		legend(off) scheme(burd)
		
reg yearsexperience salary		








