//#############################################################################
//MSPP 1000 Statistics Course Example File

//Uses UCLA dataset

//Feel free to utilize any code/functions/logic from this file or your
//recitation files, but perform your own analysis of the results.
//#############################################################################

//Day 1

//ssc intall _______

//Log
capture log close 
log using "mylogfile.log", replace 

//Load data from ucla.edu website
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear

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

//Analyzing prog and creating string variable
tab prog
list prog in 1/10
decode prog, generate(prog_str)

//Summarize write when filtered to vocation
sum write if prog_str == "vocation"

//Summarize write when filtered to vocation and ses
decode ses, generate(ses_str)
sum write if prog_str == "vocation" & ses_str == "high"

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








