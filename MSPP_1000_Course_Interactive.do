//#############################################################################
//MSPP 1000 Statistics Course Example File

//Uses UCLA dataset

//Feel free to utilize any code/functions/logic from this file or your
//recitation files, but perform your own analysis of the results.
//#############################################################################

//Day 1

//ssc install _______
//ssc install logout
//ssc install schemepack  //For better graphs
//set scheme s1color

//Log
capture log close 
log using "MSPP_Day1.log", replace 

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

log close 

//#############################################################################

//DAY 2: PROBABILITY DISTRIBUTIONS & RANDOM VARIABLES
clear all
set more off

//Load main dataset
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear

//Create string variables for labels
decode prog, generate(prog_str)
decode ses, generate(ses_str)
decode schtyp, generate(schtyp_str)

//Log file
capture log close
log using "MSPP_Day2.log", replace text

//Total observations
count
display "Total observations: " r(N)

//Show first few observations
list id female ses prog read write math science in 1/10

//Law of Large Numbers Demonstration (slide 5)
display "As more observations are collected, proportion converges to true probability"

clear
set obs 1000
set seed 12345

//Simulate coin flips
generate flip = runiform() < 0.5
generate trial = _n
generate cum_prob = sum(flip)/trial

//Show convergence
display "Flip 10: Probability = " cum_prob[10]
display "Flip 100: Probability = " cum_prob[100]
display "Flip 1000: Probability = " cum_prob[1000]
display "True probability = 0.50"

//Visualize convergence
twoway (line cum_prob trial), ///
    yline(0.5, lcolor(red) lpattern(dash)) ///
    title("Law of Large Numbers - Coin Flips") ///
    ytitle("Cumulative Proportion of Heads") ///
    xtitle("Number of Trials") ///
    legend(off) ///
    note("Converges to 0.5 as sample size increases")

//Probability Basics (slide 6)
display "Probability always between 0 and 1 (0% to 100%)"

//Load main dataset
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear

//Create string variables for labels
decode prog, generate(prog_str)
decode ses, generate(ses_str)
decode schtyp, generate(schtyp_str)

tab prog
display "P(General) = " 45/200
display "P(Academic) = " 105/200
display "P(Vocation) = " 50/200
display ""
display "These are disjoint - a student can only be in one program"
display "P(General OR Academic) = P(General) + P(Academic) = " ///slide 10
    (45/200 + 105/200)

//Addition Rule
display ""
display "Addition Rule"
display "P(A OR B) = P(A) + P(B) - P(A AND B)"

//Create contingency table for demonstration
tab prog female, row
display "P(General OR Female) = " (45/200 + 109/200 - 24/200)

//Conditional probability examples (slide 13)
tab prog ses, row
display "P(High SES | General) = " 9/45
display "P(High SES | Academic) = " 42/105
display "P(High SES | Vocation) = " 7/50

//Discrete and Continuous Random Variables (slide 15)
//Discrete
display "Countable, individualized, nondivisible values"

tab awards
display "AWARDS: Limited possible values (0 to 7)"

//Visualize discrete vs continuous
hist awards, discrete title("Discrete: Awards") ///
    xtitle("Awards") freq

//Continuious
sum read, detail
display "READ: Any value between " r(min) " and " r(max)
sum write, detail
display "WRITE: Any value between " r(min) " and " r(max)

hist read, normal title("Continuous: Reading Scores") ///
    xtitle("Score (any value in range)") freq

//Bins and Histograms (slide 16-17)	

hist read, bin(10) frequency title("Reading Scores - 10 Bins") ///
    xtitle("Reading Score")

hist read, bin(15) normal title("Reading Scores - With Normal Curve") ///
    xtitle("Reading Score") freq

hist read, bin(30) normal title("Reading Scores - With Normal Curve") ///
    xtitle("Reading Score") freq

//Expected value (slide 18)
sum read
display "E[READ] = Mean = " r(mean)
display ""

//Deviations and standed deviations (slide 19)
generate read_dev = read - r(mean)
list id read read_dev in 1/10
sum read_dev
display "Sum of deviations = " r(sum) " (should be close to 0)"

//Review data
br

//Standard deviation
display "SD(READ) = " r(sd)

//Normal distribution (slide 28)
hist read, normal title("Normal Distribution of Reading Scores") ///
    xtitle("Reading Score") freq

//Z-scores (slide 34)
sum read
generate z_read = (read - r(mean))/r(sd)

count if abs(z_read) <= 1
display "Within 1 SD (68%): " r(N)/_N * 100 "%"

count if abs(z_read) <= 2
display "Within 2 SD (95%): " r(N)/_N * 100 "%"

count if abs(z_read) <= 3
display "Within 3 SD (99.7%): " r(N)/_N * 100 "%"	

log close 

//#############################################################################
//Some extra helpful commands for Stata

//Export summary table to Excel
//Save your current dataset with new variables
save "day2_data.dta", replace

//Or save as Excel for sharing
//Saves in Stata folder
export excel using "day2_data.xlsx", firstrow(variables) replace

//Search for commands
search correlation

//Get help on a specific command
help summarize

//Find examples
help histogram##examples

//Combine graphs into one
hist read, normal title("Normal Distribution of Reading Scores") ///
    xtitle("Reading Score") freq

//Export graph
graph export "day_2_graph.png", replace width(800)

//#############################################################################

//Day 3

clear all
set more off
capture log close

//Set up log file
log using "MSPP_Day3.log", replace text

//Load main dataset
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)
decode ses, generate(ses_str)

//Population values (slide 10)
count
display "Population size = " r(N)

//Population parameters
sum read
display "Population Mean (μ) = " r(mean)
display "Population SD (σ) = " r(sd)

//Random sampling (slide 16)
set seed 12345  //For reproducibility (makes the 'random' sample the same each time)
sample 30, count

display "Sample size = " _N
sum read
display "Sample Mean (x̄) = " r(mean)
display "Sample SD (s) = " r(sd)

//Compare to population
display ""
display "Population Mean = 52.23"
display "Sample Mean = " r(mean)
display "Difference = " r(mean) - 52.23

//Reload Load main dataset
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)
decode ses, generate(ses_str)

//Population values
count
display "Population size = " r(N)

//Review a few sample differences
sample 30, count
sum read

//Sampling distributions, multiple samples (slide 27)
//Create a temporary file to store results
tempname memhold
tempfile results

//Set up the postfile
postfile `memhold' mean using `results', replace

//Loop to take 50 samples
set seed 12345
forvalues i = 1/50 {
    use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
    sample 30, count
    quietly: sum read
    local this_mean = r(mean)
    post `memhold' (`this_mean')
}

//Close the postfile
postclose `memhold'

//Load the results
use `results', clear

//Show the sampling distribution
display ""
display "--- SAMPLING DISTRIBUTION PROPERTIES ---"
sum mean, detail

display "Mean of sample means = " r(mean)
display "Population mean (μ) = 52.23"
display "Difference = " r(mean) - 52.23
display ""

display "Standard deviation of sample means (Standard Error) = " r(sd)
display "Theoretical SE = σ/√n = 10.23/√30 = " 10.23/sqrt(30)
display ""

//Visualize sampling distribution
hist mean, normal title("Sampling Distribution of the Mean (n=30)") ///
    xtitle("Sample Mean") freq

//#################################
//Quick for loop explanation
//Example: Simple loop
forvalues i = 1/5 {
    display "This is iteration number " `i'
}

//How it works:
//- i starts at 1
//- Runs the code inside { }
//- i increases by 1 (i = 2)
//- Repeats until i reaches 1000

//Analogy:
//Like telling someone "Repeat this task 1000 times, and keep count"
//#################################

//Standard error (slide 41)
//Reload the data
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear

//Population review
sum read
local pop_sd = r(sd)
display "Population SD (σ) = " `pop_sd'

//Calculate SE for different sample sizes
//"Sample Size (n)    Standard Error"
foreach n in 10 30 50 100 200 {
    local se = `pop_sd'/sqrt(`n')
    display "   `n'              " round(`se', .2)
}

//Confidence intervals (slide 61)
//Take a sample
set seed 12345
sample 50, count

//Calculate CI
sum read
local mean = r(mean)
local sd = r(sd)
local n = r(N)
local se = `sd'/sqrt(`n')

//95% CI (z = 1.96)
local lower_95 = `mean' - 1.96*`se'
local upper_95 = `mean' + 1.96*`se'

//99% CI (z = 2.576)
local lower_99 = `mean' - 2.576*`se'
local upper_99 = `mean' + 2.576*`se'

display "Sample Mean = " `mean'
display "Standard Error = " `se'
display ""
display "95% Confidence Interval: [" round(`lower_95', .2) ", " round(`upper_95', .2) "]"
display "99% Confidence Interval: [" round(`lower_99', .2) ", " round(`upper_99', .2) "]"

//Simulating confidence intervals (slide 63)
clear all
set more off

//Create the storage dataset first
clear
set obs 25
set seed 12345

//Get population parameters
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
sum read
local pop_mean = r(mean)
local pop_sd = r(sd)

//Switch back to storage dataset
clear
set obs 25
set seed 12345

//Create variables in storage dataset
generate sample_mean = .
generate lower_ci = .
generate upper_ci = .
generate contains_mean = .

//Take 25 samples
forvalues i = 1/25 {
    preserve  //Save the empty storage dataset
    
    use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
    sample 50, count
    
    quietly: sum read
    local mean = r(mean)
    local sd = r(sd)
    local se = `sd'/sqrt(50)
    
    //95% CI
    local lower = `mean' - 1.96*`se'
    local upper = `mean' + 1.96*`se'
    
    restore  //Go back to storage dataset
    
    //Store values
    replace sample_mean = `mean' in `i'
    replace lower_ci = `lower' in `i'
    replace upper_ci = `upper' in `i'
    
    if `lower' <= `pop_mean' & `upper' >= `pop_mean' {
        replace contains_mean = 1 in `i'
    }
    else {
        replace contains_mean = 0 in `i'
    }
}

//Results
sum contains_mean
display ""
display "Intervals containing the population mean: " r(sum) " out of 25"
display "Percentage: " r(sum)/25 * 100 "%"

//Show the intervals
list in 1/25

close log
//#############################################################################

//Day 4

clear all
set more off
capture log close

log using "MSPP_Day4.log", replace text

//=====================================================
//Z-Scores and Hypothesis Testing (slide 25)
//Load the dataset
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)
decode ses, generate(ses_str)

display "Is the mean reading score different from 50?"
//We assume our data is a SAMPLE from a larger population

//Population parameters from our dataset
sum read
local pop_mean = r(mean)
local pop_sd = r(sd)
local n = r(N)

//Display population variables
display "Population mean (μ) = " r(mean)
display "Population SD (σ) = " r(sd)
display "Sample size (n) = " r(N)
display ""

//Test if mean = 50
local hypothesized = 50
local z = (`pop_mean' - `hypothesized') / (`pop_sd'/sqrt(`n'))

//A local macro is a temporary storage container for text or numbers that you can use throughout your code. 

//Set up hypothesis
display "H₀: μ = " `hypothesized'
display "Hₐ: μ ≠ " `hypothesized' " (two-tailed)"
display ""

//Calculate z-score
display "z = (" `pop_mean' " - " `hypothesized' ")/(" `pop_sd' "/√" `n' ") = " `z'

//Critical value for α = 0.05 (two-tailed)
local crit = invnormal(0.975)
display "Critical value (α=0.05) = ±" `crit'

display ""
if abs(`z') > `crit' {
    display "|z| = " abs(`z') " > " `crit' ", REJECT H₀"
    display "The mean reading score IS significantly different from " `hypothesized'
}
else {
    display "|z| = " abs(`z') " < " `crit' ", FAIL TO REJECT H₀"
    display "The mean reading score is NOT significantly different from " `hypothesized'
}

//If / else function. Determining which line to run based on set conditions

//Calculate p-value
local p = 2 * (1 - normal(abs(`z')))
display ""
display "p-value"
display "p = " `p'
if `p' < 0.05 {
    display "p = " `p' " < 0.05, REJECT H₀"
}
else {
    display "p = " `p' " > 0.05, FAIL TO REJECT H₀"
}

//===========================================================
//T-test (slide 33)
//Again we assume our data is a SAMPLE from a larger population
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear

display "Testing if mean math score equals 50"

//One-sample t-test
display "t-test output"
ttest math == 50

if r(p) < 0.05 {
    display "p < 0.05, REJECT H₀"
    display "Math scores ARE significantly different from 50"
}
else {
    display "p > 0.05, FAIL TO REJECT H₀"
    display "Math scores are NOT significantly different from 50"
}

//Visualize
hist math, normal title("Math Score Distribution") ///
    xtitle("Math Score") ///
    xline(50, lcolor(red) lpattern(dash))

//==============================================
//t-test 2 sample (slide 34)

use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)

display "Writing scores by program type"
display "Testing if General vs Academic programs have different mean writing scores"

//Two-sample t-test (only General vs Academic)
ttest write if prog_str == "general" | prog_str == "academic", by(prog_str)

//Interpretation
display ""
display "--- INTERPRETATION ---"
display "t = " r(t)
display "df = " r(df_t)
display "p = " r(p)
display ""

if r(p) < 0.05 {
    display "p < 0.05, REJECT H₀"
    display "General and Academic programs have SIGNIFICANTLY different writing scores"
}
else {
    display "p > 0.05, FAIL TO REJECT H₀"
    display "General and Academic programs do NOT have significantly different writing scores"
}

//Visualize
graph box write if prog_str == "general" | prog_str == "academic", ///
    over(prog_str) title("Writing Scores by Program") ///
    ytitle("Writing Score")

//==============================================
//anova, f-test (slide 38)
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)

display "Reading scores by program type"
display "Testing if reading scores differ across General, Academic, and Vocation programs"

//ANOVA
display ""
display "anova output"
oneway read prog, tabulate

//Interpretation
display ""
display "interpretation"
display "F = " r(F)
display "df = " r(df) ", " r(df_r)
display "p = " r(p)
display ""

if r(p) < 0.05 {
    display "p < 0.05, REJECT H₀"
    display "Not all programs have the same mean reading score"
}
else {
    display "p > 0.05, FAIL TO REJECT H₀"
    display "All programs have the same mean reading score"
}

//Visualize
graph box read, over(prog_str) title("Reading Scores by Program Type") ///
    ytitle("Reading Score")

//==============================================
//chi-squared (slide 46)
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)

display "program type distribution"
display "Testing if students are equally distributed across programs"

//Counts by program
tab prog

//Expected counts (equal distribution)
display ""
display "Expected: 200/3 = 66.67 students per program"

//Calculate chi-square manually
tab prog, matcell(observed)
local total = `r(N)'
local expected = `total'/3

//Calculate chi-square
local chi_sq = 0
forvalues i = 1/3 {
    local obs = observed[`i',1]
    local chi_sq = `chi_sq' + (`obs' - `expected')^2 / `expected'
}

display ""
display "--- CHI-SQUARE CALCULATION ---"
display "χ² = " `chi_sq'

//Critical value (df=2, α=0.05)
local crit = invchi2(2, 0.95)
display "Critical value (df=2, α=0.05) = " `crit'

if `chi_sq' > `crit' {
    display "χ² = " `chi_sq' " > " `crit' ", REJECT H₀"
    display "Students are NOT equally distributed across programs"
}
else {
    display "χ² = " `chi_sq' " < " `crit' ", FAIL TO REJECT H₀"
    display "Students ARE equally distributed across programs"
}

//==============================================
//chi-squared (slide 51)
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)
decode ses, generate(ses_str)

display "Program type and SES"
display "Testing if program type and SES are independent"

//Chi-square test
display ""
display "chi-squared output"
tab prog ses, chi2 row col expected

//Interpretation
display ""
display "interpretation"
display "χ² = " r(chi2)
display "df = " r(df)
display "p = " r(p)
display ""

if r(p) < 0.05 {
    display "p < 0.05, REJECT H₀"
    display "Program type and SES are NOT independent"
    display "There IS a relationship between program and SES"
}
else {
    display "p > 0.05, FAIL TO REJECT H₀"
    display "Program type and SES ARE independent"
    display "There is NO relationship between program and SES"
}

//Visualize
graph hbar (count), over(prog_str) over(ses_str) ///
    title("Program Type by SES") ///
    ytitle("Count") blabel(bar)

//#############################################################################
//Day 5

clear all
set more off
capture log close

log using "MSPP_Day5.log", replace text

//Load the dataset
use https://stats.idre.ucla.edu/stat/data/hsbdemo, clear
decode prog, generate(prog_str)
decode ses, generate(ses_str)

//Basic scatterplot
twoway scatter read write, ///
    title("Reading vs Writing Scores") ///
    xtitle("Writing Score") ///
    ytitle("Reading Score") ///
    mcolor(blue) msymbol(circle)

//Scatterplot with regression line
twoway (scatter read write) (lfit read write), ///
    title("Reading vs Writing Scores with Regression Line") ///
    xtitle("Writing Score") ///
    ytitle("Reading Score") ///
    legend(label(1 "Data Points") label(2 "Regression Line"))

//Multiple scatterplots in one
graph twoway (scatter read write) (lfit read write), ///
    by(prog_str, title("Reading vs Writing by Program Type")) ///
    xtitle("Writing Score") ///
    ytitle("Reading Score")


//#############################################################################
//Extra information, data engineering / cleaning





