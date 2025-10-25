clear all
set more off

* Set up directories
global research "/Users/carlromer/Documents/"
global projects "${research}UCSD/"
global mindspark "${projects}Computations/"
global el "${mindspark}3_Data_processing/Endline/"
global el_clean "${el}Clean/"
global output "${mindspark}7. Output/Replication Assignment/"

* Create output directory if it doesn't exist
cap mkdir "${output}"

* Load the dataset used for Table 1 (first table in the paper)
use ${el_clean}ms_blel_jpal_long, clear

* Keep only baseline observations (round==1) to match Table 1
keep if round == 1

* Describe the dataset structure
describe
codebook st_id
count

* Pick a continuous variable: Math test IRT-scaled score (MLE)
* This represents students' mathematics achievement at baseline,
* scaled using Item Response Theory Maximum Likelihood Estimation

* Generate histogram
histogram m_theta_mle, ///
    title("Distribution of Baseline Math Test Scores") ///
    xtitle("Math Test Score (IRT-scaled, MLE)") ///
    ytitle("Density") ///
    color(navy) ///
    graphregion(color(white)) ///
    bgcolor(white)

* Save the graph
graph export "${output}math_score_histogram.png", replace

* Summary statistics for the variable
summarize m_theta_mle, detail
