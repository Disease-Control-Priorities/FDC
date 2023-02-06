# FDC

Model inputs and code for "Worldwide health gains from scaling up implementation of fixed-dose combination therapies for primary and secondary prevention of atherosclerotic cardiovascular disease"

*Some inputs required to run the model in its entirety are not hosted online due to large file size. Please contact the corresponding author for access to these files. 

For code generating the demographic projections found in the data_NCDC folder, please reference: https://github.com/Disease-Control-Priorities/NCD-Countdown

For code generating estimates of the transition probabilities found in the data_80_80_80 folder, pleaese reference: https://github.com/Disease-Control-Priorities/HTN

Code and inputs to generate the coverage scale-up CSVs can be found in the coverage_data folder and are generated
by running the FDC PURE data.R script.

The model can be run in the following order:

1.polypill_model_with_aspirin.R
2.polypill_model.R
3.new_figures.R

In the first two scripts (#1&2):

Lines 24-53 are calculing the baseline slopes of the IR and CF transition probabilities
for those age 80+ and applying the effective coverage estimates for those age 55-80. 

The slopes are used later in the model (lines 64-72) to carry on treatment effects for the 80+ age group.
We assume treatment initiation only occurs for those age 55-80, however, some benefits continue 
on past the age of 80 for those intitating treatment in their earlier ages. To simulate this
we apply the baseline slope of the transition probability for those age 80-90 to individuals age 80+ in the model.

The Markov-demographic model can be found in lines 90-130.

The model as a whole is written as a function and is run for each country (in a loop)
in lines 168-177. And takes 30-50 minutes for all 182 countries.

Script #3 (new_figures.R) is to generate additional tables and figures.

