ssc install diff
use "D:\RepTemplates\Applied_econometrics_2022\DinD_ex.dta", clear
codebook, compact
*****QUESTION 1: Use mean differences to compute the difference in means estimate of the change in minimum wage.***
br
pwd
mat T = J(3,6,.)
drop if after==1
ttest fte, by(nj)

mat T[1,1] = r(mu_1)
mat T[2,1] = r(mu_2)
mat T[3,1] = r(mu_2) - r(mu_1)
ttest fte, by(nj)
mat T[1,2] = r(mu_1)
mat T[2,2] = r(mu_2)
mat T[3,2] = r(mu_2) - r(mu_1)
mat T[1,3] = T[1,1] - T[1,2]
mat T[2,3] = T[2,1] - T[2,2]
mat T[3,3] = T[3,1] - T[3,2]
mat rownames T = PA NJ Difference
frmttable using ttest.doc, statmat(T) varlabels replace ctitle("", before, after, Difference)

diff fte, t(nj) p(after) robust
***Question 2: Estimate the difference-in-difference using a regression model in differences***
use "D:\RepTemplates\Applied_econometrics_2022\DinD_ex.dta", clear
reg dfte njafter nj after
reg dfte njafter nj after, robust
***QUESTION 3: Now estimate the following model in levels, i.e., with the left-hand-side variable in levels.***
reg fte njafter nj after
reg fte njafter nj after, robust
***QUESTION 4: Now estimate the levels model from question 3 but cluster on sheet. How do the standard errors change?***
reg fte njafter nj after, cluster(sheet)
reg fte njafter nj after, cluster(sheet) robust
**** QUESTION 5: Now estimate the levels model using fixed effects (i.e. xtreg). Which variables get dropped and why?
xtreg fte nj njafter after, fe i(sheet)
