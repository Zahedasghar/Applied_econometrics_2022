* Don't forget to install causaldata with ssc install causaldata
* if you haven't yet.
causaldata Mroz.dta, use clear download
* Keep just working women
keep if lfp == 1
* Get unlogged earnings
g earn = exp(lwg)
* Drop negative other earnings
drop if inc < 0

* 1. Draw a scatterplot
twoway scatter inc earn, yscale(log) xscale(log)

* 2. Get the conditional mean college attendance
*table wc, c(mean earn) earlier stata version
tabstat earn, statistics( mean ) by(wc)
* 3. Get the conditional mean by bins
* Create the cut variable with ten groupings
*egen inc_cut = cut(inc), group(10) label
tabstat earn, statistics( mean ) by(inc_cut)

* 4. Draw the LOESS and linear regression curves
* Create the logs manually for the fitted lines
g loginc = log(inc)
twoway scatter loginc lwg || lowess loginc lwg
twoway scatter loginc lwg || lfit loginc lwg

* 5. Run a linear regression, by itself and including controls
reg lwg loginc
reg lwg loginc wc k5