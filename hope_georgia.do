***HOPE GEORGIA DATA

import excel "D:\RepTemplates\Applied_econometrics_2022\HOPE.xlsx", sheet("Sheet1") firstrow
reg InCollege Georgia After AfterGeorgia
reg InCollege Georgia After AfterGeorgia, r
display _b[_cons]
display _b[_cons]+_b[Georgia]
display _b[_cons]  +_b[After]
display _b[_cons]+_b[Georgia]   +_b[After] +_b[AfterGeorgia]
reg InCollege AfterGeorgia Georgia      Age18   Black i.StateCode i.Year
reg InCollege AfterGeorgia              i.StateCode i.Year
reg InCollege AfterGeorgia Georgia if Black==0
twoway (line InCollege Year if Georgia)
twoway (line InCollege Year if Georgia) (scatter InCollege Georgia if Georgia)
