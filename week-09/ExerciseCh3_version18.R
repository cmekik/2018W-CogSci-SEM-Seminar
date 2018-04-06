library(devtools)
install.packages("lavaan")
###
library(lavaan)

###NOTE, In order for this code to work you MUST have already loaded performance.data into your environment (and subset it to the first 12 columns)


###Original (book) version of Four Factor Model
achievement.goal.cfa4.model<-'map=~ags1+ags5+ags7
mav=~ags2+ags6+ags12
pap=~ags3+ags9+ags11
pav=~ags4+ags8+ags10
'
###Model Fit for Original (book) version of Four Factor Model
### Note that the only change is to set std.lv to True.
###     From lavaans documentation:
###             If TRUE, the metric of each latent variable is determined by fixing their (residual) variances
###             to 1.0. If FALSE, the metric of each latent variable is determined by fixing the factor loading
###             of the first indicator to 1.0.
### According to some guy (Ed Rigdon):
###      If you standardize the factors, you can reduce the dimensionality by forcing covariances to 1.  That will make the nesting clear.
achievement.goal.cfa4.dwls.fit<-cfa(achievement.goal.cfa4.model, data=performance.data, estimator="WLSMV", std.lv=T)


###Original (book) version of Three Factor Model
achievement.goal.cfa3.model<-'mastery=~ags1+ags5+ags7+ags2+ags6+ags12
pap=~ags3+ags9+ags11
pav=~ags4+ags8+ags10
'

###Adjusted version of Three Factor Model 

##this starts with the same base model as the four factor model.
##    to pare this down to three factors we need to force the two elements of `mastery' to covary.
##    these two aspects are `mav' and `map'
##The first step is to fix the covariance between mav<->map 
##    to do this we use this line:
##        mav~~1*map
##    this creates a de facto mastery variable
##The second step is to fix the covariance from our new pseudo `mastery' variable to all the other variables
##    this replicates the free parameters from the original 3 factor model:
##        mastery~~pap
##        mastery~~pav
##    to do this we fix the covariance of mav<->pap and mav<->pav to two free parameters--a and b
##        mav~~a*pap + b*pav
##    then we fix the covariance of map<->pap and map<->pav to the SAME free parameters--a and b
##        map~~a*pap + b*pav
##    together, the previous two lines force mav<->pap and map<->pap to covary with pap the same and
##    we also force map<->pav and mav<->pav to covary the same.
##    Additionally, these two lines can actually be reduced down to a single line of:
##        map+mav~~a*pap + b*pav
##    The `+' operator when on the left side tells lavaan to `do the RHS for both of these variables'

achievement.goal.cfa3.model_new<-'map=~ags1+ags5+ags7
mav=~ags2+ags6+ags12
pap=~ags3+ags9+ags11
pav=~ags4+ags8+ags10
mav~~1*map
mav~~a*pap + b*pav
map~~a*pap + b*pav
'

###Model Fit for Original (book) version of Three Factor Model
achievement.goal.cfa3.dwls.fit_old<-cfa(achievement.goal.cfa3.model, data=performance.data, estimator="WLSMV",std.lv=T)
###Model Fit fo adjusted version of Three Factor Model 
achievement.goal.cfa3.dwls.fit<-cfa(achievement.goal.cfa3.model_new, data=performance.data, estimator="WLSMV",std.lv=T)


### Running the next two lines of code will show you the basic stats of the old and new versions  
### you can see for yourself that they're the same model despite different structure calls
achievement.goal.cfa3.dwls.fit_old
achievement.goal.cfa3.dwls.fit

### This shows you the differences in paramaters
### parameters for Book 3-Factor
parTable(achievement.goal.cfa3.dwls.fit_old)
### parameters for Adjusted 3-Factor
parTable(achievement.goal.cfa3.dwls.fit)
### parameters for Book 4-Factor
parTable(achievement.goal.cfa4.dwls.fit)
## recall the goal is for free parameters in Adjusted 3-Factor to match free parameters for Book 4-Factor







###Original (book) version of Two Factor Model
achievement.goal.cfa2.model<-'mastery=~ags1+ags2+ags5+ags6+ags7+ags12
performance=~ags3+ags4+ags8+ags9+ags10+ags11
'

###Adjusted version of Two Factor Model 

##Again, this starts with the same base model as the four factor model.
##    to pare this down to two factors we need to force the two elements of `mastery' to covary.
##    these two elements are `mav' and `map'
##    Additionally, we need to force the two elements of `performance' to covary.
##    these two elements are `pav' and `pap'
##The first step (again) is to fix the covariance between mav<->map 
##    to do this we use this line:
##        mav~~1*map
##    this creates a de facto mastery variable
##The second step, similarly is to fix the covariance between pav<->pap 
##    to do this we use this line:
##        pap~~1*pav
##    this creates a de facto performance variable
##Note that for the previous to lines the order doesn't matter, nor does the direction
##    You could just as well do:
##        pav~~1*pap
##        map~~1*mav
##The third step is to fix the covariance from our new pseudo `mastery' variable to all the other variables
##    this replicates the free parameter from the original 2 factor model:
##        mastery~~performance
##    to do this we fix the covariance of mav<->pap and mav<->pav to one free parameter--a
##        mav~~a*pap + a*pav
##
##    the reason we only use one free parameter (instead of two) is that we now want pav<->pap to be treated 
##    as a single variable the same way that mav<->map is treated as a single variable 
##
##    so, next we fix the covariance of map<->pap and map<->pav to the SAME free parameter--a
##        map~~a*pap + a*pav
##    together, the previous two lines force mav and map to covary with pap and pav the same
##    to explain this more clearly, we could just as easily define this relationship the opposite way by doing:
##        pap~~a*map + a*mav
##        pav~~a*map + a*mav
##    These two lines can actually be reduced down to a single line of:
##        map+mav~~a*pap + a*pav
##    or (to show the opposite):
##        pap+pav~~a*map + a*mav


achievement.goal.cfa2.model_new<-'map=~ags1+ags5+ags7
mav=~ags2+ags6+ags12
pap=~ags3+ags9+ags11
pav=~ags4+ags8+ags10
map~~1*mav
pap~~1*pav
mav~~a*pap + a*pav
map~~a*pap + a*pav
'

###Model Fit for Original (book) version of Two Factor Model
achievement.goal.cfa2.dwls.fit_old<-cfa(achievement.goal.cfa2.model, data=performance.data,estimator="WLSMV",std.lv=T)
###Model Fit fo adjusted version of Two Factor Model
achievement.goal.cfa2.dwls.fit<-cfa(achievement.goal.cfa2.model_new, data=performance.data,estimator="WLSMV",std.lv=T)

### Running the next two lines of code will show you the basic stats of the old and new versions  
### you can see for yourself that they're the same model despite different structure calls
achievement.goal.cfa2.dwls.fit_old
achievement.goal.cfa2.dwls.fit


### This shows you the differences in paramaters
### parameters for Book 2-Factor
parTable(achievement.goal.cfa2.dwls.fit_old)
### parameters for Adjusted 2-Factor
parTable(achievement.goal.cfa2.dwls.fit)
### parameters for Book 4-Factor
parTable(achievement.goal.cfa4.dwls.fit)
## recall the goal is for free parameters in Adjusted 2-Factor to match free parameters for Book 4-Factor



# Example 8
anova(achievement.goal.cfa3.dwls.fit, achievement.goal.cfa2.dwls.fit)

# Example 9
anova(achievement.goal.cfa4.dwls.fit, achievement.goal.cfa2.dwls.fit)

# Example 10
anova(achievement.goal.cfa4.dwls.fit, achievement.goal.cfa3.dwls.fit)


