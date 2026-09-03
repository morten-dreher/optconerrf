# optconerrf 1.0.3

* Added an additional check if the specified constraints (maximumConditionalError, minimumConditionalError,
minimumSecondStageInformation, maximumSecondStageInformation) are too strict.
* Added a message to the user in case that the user specifies an upper constraint to the conditional error 
function that is or could be less strict than the restriction imposed by the conditionalPower resp. the conditionalPowerFunction.
* Added the internal functions `getConstraintC_min` and `getConstraintC_max` to calculate constraints.
* Corrected the calculation of `getPsi()` in the special case that additional constraints are applied and the conditional power
is not within the bounds `pnorm(-2)` and `pnorm(2)`.
* Updated the documentation regarding the conditional power function. If a conditional power function
is used the calculated conditional error function is not necessarily the optimal conditional error function.

# optconerrf 1.0.2

Improved unit test coverage

# optconerrf 1.0.1

Corrected some bugs that led to incorrect (expected) second-stage informations for interim estimates: 

* Fixed a bug in the internal function `integrateExpectedInformation()` that led to incorrect scaling of the calculated interim estimates.
* Added the required `sqrt()` function to the first-stage information in `getSecondStageInformation()` when using interim estimates.

# optconerrf 1.0.0

* Initial CRAN submission.


