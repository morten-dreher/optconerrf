# optconerrf 1.0.1

Corrected some bugs that led to incorrect (expected) second-stage informations for interim estimates: 

* Fixed a bug in the internal function `integrateExpectedInformation()` that led to incorrect scaling of the calculated interim estimates.
* Added the required `sqrt()` function to the first-stage information in `getSecondStageInformation()` when using interim estimates.

# optconerrf 1.0.0

* Initial CRAN submission.


