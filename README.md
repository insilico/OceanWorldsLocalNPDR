# OceanWorldsLocalNPDR
Support for submitted paper: 

## IRMS plus time-series features Biosignature Data and Simulated Data
`data/`

## Analysis Scripts

Analysis for real biosignature data: `bio_local_npdr.R.`
<p>
Analysis for simulated data: `sim_local_npdr.R.`
<p>
Scripts need functions sourced from `local_npdr_functions.R.` These will be added to npdr library.

## Installation
To use NPDR, install the R library as follows. Check other standard dependencies. 

``` r
# install.packages("devtools") # uncomment to install devtools
library(devtools)
devtools::install_github("insilico/npdr")
library(npdr)
```

Other possible dependencies for running analysis script:

``` r
install.packages(c("ranger", "reshape2", "dplyr", "caret", "glmnet"))
install.packages(c("speedglm", "wordspace", "doParallel", "foreach"))
```

## Authors
Lily Clough and Brett McKinney

## Contact

[brett.mckinney@gmail.com](brett.mckinney@gmail.com)

## Websites

-   [insilico Github Organization](https://github.com/insilico)

-   [insilico McKinney Lab](https://brett-mckinney.github.io/)

## Related references

-   [NPDR github](https://insilico.github.io/npdr/)

