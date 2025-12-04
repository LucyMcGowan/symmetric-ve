# SVE

Lucy D'Agostino McGowan

This repository contains the code and data necessary to reproduce the analyses, figures, and tables from the manuscript [Symmetric Vaccine Efficacy]().

## Reproducing the results

The functions needed to calculate SVE, its variance, and confidence intervals can be found in `0-functions.R`.

```r
source("0-functions.R")
```

The simulations can be replicated by running `1-run-simulations.R`. The resulting simulation results are stored in `simulation_coverage.Rda`. 

```r
source("1-run-simulations.R") # run the simulations
```

Then the results for the three confidence interval types needs to be run:

```r
source("2-calculate-results.R") # run the simulations
```

### Generate Figures

Each figure can be reproduced by running the corresponding script:

```r
source("figure-1.R")  # Figure 1
source("figure-2.R")  # Figure 2
source("figure-3.R")  # Figure 3
source("figure-s1-s2.R")  # Figure S1 & S2
source("figure-s3.R")  # Figure S3
source("figure-s4.R")  # Figure S4
source("table-s1.R")  # Table S1

```

## Real Data Example

The data from the HIV trial used in the paper can be found in the `data-example.R` file. This file also includes code to replicate Table S2 and Figure 4.

```r
source("data-example.R") # Table S2, Figure 4
```
