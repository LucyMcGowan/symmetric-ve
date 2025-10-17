# SVE

Lucy D'Agostino McGowan

This repository contains the code and data necessary to reproduce the analyses, figures, and tables from the manuscript [Symmetric Vaccine Efficacy]().

## Reproducing the results

The functions needed to calculate SVE, its variance, and confidence intervals can be found in `functions.R`.

```r
source("functions.R")
```

The simulations can be replicated by running `simulations.R`. The resulting simulation results are stored in `simulation_coverage.Rda`. 

```r
source("simulations.R") # run the simulations
```


### Generate Figures

Each figure can be reproduced by running the corresponding script:

```r
source("figure-1.R")  # Figure 1
source("figure-2.R")  # Figure 2
source("figure-3.R")  # Figure 3
source("figure-4.R")  # Figure 4
```

## Real Data Example

The data from the HIV trial used in the paper can be found in the `data-example.R` file. This file also includes code to replicate Table 1 and Figure 5.

```r
source("data-example.R") # Table 1, Figure 5
```
