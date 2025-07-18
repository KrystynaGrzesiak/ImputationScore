# Simulations on Imputation Scores

This repository contains the R code used to run the simulations presented in the article *How to rank imputation methods*. The simulations compare different methods for imputing missing data and evaluate their impact on the final analysis results.

## Repository Structure

* R/ – R scripts to run simulations, generate artificial data and analyze results
* data/ – Example datasets used in the experiments
* results/ – Generated results and visualizations
* renv.lock – Dependency management file (for reproducibility with renv)

## Running the Simulations

1. Clone the repository

```bash
git clone https://github.com/KrystynaGrzesiak/ImputationScore
cd ImputationScore
```

2. Open R and restore the R environment (using renv):

```r
renv::restore()
```

For some of the implementations Python is required. To use these functions run first:

```r
renv::use_python()
```

3. Run the targets main simulation script:

```r
source("run.R")
```

## How to cite

Citation coming soon
