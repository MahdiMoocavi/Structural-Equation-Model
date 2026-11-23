# Structural Equation Modelling: Religiosity, Meaning, and Attitudes

An R analysis demonstrating data preparation, confirmatory factor analysis (CFA), structural equation modelling (SEM), mediation models, model comparison, bootstrap-based estimation, and a hold-out model-fit comparison.

## Research focus

The script examines relationships among latent constructs representing religiosity, attitudes, meaning, and confidence in institutions, and explores how religiosity, education, and happiness relate to attitudes toward Muslims in the selected survey dataset.

## Analysis workflow

1. Import the survey data and select variables relevant to the research questions.
2. Recode selected categorical variables and missing-value codes.
3. Prepare scale scores, reverse-coded items, and analysis variables.
4. Inspect descriptive statistics, missingness, distributions, correlations, and multicollinearity.
5. Fit a confirmatory factor analysis model for the latent constructs.
6. Estimate structural models and indirect pathways involving education and happiness.
7. Compare model fit, conduct a sensitivity analysis, and obtain bootstrap parameter estimates.
8. Fit the same prespecified model separately in a training subset and a hold-out subset for a descriptive comparison of model fit.

## Input data

The script expects the SPSS data file ZA7570_v2-1-0.sav in the working directory. The dataset is not included; users must obtain it from its authorized source and comply with its terms of use.

## Requirements

R and the following packages:

```r
install.packages(c(
  "haven", "dplyr", "scales", "psych", "car",
  "corrplot", "lavaan", "semPlot"
))
```

## Run

Place the authorized .sav file in the working directory, install the dependencies, and execute SEM_2023_4.R.

## Outputs

The script prints descriptive and model summaries and generates correlation and SEM path visualizations in the active graphics device.

## Interpretation and limitations

The train/test section is a hold-out comparison of model fit, not a predictive-accuracy benchmark. Model specification, missing-data handling, outlier rules, sample splitting, and fit diagnostics should be reviewed in the context of the research question. The source survey data are required for numerical reproduction.
