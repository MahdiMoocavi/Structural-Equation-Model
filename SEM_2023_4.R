### The analysis script develops and validates a Structural Equation Model (SEM).
### The purpose of the SEM is to determine the variables that contribute to a negative attitude to muslims.

# Loading the necessary libraries
library(haven)     # To load the .sav file
library(dplyr)     # For data manipulation
library(scales)    # For scaling variables
library(psych)     # For descriptive statistics
library(car)       # For Variance Inflation Factors (VIF)
library(corrplot)  # For correlation plot
library(lavaan)    # For Confirmatory Factor Analysis (CFA) and Structural Equation Modeling (SEM)
library(semPlot) # For plotting the SEM model

# Setting the directory
curdir <- getwd()
setwd(curdir)


##### DATA PROCESSING
### Loading the data
full_df <- haven::read_sav("ZA7570_v2-1-0.sav")

# Creating a new data frame for our variables of interest
df <- subset(full_df, select=c("CASEID","SEX","AGE","EDUCYRS","WORK","v1", # Demographics and Happiness
                               "v7","v8","v9","v10","v11", # Q7: Confidence in Authority
                               "v13","v14","v15", # Q9: Attitude to Science and Religion
                               "v21","v22","v23","v24", # Q15: Religious Beliefs
                               "v27","v29","v30", # Q16: Meaning of Life in Religion
                               "v39","v58")) # Q20b: Person's Religion ;and Q35b: Attitude to Muslims
# Renaming the columns
colnames(df) <- c("Sub", "Sex", "Age", "Edu", "Emp", "Happiness",
                  "ConfParl","ConfBuis", "ConfChur", "ConfCour", "ConfScho",
                  "AttSci", "AttRel", "AttTol",
                  "RlgGod", "RlgAftL", "RlgHeav", "RlgHell",
                  "MeanGodC", "MeanGodE", "MeanLife",
                  "Religion", "Muslims")
# Re-coding NA Values
df <- df %>%
  mutate(across(-c(Sub, Sex, Age, Edu), ~ifelse(. %in% c(8, 9, 0), NA, .)))

# Recode Sex variable
df$Sex <- ifelse(df$Sex == 1, "male", "female")

# Recode Emp variable
df$Emp <- ifelse(df$Emp == 1, "working",
                 ifelse(df$Emp == 2, "worked in the past", "never worked"))

# Recode Religion variable
df$Religion <- ifelse(df$Religion == 1, "catholic",
                      ifelse(df$Religion == 2, "protestant",
                             ifelse(df$Religion == 3, "orthodox",
                                    ifelse(df$Religion == 4, "other christian", NA))))


# Checking for missing data
cat("Missing data (n): ", sum(is.na(df)), "\n")
# Removing rows with NA
df <- na.omit(df)
# Replace Sub with sequence from 1 to n
df$Sub <- seq_len(nrow(df))
cat("Sample Size (after removing missing data): ", nrow(df), "\n")

# Reverse coding for specified variables
reverse_vars <- c("ConfParl", "ConfBuis", "ConfChur", "ConfCour", "ConfScho",
                  "AttRel", "AttTol", "RlgAftL", "RlgHeav", "RlgHell",
                  "MeanGodC", "MeanGodE", "Happiness")

df[, reverse_vars] <- lapply(df[, reverse_vars], function(x) max(x) - x + 1)


### Removing Outliers
# Identifying and removing outliers for v58 (IQR method)
IQR_Muslims <- IQR(df$Muslims)
Q1_Muslims <- quantile(df$Muslims, 0.25)
Q3_Muslims <- quantile(df$Muslims, 0.75)
# Over-write the data frame with no outliers
df <- subset(df, Muslims >= (Q1_Muslims - 1.5*IQR_Muslims) &
               Muslims <= (Q3_Muslims + 1.5*IQR_Muslims))

# Create Q15 variable as a sum of "v27" to "v32" (ONLY TO REMOVE OUTLIERS)
df$RelBel <- rowMeans(df[,c( "RlgGod", "RlgAftL", "RlgHeav", "RlgHell")])
# Identifying and removing outliers for Q15 (IQR method)
IQR_RelBel <- IQR(df$RelBel)
Q1_RelBel <- quantile(df$RelBel, 0.25)
Q3_RelBel <- quantile(df$RelBel, 0.75)
# Over-write the data frame with no outliers
df <- subset(df, RelBel >= (Q1_RelBel - 1.5*IQR_RelBel) &
               RelBel <= (Q3_RelBel + 1.5*IQR_RelBel))

# Data frame after removing outliers for both variables
cat("Sample Size (after removing ourliers): ", nrow(df), "\n")



##### DATA ANALYSIS
### Descriptive Statistics
describe(df)
table(df$Sex)


### Scaling
# Columns to not scale
noscale_cols <- c("Sub","Sex","Emp","Religion")
# Scale other columns
df[, !(names(df) %in% noscale_cols)] <- lapply(df[, !(names(df) %in% noscale_cols)],
                                               function(x) as.vector(scale(x)))


### Assumptions
# Normality: Checking using histograms and QQ-plots
# for RelBel
par(mfrow = c(1, 2))
hist(df$RelBel, main="Figure 4A. Histogram of Religious Beliefs", xlab="Standardized Score")
qqnorm(df$RelBel, main="Figure 4B. QQ-plot of Religious Beliefs")

# for Muslims
par(mfrow = c(1, 2))
hist(df$Muslims, main="Figure 5A. Histogram of Attitude to Muslims", xlab="Standardized Scores")
qqnorm(df$Muslims, main="Figure 5B. QQ-plot of Attitude to Muslims")


### Correlation Matrix
# Select only numeric columns for correlation analysis
numeric_cols <- sapply(df, is.numeric)
corMatrix <- cor(df[, numeric_cols], use = "pairwise.complete.obs")
# Plotting the correlation matrix
par(mfrow = c(1, 1))
corrplot(corr = corMatrix, type = 'upper', tl.col = "black")
title(main = "Figure 3. Correlation Matrix")


### VIF
vif_RelBel <- car::vif(lm(RelBel ~ ., data = df))
print(vif_RelBel)


#### SEM
### CFA
# Specify the CFA model
cfa_model <- '
  # latent variable definitions
  Authority =~ ConfParl + ConfBuis + ConfChur + ConfCour + ConfScho
  Attitude =~ AttSci + AttRel + AttTol
  Religiosity =~ RlgGod + RlgAftL + RlgHeav + RlgHell
  Meaning =~ MeanGodC + MeanGodE + MeanLife
'
# Fit the model
fit <- cfa(cfa_model, data = df)
# Summary of the fit
summary(fit, fit.measures = TRUE)


### MODEL I
# Defining the model
model1 <- '
  # latent variables
    Religiosity =~ RlgGod + RlgAftL + RlgHeav + RlgHell
    Attitude =~ AttSci + AttRel + AttTol
    Meaning  =~ MeanGodC + MeanGodE + MeanLife
    Authority =~ ConfParl + ConfBuis + ConfChur + ConfCour + ConfScho

  # regression paths
    Religiosity ~ Attitude + Meaning + Authority
'
# Fitting the model
fit1 <- sem(model1, data = df)
# Summary of the model
summary(fit1, standardized=TRUE, fit.measures=TRUE)

# Plotting the model with improved aesthetics and label offsets
semPaths(fit1, whatLabels = "std ", style = "lisrel",
         layout = "tree2", edge.label.cex = .8, curvePivot = TRUE,
         rotation = 2,nCharNodes=20,optimizeLatRes=TRUE, levels= c(1,8,15,30),
         )
# Add title to the plot
title(main = "Figure 4. Structural Equation Model I")



# Model II
model2 <- '
  # latent variables
    Religiosity =~ RlgGod + RlgAftL + RlgHeav + RlgHell

  # Education Mediation
  # regression paths
    Muslims ~ e*Religiosity
  # Mediation (Education)
    Edu ~ a*Religiosity
    Muslims ~ b*Edu
    ab := a*b
  # Mediation (Happiness)
    Happiness ~ c*Religiosity
    Muslims ~ d*Happiness
    cd := c*d
    total := e + (a*b) + (c*d)
'

# Fitting the model
fit2 <- sem(model2, data = df)
# Summary of the model
summary(fit2, standardized=TRUE, fit.measures=TRUE)

# Plotting the model
semPaths(fit2, whatLabels = "std ", style = "lisrel",
         layout = "tree2", edge.label.cex = .8, curvePivot = TRUE,
         rotation = 2,nCharNodes=20,optimizeLatRes=TRUE, levels= c(3,7,14,20),
         thresholdSize = 25,sizeMan = 8, sizeLat = 10)
# Add title to the plot
title(main = "Figure 5. Structural Equation Model II")




#### Post-Hoc Analysis
### Model Comparison (with model II)
fitMeasures(fit1, c("logl", "AIC", "BIC", "chisq", "df",
                     "pvalue", "cfi", "tli", "rmsea"),
            output = "matrix")
fitMeasures(fit2, c("logl", "AIC", "BIC", "chisq", "df",
                    "pvalue", "cfi", "tli", "rmsea"),
            output = "matrix")
### Sensitivity Analysis
# Model IV for Sensitivity Analysis (Removed 'Happiness' from Model III)
model3 <- '
  # latent variables
    Religiosity =~ RlgGod + RlgAftL + RlgHeav + RlgHell

  # Education Mediation
  # regression paths
    Muslims ~ e*Religiosity
  # Mediation (Education)
    Edu ~ a*Religiosity
    Muslims ~ b*Edu
    ab := a*b
    total := e + (a*b)
'
# Fitting the model
fit3 <- sem(model3, data = df)

# Model Comparison (Model III and IV)
fitMeasures(fit2, c("logl", "AIC", "BIC", "chisq", "df",
                     "pvalue", "cfi", "tli", "rmsea"),
            output = "matrix")
fitMeasures(fit3, c("logl", "AIC", "BIC", "chisq", "df",
                    "pvalue", "cfi", "tli", "rmsea"),
            output = "matrix")


### Bootstrap Analysis
fit2.bootstrap <- sem(model2, data = df, se = "bootstrap")
# Getting parameter estimates
parameterEstimates(fit2.bootstrap)



### Cross-Validation
# Splitting the data
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
train_df <- df[train_index, ]
test_df <- df[-train_index, ]

# Re-fitting Model III with the training data
fit2_train <- sem(model2, data = train_df)
# Model validation with testing data
fit2_test <- sem(model2, data = test_df,
                 sample.cov = lavInspect(fit3_train, "sampstat"),
                 sample.nobs = nrow(test_df), meanstructure = FALSE)

# Comparing fit measures of train and test data
fitMeasures(fit2_train, c("logl", "AIC", "BIC", "chisq", "df",
                          "pvalue", "cfi", "tli", "rmsea"),
            output = "matrix")

fitMeasures(fit2_test, c("logl", "AIC", "BIC", "chisq", "df",
                          "pvalue", "cfi", "tli", "rmsea"),
            output = "matrix")
