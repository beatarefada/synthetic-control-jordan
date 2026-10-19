# ------------------------------------------------------------------------------
# 1. Load Required Libraries
# ------------------------------------------------------------------------------
library(readr)
library(Synth)
library(dplyr)
library(zoo)
library(purrr)

# ------------------------------------------------------------------------------
# 2. Import & Filter Data
# ------------------------------------------------------------------------------
# Read local dataset
data <- read_csv("jordan_pisa_data.csv")
data <- as.data.frame(data)

# Filter out unneeded countries
data <- data %>%
  filter(`year` < 2022, `country` != "Tunisia")

# Displaying data
head(data)

# ------------------------------------------------------------------------------
# 3. Handle Missing Data via Interpolation
# ------------------------------------------------------------------------------
# Fill missing values using linear interpolation
data$pisa_math_filled       <- na.approx(data$pisa_math, na.rm = FALSE)
data$pisa_read_filled       <- na.approx(data$pisa_read, na.rm = FALSE)
data$student_teacher_filled <- na.locf(data$student_teacher_ratio, na.rm = FALSE)

summary(data)

# ------------------------------------------------------------------------------
# 4. Calculate Summary Statistics
# ------------------------------------------------------------------------------
# List of variables using clean snake_case names
vars <- c("pisa_math_filled", "pisa_read_filled", "population", "gdp_pc", "political_stability", "student_teacher_filled")

var_names <- c(
  "pisa_math_filled"       = "PISA Math",
  "pisa_read_filled"       = "PISA Reading",
  "population"             = "Population",
  "gdp_pc"                 = "GDP per capita",
  "political_stability"    = "Political Stability",
  "student_teacher_filled" = "Student-Teacher Ratio"
)

# Function to calculate means by year
calculate_stats <- function(var, var_name) {
  data %>%
    group_by(year) %>%
    summarize(mean = mean(.data[[var]], na.rm = TRUE), .groups = 'drop') %>%
    mutate(varname_str = var_name)
}

# Calculate means
stats_list <- map2(vars, var_names[vars], calculate_stats)

# Combine into a single data frame
result <- bind_rows(stats_list)

glimpse(result)

# ------------------------------------------------------------------------------
# 5. Synthetic Control Analysis — Mathematics
# ------------------------------------------------------------------------------
dataprep.math <- dataprep(
  foo = data,
  predictors = c("population", "gdp_pc", "political_stability", "student_teacher_filled"),
  predictors.op = "mean",
  time.predictors.prior = c(2006, 2009),
  special.predictors = list(
    list("pisa_math_filled", 2006, "mean"),
    list("pisa_read_filled", 2006, "mean")
  ),
  dependent = "pisa_math_filled",
  unit.variable = "country_code",
  unit.names.variable = "country",
  time.variable = "year",
  treatment.identifier = 7,
  controls.identifier = c(1:6, 8:13, 15),
  time.optimize.ssr = c(2006, 2009, 2012, 2015, 2018),
  time.plot = c(2006, 2009, 2012, 2015, 2018)
)

# Run synthetic control optimization
synth.math <- synth(data.prep.obj = dataprep.math)

# Output weight and predictor balance tables
synth.tables.math <- synth.tab(dataprep.res = dataprep.math, synth.res = synth.math)
print(synth.tables.math$tab.w[1:13, ])
print(synth.tables.math$tab.pred[1:5, ])

# Path plot
path.plot(
  synth.res = synth.math,
  dataprep.res = dataprep.math,
  Ylab = "PISA - Mathematics",
  Xlab = "Year",
  Ylim = c(370, 410),
  Legend = c("Jordan", "Synthetic Jordan"),
  Legend.position = "bottomright"
)
abline(v = 2011, lty = 2)

# Gaps plot
gaps.plot(
  synth.res = synth.math,
  dataprep.res = dataprep.math,
  Ylab = "Difference in PISA, Jordan vs Synthetic Jordan",
  Xlab = "Year",
  Main = NA
)
abline(v = 2011, lty = 2)

# ------------------------------------------------------------------------------
# Placebo test 1. Donor pool countries versus Jordan
# ------------------------------------------------------------------------------
# Matrix for storing results
store <- matrix(NA, 5, 14)
colnames(store) <- unique(data$country)

# Extract unique Country Codes
country_codes <- unique(data$country_code)

# Filter out Tunisia's Country Code (14)
country_codes <- country_codes[country_codes != 14]

# Run placebo test loop across all donor countries
for (i in seq_along(country_codes)) {
  iter <- country_codes[i]
  
  dataprep.out <- dataprep(
    foo = data,
    predictors = c("population", "gdp_pc", "political_stability", "student_teacher_filled"),
    predictors.op = "mean",
    time.predictors.prior = c(2006, 2009),
    special.predictors = list(
      list("pisa_math_filled", 2006, "mean"),
      list("pisa_read_filled", 2006, "mean")
    ),
    dependent = "pisa_math_filled",
    unit.variable = "country_code",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = iter,
    controls.identifier = country_codes[country_codes != iter],
    time.optimize.ssr = c(2006, 2009, 2012, 2015, 2018),
    time.plot = c(2006, 2009, 2012, 2015, 2018)
  )
  
# Run synth optimization
synth.out <- synth(
  data.prep.obj = dataprep.out,
  method = "BFGS")
  
# Store calculated gaps
store[, i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)}

# Create data matrix for plotting
fig_d <- store
rownames(fig_d) <- c(2006, 2009, 2012, 2015, 2018)

# Set parameters and time window
gap.start   <- 1
gap.end     <- nrow(fig_d)
years       <- c(2006, 2009, 2012, 2015, 2018)
gap.end.pre <- which(rownames(fig_d) == "2009")

# Pre-treatment Mean Squared Prediction Error (MSPE)
mse <- apply(fig_d[gap.start:gap.end.pre, ]^2, 2, mean)
data.mse <- as.numeric(mse["Jordan"])

# Base plot setup
plot(
  years,
  fig_d[, "Jordan"],
  xlab = "Year",
  ylab = "PISA - Mathematics",
  xlim = range(years),
  ylim = c(-50, 50),
  type = "n", # Empty canvas so lines render cleanly
  xaxs = "i",
  yaxs = "i"
)

# Add placebo lines for control countries (gray)
for (i in 1:ncol(fig_d)) {
  lines(years, fig_d[, i], col = "gray70")
}

# Add Jordan line (solid black)
lines(years, fig_d[, "Jordan"], lwd = 2, col = "black")

# Annotations and vertical treatment line (2011)
abline(v = 2011, lty = 2)
legend("bottomright", legend = c("Jordan", "Control regions"), 
       lty = 1, col = c("black", "gray70"), lwd = c(2.5, 1), cex = 0.8)
arrows(2009, -20, 2010.5, -20, col = "black", length = 0.1)
text(2007.5, -20, "Refugee Crisis", cex = 0.8)

# Print gap values matrix
print(fig_d)

# ------------------------------------------------------------------------------
# Placebo test 2. Using Brazil
# ------------------------------------------------------------------------------
#Extracting control identifiers
unique_country_codes <- unique(data$country_code)
exclude_codes <- c(2, 7, 14) # excluding Brazil, Jordan and Tunisia

# Filter out the excluded country codes
control_country_codes <- unique_country_codes[!unique_country_codes %in% exclude_codes]

# Dataprep
dataprep.out.2 <-              dataprep(foo = data,
                                        predictors = c("population", "gdp_pc", "political_stability", "student_teacher_filled") ,
                                        predictors.op = "mean" ,
                                        time.predictors.prior = c(2006, 2009),
                                        special.predictors = list(
                                          list("pisa_math_filled", 2006, 'mean'),
                                          list("pisa_read_filled", 2006, 'mean')),
                                        dependent = "pisa_math_filled",
                                        unit.variable = "country_code",
                                        unit.names.variable = "country",
                                        time.variable = "year",
                                        treatment.identifier = 2, # Change the ID to other unit that did NOT receive the treatment
                                        controls.identifier = control_country_codes,
                                        time.optimize.ssr =c(2006, 2009, 2012, 2015, 2018),
                                        time.plot = c(2006, 2009, 2012, 2015, 2018)
)

# Synth
synth.out.2 <- synth(data.prep.obj = dataprep.out.2,
                     method = "BFGS"
)

# Path Plot
path.plot(
  synth.res = synth.out.2,
  dataprep.res = dataprep.out.2,
  tr.intake = NA,
  Ylab = "PISA Mathematics Scores",
  Xlab = "Year",
  Ylim = c(250, 500),
  Legend = c("Brazil", "synthetic Brazil"),
  Legend.position = "bottomright"
)

# ------------------------------------------------------------------------------
# Robustness - Removing Donor Countries
# ------------------------------------------------------------------------------
# Dataprep
dataprep.out.r <-              dataprep(foo = data,
                                        predictors = c("population", "gdp_pc", "political_stability", "student_teacher_filled"),
                                        predictors.op = "mean",
                                        time.predictors.prior=c(2006, 2009),
                                        special.predictors = list(
                                          list("pisa_math_filled", 2006, 'mean'),
                                          list("pisa_read_filled", 2006, 'mean')),
                                        dependent = "pisa_math_filled",
                                        unit.variable = "country_code",
                                        unit.names.variable = "country",
                                        time.variable = "year",
                                        treatment.identifier = 7,
                                        controls.identifier = c(1, 3:6, 8:13, 15),
                                        time.optimize.ssr=c(2006, 2009, 2012, 2015, 2018),
                                        time.plot=c(2006, 2009, 2012, 2015, 2018)
)

# Synth
synth.out.r <- synth(
  data.prep.obj = dataprep.out.r,
  method = "BFGS"
)

# Path plot
path.plot(synth.res = synth.out.r,
          dataprep.res = dataprep.out.r,
          Ylab = "PISA - Mathematics",
          Xlab = "Year",
          Ylim = c(370, 410),
          Legend = c("Jordan","Synthetic Jordan without Brazil"),
          Legend.position = "bottomright")
abline(v=2011, lty=2) # adding vertical line for treatment year
axis(1, at=seq(2006, 2018, by=2))

# ------------------------------------------------------------------------------
# Synthetic Control Analysis - Reading
# ------------------------------------------------------------------------------
# Data prep
dataprep.read <- dataprep(foo=data,
                          predictors = c("population", "gdp_pc", "political_stability", "student_teacher_filled"),
                          predictors.op = "mean",
                          time.predictors.prior=c(2006, 2009, 2012),
                          special.predictors = list(
                            list("pisa_math_filled", 2006, 'mean'),
                            list("pisa_read_filled", 2006, 'mean')),
                          dependent = "pisa_read_filled",
                          unit.variable = "country_code",
                          unit.names.variable = "country",
                          time.variable = "year",
                          treatment.identifier = 7,
                          controls.identifier = c(1:6, 8:13, 15),
                          time.optimize.ssr=c(2006, 2009, 2012, 2015, 2018),
                          time.plot=c(2006, 2009, 2012, 2015, 2018))

# Sythnetic control
synth.read <- synth(data.prep.obj = dataprep.read)

# Creating tables
synth.tables.read <- synth.tab(dataprep.res = dataprep.read,
                               synth.res = synth.read)
synth.tables.read$tab.w[1:13, ]
synth.tables.read$tab.pred[1:5, ]

# Path plot
path.plot(synth.res = synth.read,
          dataprep.res = dataprep.read,
          Ylab = "PISA - Reading",
          Xlab = "Year",
          Ylim = c(380,430),
          Legend = c("Jordan","Synthetic Jordan"),
          Legend.position = "bottomright")
abline(v=2011, lty=2) # adding vertical line for treatment year
axis(1, at=seq(2006, 2018, by=2))

# Gaps plot
gaps.plot(synth.res = synth.read,
          dataprep.res = dataprep.read,
          Ylab = "PISA - Reading",
          Xlab = "Year",
          Main = NA)
abline(v=2011, lty=2) # adding vertical line for treatment year
axis(1, at=seq(2006, 2018, by=2))