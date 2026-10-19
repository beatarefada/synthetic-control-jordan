# Required packages for Synthetic Control Analysis
required_packages <- c("Synth", "dplyr", "purrr", "tidyr", "zoo")

# Install missing packages automatically
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}
