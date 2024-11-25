# streptokinase_meta_analysis_data_wrangle.R
# format data from "DOI: 10.1056/NEJM199207233270406" figure 1 "individual analysis and conventional meta analysis odds ratio"
# 2024-11-25

# load libraries
library(dplyr)
library(tidyverse)

# read in csv
df <- read.csv(file = "02_data/individual_analysis_and_conventional_meta_analysis_odds_ratio.csv", header = FALSE)

# pivot wider transform
formatted_df <- df %>%
  mutate(id = rep(1:(nrow(df)/3), each = 3)) %>%  # Add an ID to group by
  pivot_wider(
    names_from = V1,  # Values of `V1` will become new column names
    values_from = V2)  # Values in `V2` will fill the cells

# number of patients per trail in order of year
patient_no <- c(23,42,167,730,426,321,517,206,107,108,91,23,595,728,230,24,483,58,315,1741,11712,52,59,38,44,98,64,219,107,25,368,17187,66,36974)

# add patient number to formatted df
formatted_df$patient_no <- patient_no

# save data frame
write.csv(formatted_df, file = "02_data/formatted_meta_analysis_odds_ratio.csv")
 
