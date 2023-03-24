"
Load all required libraries
Load all functions to input data from csvs
Create test data
"

## Load all packages
# install if necessary, like so:
# install.packages(c("tidyverse","skimr","caret","rpart","rpart.plot", "ggpubr"))
library(tidyverse)
library(skimr)
library(caret)
library(rpart)
library(rpart.plot)
library(ggpubr)
library(rstatix)
library(stats)
library(plyr)
library(dplyr)
library(PairedData)
library(patchwork)
library(car)
library(ggplot2)
library(gridExtra)
library(ggpmisc)
library(RColorBrewer)
library(pwr)

rm(list = ls()) # clear environment
# run setwd() if necessary

load_all_data <- function(data_source = "actual") {
    filename <- switch(data_source,
        "actual" = "ala_exp_data_all.csv",
        "test_good" = "validation/test_data_good.csv",
        "test_bad" = "validation/test_data_bad.csv",
        "test_random" = "validation/test_data_random.csv"
    )

    ala_exp_data <- read_csv(filename)
    ala_exp_data$RunAB <- substr(ala_exp_data$Run, nchar(ala_exp_data$Run)-1+1, nchar(ala_exp_data$Run))

    # set column types
    number_cols <- c("ResultTreatableLength", "ResultMinTreatableDia", "ResultStent1Length", "ResultStent2Length", "catheter_id", "depth_value", "num_frames", "pullback_speed")
    ala_exp_data <- ala_exp_data %>% mutate_at(intersect(names(ala_exp_data), number_cols), as.numeric)

    factor_cols <- c("Order", "Pair", "Tester", "Run", "ALA", "id")
    ala_exp_data <- ala_exp_data %>% mutate_at(intersect(names(ala_exp_data), factor_cols), as.factor)

    # combine with actual outcomes
    actual_outcomes <- load_actual_outcomes()
    ala_exp_data <- merge(ala_exp_data, actual_outcomes, by = "Run", suffixes = c("", ".actual"))

    return(ala_exp_data)
}

load_actual_outcomes <- function() {
    f <- "actual_outcomes.csv"
    actual_outcomes <- read_csv(f)
    actual_outcomes <- actual_outcomes %>%
        dplyr::rename(
            "Run" = "ID",
            "ResultTreatableLength" = "Total_treatment_length",
            "ResultMinTreatableDia" = "Min_stent_diameter"
        )

    return(actual_outcomes[c("Run", "ResultTreatableLength", "ResultMinTreatableDia")])
}

load_paired_data <- function(ala_exp_data) {
    ala_paired_data <- dplyr::select(ala_exp_data, Tester, Pair, ALA, TotalSeconds)

    ala_paired_data <- ala_paired_data %>% spread(key = "ALA", value = "TotalSeconds")
    ala_paired_data <- ala_paired_data %>%
        dplyr::rename(
            "ala_seconds" = `TRUE`,
            "non_ala_seconds" = `FALSE`
        )
    ala_paired_data$ala_time_diff <- ala_paired_data$ala_seconds - ala_paired_data$non_ala_seconds

    return(ala_paired_data)
}

source("validation/create_test_data.r")
prepare_test_data(load_actual_outcomes())
