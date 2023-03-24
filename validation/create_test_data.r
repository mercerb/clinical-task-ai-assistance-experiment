" 
Create fake test data to validate analysis: 
an 'ideal' dataset; a 'bad' dataset; and a 'random' dataset.

In the 'ideal' experiment, there is just one pair, not 5, and more testers.
The ALA trials are significantly faster than the non-ALA trials.
They are also much closer to the real outcome than the non-ALA trials.

In the 'bad' dataset, there is missing data. The 'random' dataset,
like the bootstrapped dataset, has ALA labels randomized.
"
good_filename <- "validation/test_data_good.csv"
bad_filename <- "validation/test_data_bad.csv"
random_filename <- "validation/test_data_random.csv"

# Function for creating random numbers, normally distributed around mean/sd, all positive
random_vector <- function(num_samples, mean, sd) {
    rnorm <- rnorm(num_samples * 2, mean, sd)
    x <- rnorm[rnorm > 0]
    z <- sample(x, num_samples, replace = FALSE)
    if (length(z) == num_samples) return(z)
    stop(sprintf("z is len %f", length(z)))
}

round_diameter <- function(float) {
    # return closest size to float
    sizes <- c(2.5, 2.75, 3, 3.25, 3.5, 3.75, 4)
    return(sizes[which.min(abs(sizes - float))])
}

prepare_good_test_data <- function(outcomes) {
    if (file.exists(good_filename)) {
        return(read_csv(good_filename))
    }

    num_testers <- 100
    num_pairs <- 1
    num_runs <- 2 * num_pairs
    num_samples <- num_testers * num_runs

    # Generate testers and runs and pairs
    tester_ids <- sprintf("Tester%d", 1:num_testers)
    Tester <- rep(tester_ids, each = num_runs)
    Pair <- rep(3, times = num_samples)
    Run <- rep(c("3A", "3B"), num_testers * num_pairs)

    ala_first_half <- rep(c(TRUE, FALSE), times = num_testers / 2)
    ala_second_half <- rep(c(FALSE, TRUE), times = num_testers / 2)
    ALA <- c(ala_first_half, ala_second_half)

    # Inspired by actual results but modified so that ALA is faster, more correct, and more consistent
    time_mean_seconds_ala <- 60
    time_mean_seconds_no_ala <- 120
    time_sd_seconds_ala <- 6
    time_sd_seconds_no_ala <- 12
    length_sd_mm_ala <- 5
    length_sd_mm_no_ala <- 15
    dia_sd_mm_ala <- 0.2
    dia_sd_mm_no_ala <- 0.4

    actual_outcome_3A_len <- filter(outcomes, (outcomes$Run == "3A"))$ResultTreatableLength
    actual_outcome_3A_dia <- filter(outcomes, (outcomes$Run == "3A"))$ResultMinTreatableDia
    actual_outcome_3B_len <- filter(outcomes, (outcomes$Run == "3B"))$ResultTreatableLength
    actual_outcome_3B_dia <- filter(outcomes, (outcomes$Run == "3B"))$ResultMinTreatableDia

    num_trials <- num_testers / num_runs # each run is seen by half testers with ALA + half without
    results_len_3A_ala <- random_vector(num_trials, actual_outcome_3A_len + 2, length_sd_mm_ala)
    results_dia_3A_ala <- random_vector(num_trials, actual_outcome_3A_dia + .2, dia_sd_mm_ala)
    results_dia_3A_ala <- map_dbl(results_dia_3A_ala, round_diameter)

    results_len_3B_ala <- random_vector(num_trials, actual_outcome_3B_len - 2, length_sd_mm_ala)
    results_dia_3B_ala <- random_vector(num_trials, actual_outcome_3B_dia - .2, dia_sd_mm_ala)
    results_dia_3B_ala <- map_dbl(results_dia_3B_ala, round_diameter)

    results_len_3A_no_ala <- random_vector(num_trials, actual_outcome_3A_len + 10, length_sd_mm_no_ala)
    results_dia_3A_no_ala <- random_vector(num_trials, actual_outcome_3A_dia + .4, dia_sd_mm_no_ala)
    results_dia_3A_no_ala <- map_dbl(results_dia_3A_no_ala, round_diameter)

    results_len_3B_no_ala <- random_vector(num_trials, actual_outcome_3B_len - 10, length_sd_mm_no_ala)
    results_dia_3B_no_ala <- random_vector(num_trials, actual_outcome_3B_dia - .4, dia_sd_mm_no_ala)
    results_dia_3B_no_ala <- map_dbl(results_dia_3B_no_ala, round_diameter)

    results_time_ala <- random_vector(num_samples / 2, time_mean_seconds_ala, time_sd_seconds_ala)
    results_time_no_ala <- random_vector(num_samples / 2, time_mean_seconds_no_ala, time_sd_seconds_no_ala)

    # Assemble dataframe with results
    good_df <- data.frame(Tester, Run, Pair, ALA)
    good_df <- good_df %>% mutate_at(c("Tester", "Run", "Pair"), as.factor)

    good_df <- good_df[order(ALA, Run, decreasing = TRUE), ] # 3B True, 3A True, 3B False, 3A False

    good_df$TotalSeconds <- c(results_time_ala, results_time_no_ala)
    len <- c(results_len_3B_ala, results_len_3A_ala, results_len_3B_no_ala, results_len_3A_no_ala)
    dia <- c(results_dia_3B_ala, results_dia_3A_ala, results_dia_3B_no_ala, results_dia_3A_no_ala)
    good_df$ResultTreatableLength <- len
    good_df$ResultMinTreatableDia <- dia

    write.csv(good_df, good_filename, row.names = FALSE)
}

increase_sd <- function(col){
    return(case_when((col %% 2) != 0 ~ col + 3*sd(col), TRUE ~ col))
}

prepare_bad_test_data <- function() {
    # unlike the 'good data', which is generated from scratch,
    # the 'bad data' is a worse version of the real data
    if (file.exists(bad_filename)) {
        return(read_csv(bad_filename))
    }
    ala_df <- load_all_data()
    ala_df <- dplyr::select(ala_df, "Tester","Run","Pair","ALA","TotalSeconds","ResultTreatableLength","ResultMinTreatableDia")

    # remove testers who completed all runs
    bad_df <- ala_df[ala_df$Tester != "Tester2" & ala_df$Tester != "Tester3" & ala_df$Tester != "Tester9", ] 

    # remove various pairs from various testers to impact block design
    # also so that sample size drops to below 30
    bad_df <- bad_df[bad_df$Tester != "Tester1" | bad_df$Pair != 3, ] 
    bad_df <- bad_df[bad_df$Tester != "Tester4" | (bad_df$Pair != 3 & bad_df$Pair != 4), ] 
    bad_df <- bad_df[bad_df$Tester != "Tester5" | (bad_df$Pair != 2 & bad_df$Pair != 3), ] 
    bad_df <- bad_df[bad_df$Tester != "Tester6" | (bad_df$Pair != 4 & bad_df$Pair != 5), ] 
                        
    # and now...randomly increase the standard deviation of half the result rows
    bad_df <- bad_df %>% mutate(TotalSeconds = increase_sd(TotalSeconds))
    bad_df <- bad_df %>% mutate(ResultMinTreatableDia = increase_sd(ResultMinTreatableDia))
    bad_df <- bad_df %>% mutate(ResultTreatableLength = increase_sd(ResultTreatableLength))

    # also, do not give an even number of ALA vs non ALA runs. delete 10% of ALA "A" runs randomly
    ala_a_runs <- bad_df$ALA == TRUE & (substring(bad_df$Run, 2, 2) == "A") # where ALA is true and the run is "A"
    bad_df <- bad_df[ -sample(which(ala_a_runs == TRUE), round(sum(ala_a_runs) * 0.1)),] 

    write.csv(bad_df, bad_filename, row.names = FALSE)
}

prepare_random_test_data <- function() {
    set.seed(123)
    ala_df <- load_all_data()
    ala_df <- dplyr::select(ala_df, "Tester","Run","Pair","ALA","TotalSeconds","ResultTreatableLength","ResultMinTreatableDia")

    # take original df -> randomly shuffle all ALA labels -> replace ALA column with the random ones
    random_df <- transform(ala_df, ALA = sample(ALA))
    write.csv(random_df, random_filename, row.names = FALSE)
}

prepare_test_data <- function(outcomes) {
    prepare_good_test_data(outcomes)
    prepare_bad_test_data()
    prepare_random_test_data()
}
