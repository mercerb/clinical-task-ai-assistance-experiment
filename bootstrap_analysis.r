"
Modified bootstrap analysis with 2701 simulations of the data, ALA labels shuffled.

2701 is the number of possible combinations: 74 trials C 2 ALA options (true/false)
Where the actual results (where ALA is coded correctly) fall in this distribution
indicate appropriate confidence intervals. For ex, if the true result falls at the 85% 
percentile mark, there's a 15% chance of seeing a result at least as extreme as this one by chance.
"

run_bootstrap_analysis <- function(ala_exp_data) {
    ala_select <- ala_exp_data[c(
        "Tester", "Pair", "Run", "ALA", "TotalSeconds",
        "ResultTreatableLength", "ResultTreatableLength.actual",
        "ResultMinTreatableDia", "ResultMinTreatableDia.actual"
    )]
    set.seed(123)
    n <- length(ala_select$ALA)
    boolean_vector <- rep(c(TRUE, FALSE), (n / 2))
    n_sims <- 2701

    get_sim_df <- function() {
        df <- ala_select
        df$ALA <- sample(boolean_vector, n, replace = FALSE) # randomized
        return(df)
    }

    # using Euclidean distance here; it wasn't too different from Mahal
    # it's easier for bulk calculations because it doesn't require "drop.na"
    dist_from_origin <- function(x, y) sqrt((x^2) + (y^2))

    get_summary_for_df <- function(df) {
        df$diff_len_from_actual <- df$ResultTreatableLength - df$ResultTreatableLength.actual
        df$diff_dia_from_actual <- df$ResultMinTreatableDia - df$ResultMinTreatableDia.actual
        df$diff_len_dia <- dist_from_origin(df$diff_len_from_actual, df$diff_dia_from_actual)

        summary <- df %>%
            dplyr::group_by(ALA) %>%
            dplyr::summarize(
                mean_diff_len_dia = mean(diff_len_dia, na.rm = TRUE),
                sd_diff_len_dia = sd(diff_len_dia, na.rm = TRUE),
                mean_seconds = mean(TotalSeconds, na.rm = TRUE),
                sd_seconds = sd(TotalSeconds, na.rm = TRUE)
            )
        return(summary)
    }

    df_spread <- function(df) {
        df <- tidyr::pivot_wider(df,
            names_from = ALA,
            values_from = c(mean_diff_len_dia, sd_diff_len_dia, mean_seconds, sd_seconds),
            names_glue = "{.value}.{ALA}"
        )
        df$ala_seconds_diff <- df$mean_seconds.TRUE - df$mean_seconds.FALSE
        df$mean_outcome_diff <- mean(c(df$mean_diff_len_dia.FALSE, df$mean_diff_len_dia.FALSE))
        return(df)
    }

    # Run the replication and gather the summarized results
    sim_list <- replicate(n = n_sims, expr = {
        get_sim_df()
    }, simplify = FALSE)
    sim_summary_list <- map(sim_list, get_summary_for_df)
    df_list <- lapply(1:length(sim_summary_list), function(x) (df_spread(sim_summary_list[[x]])))
    sim_summary_all <- bind_rows(df_list)

    # Summarize the actual data from the experiment
    summary_actual <- df_spread(get_summary_for_df(ala_exp_data))

    ## Now look at results
    # For both outcomes and time, smaller is better
    # Outcome accuracy is determined by distance from the optimal,
    # so smaller distances are better.
    # For time, the difference between time spent with ALA
    # from time spent without it (ALA minus non ALA) should be negative, ideally

    ## (1) Outcomes across all testers and runs
    # each df will have an average distance from the correct outcome with vs without ALA
    # compare to the true average distance from the correct outcome with vs without ALA
    actual_ALA_mean_diff <- summary_actual$mean_diff_len_dia.TRUE # 8.12
    mean_diff_simulated <- mean(sim_summary_all$mean_outcome_diff) # 9.43
    empirical_cdf_outcomes <- ecdf(sim_summary_all$mean_outcome_diff)
    empirical_cdf_outcomes(actual_ALA_mean_diff) # 0.14. 14% chance of getting a diff this small or smaller

    hist_outcomes <- ggplot(sim_summary_all, aes(mean_outcome_diff)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
        labs(
            # title = "Mean Difference From Optimal: Actual ALA Outcome Compared to 2701 Random Simulations",
            # subtitle = "Blue line at 8.12mm is smaller than 86% of the distribution",
            x = "Mean (length, diameter) distance from actual outcome (Euclidean)",
            y = "Count (# of Simulations)"
        ) +
        geom_vline(data = summary_actual, aes(xintercept = mean_diff_len_dia.TRUE, linetype = "ALA mean distance"), color = "blue") +
        theme_classic()
    ggsave("images_normal/bootstrap_outcome_histogram.jpeg", hist_outcomes, width = 8, height = 6.5, dpi = 300)

    ## (2) Times across all testers and runs
    # each df will have an average time spent with vs without ALA
    ala_seconds_diff_actual <- summary_actual$ala_seconds_diff # -35
    ala_seconds_diff_simulated <- mean(sim_summary_all$ala_seconds_diff) # -0.56
    empirical_cdf_time <- ecdf(sim_summary_all$ala_seconds_diff)
    empirical_cdf_time(ala_seconds_diff_actual)
    # 0.17, 17% chance of getting a diff this small or smaller
    # ALA is better than at least 83% of random cases...for both outcomes and time

    hist_times <- ggplot(sim_summary_all, aes(ala_seconds_diff)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
        labs(
            # title = "Mean Time Savings: Actual ALA Outcome Compared to 2701 Random Simulations",
            #  subtitle = "Blue line at -35 seconds is smaller than 83% of the distribution",
            x = "Average difference in seconds from non-ALA to ALA for simulation",
            y = "Count (# of Simulations)"
        ) +
        geom_vline(data = summary_actual, aes(xintercept = ala_seconds_diff, linetype = "ALA seconds saved"), color = "blue") +
        theme_classic()
    ggsave("images_normal/bootstrap_time_histogram.jpeg", hist_times, width = 8, height = 6.5, dpi = 300)
}
