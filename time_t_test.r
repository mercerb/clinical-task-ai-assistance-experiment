"
The time analysis seeks to answer 3 questions:
1) Overall (not controlling for any factors besides ALA), does ALA save time?
2) With testers as repeated measures, does ALA save time?

Answer both here.
"

run_time_analysis <- function(ala_exp_data, image_folder) {

    ## Question 1: time savings overall

    time_summary <- ala_exp_data %>%
        dplyr::group_by(ALA) %>%
        dplyr::summarize(seconds.mean = mean(TotalSeconds), seconds.sd = sd(TotalSeconds))
    mean_diff_s <- time_summary$seconds.mean[2] - time_summary$seconds.mean[1]
    pct_ala_time_diff <- mean_diff_s / time_summary$seconds.mean[1]
    print(sprintf("Time spent with ALA compared to non-ALA: %g. Percent diff: %f", mean_diff_s, pct_ala_time_diff))
    # negative is good for ALA

    # start with very simple plots. total seconds with/without ALA
    p_time_deviation_seconds <- bar_plot_with_sddev(
        time_summary,
        x = "ALA",
        y = time_summary$seconds.mean, y_dev = time_summary$seconds.sd, fill = "ALA"
    ) +
        labs(
            #title = "Average time spent on runs with vs without ALA", 
            x = "ALA presence", y = "Time (seconds)")

    ggsave(sprintf("%s/time_deviation_seconds.jpeg", image_folder), p_time_deviation_seconds)

    # then break down by pair
    # expectation: pairs 1 & 5 are the most complex; 2 & 4 are medium; 3 is easiest
    # one thing we can suspect from this graph is that ALA made the most complex ones faster
    time_pair_summary <- ala_exp_data %>%
        group_by(ALA, Pair) %>%
        dplyr::summarize(seconds.mean = mean(TotalSeconds), seconds.sd = sd(TotalSeconds))

    times_by_pair <- bar_plot_with_sddev(
        time_pair_summary,
        x = "Pair",
        y = time_pair_summary$seconds.mean, y_dev = time_pair_summary$seconds.sd, fill = "ALA"
    ) +
        labs(
            #title = "Average time spent by pair with vs without ALA", 
            x = "Pair", y = "Time (seconds)")
    ggsave(sprintf("%s/time_deviation_seconds_by_pair.jpeg", image_folder), times_by_pair)

    # and by tester
    time_tester_summary <- ala_exp_data %>%
        group_by(ALA, Tester) %>%
        dplyr::summarize(seconds.mean = mean(TotalSeconds), seconds.sd = sd(TotalSeconds))

    times_by_tester <- bar_plot_with_sddev(
        time_tester_summary,
        x = "Tester",
        y = time_tester_summary$seconds.mean, y_dev = time_tester_summary$seconds.sd, fill = "ALA"
    ) +
        labs(#title = "Average time spent by tester with vs without ALA", 
            x = "Tester", y = "Time (seconds)")
    ggsave(sprintf("%s/time_deviation_seconds_by_tester.jpeg", image_folder), times_by_tester)

    ## Question 2: time savings by tester

    if(grepl("random", image_folder, fixed = TRUE)) {
        # random dataset can't do paired data
        res <- t.test(TotalSeconds ~ ALA, data=ala_exp_data, alternative="less")
        result_text <- "Randomized dataset: does ALA contribute to faster runs? p=%g"
        print(sprintf(result_text, res$p.value))
        return()
    }

    ## Paired data: reshape as pairs so we get diffs across each tester/pair
    ala_paired_data <- load_paired_data(ala_exp_data)

    # plot diff (average savings) per tester
    time_diff_by_tester <- ggboxplot(ala_paired_data, x = "Tester", y = "ala_time_diff", add = "point") +
        geom_hline(aes(yintercept = 0), color = "blue", size = 1) +
        labs(
            #title = "Average time savings with ALA by tester", 
            x = "Tester", y = "Time Difference From Control (seconds)") +
        theme_classic()
    ggsave(sprintf("%s/time_diff_by_tester.jpeg", image_folder), time_diff_by_tester)

    ## Identify outliers
    # 3 outliers, 2 extreme
    outliers <- ala_paired_data %>%
        identify_outliers(ala_time_diff)
    outliers[c("Tester", "Pair", "ala_time_diff", "is.outlier", "is.extreme")]

    paired_no_outliers <- subset(ala_paired_data, !ala_time_diff %in% outliers$ala_time_diff)

    ## Check normality
    # normality is the assumption, so ideally p will be >0.05
    # compute the difference

    ## Run Shapiro-Wilk test for normality of the differences
    shapiro.outliers <- shapiro.test(ala_paired_data$ala_time_diff)
    shapiro.no_outliers <- shapiro.test(paired_no_outliers$ala_time_diff)
    print(sprintf("Shapiro tests: P=%g for outliers, P=%g for no outliers", shapiro.outliers$p.value, shapiro.no_outliers$p.value))

    p_times <- ggpubr::ggdensity(ala_paired_data$ala_time_diff,
        main = "Density plot of time differences",
        xlab = "Time (seconds)"
    ) + theme_classic()
    p_times_no_outliers <- ggpubr::ggdensity(paired_no_outliers$ala_time_diff,
        main = "Density plot of time differences",
        xlab = "Time (seconds)"
    ) + theme_classic()
    qq_times <- ggqqplot(ala_paired_data$ala_time_diff) + theme_classic()
    qq_times_no_outliers <- ggqqplot(paired_no_outliers$ala_time_diff) + theme_classic()
    all <- p_times + p_times_no_outliers + qq_times + qq_times_no_outliers + plot_annotation(tag_levels = "I")
    title <- "Normality analysis of ALA time savings with vs without outliers"
    ggsave(sprintf("%s/time_diff_normality.jpeg", image_folder), all)

    ## t-tests
    # I'm using paired data, so these will all be paired t-tests.
    # I run the two-sided and one-sided (assuming ALA saves time) tests.
    # I run each test with and without outliers, and for 2 confidence intervals,
    # for a total of 8 tests.
    # -------------

    # two-sided t-test with outliers
    for (conf_level in c(0.95, 0.85)) {
        res <- t.test(ala_paired_data$ala_seconds, ala_paired_data$non_ala_seconds,
            paired = TRUE,
            conf.level = conf_level
        )
        print(res)
    }

    # two-sided t-test without outliers
    for (conf_level in c(0.95, 0.85)) {
        res <- t.test(paired_no_outliers$ala_seconds, paired_no_outliers$non_ala_seconds,
            paired = TRUE,
            conf.level = conf_level
        )
        print(res)
    }

    # 1-sided t-test with outliers
    for (conf_level in c(0.95, 0.85)) {
        res <- t.test(ala_paired_data$ala_seconds, ala_paired_data$non_ala_seconds,
            paired = TRUE,
            alternative = "less",
            conf.level = conf_level
        )
        print(res)
    }

    # 1-sided t-test without outliers
    for (conf_level in c(0.95, 0.85)) {
        res <- t.test(paired_no_outliers$ala_seconds, paired_no_outliers$non_ala_seconds,
            paired = TRUE,
            alternative = "less",
            conf.level = conf_level
        )
        print(res)
    }
}
