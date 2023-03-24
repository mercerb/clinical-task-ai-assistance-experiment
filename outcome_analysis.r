"
The outcome analysis seeks to answer 3 questions:
1) Does ALA have a meaningful impact on stent length or diameter?
2) Do users of ALA have more consistency (less standard deviation) than the norm?
3) Does ALA get results closer to the 'ideal' outcome? (define ideal as what actually happened at the CRF)

Here, focus on Question 3.
"

plot_pair_len_and_dia <- function(ala_exp_data, image_folder, pair) {
    pair_data <- dplyr::filter(ala_exp_data, (ala_exp_data$Pair == pair))
    run_data_A <- dplyr::filter(pair_data, (pair_data$RunAB == "A"))
    run_data_B <- dplyr::filter(pair_data, (pair_data$RunAB == "B"))

    actual_len <- run_data_A$ResultTreatableLength.actual[1]
    actual_dia <- run_data_A$ResultMinTreatableDia.actual[1]
    if (pair == 2) actual_len <- 0

    len_plot_A <- ggplot(run_data_A, aes(x = ALA, y = ResultTreatableLength, color = ALA)) +
        geom_point(size = 4) +
        theme_classic() +
        geom_hline(yintercept = actual_len, color = "green")
    dia_plot_A <- ggplot(run_data_A, aes(x = ALA, y = ResultMinTreatableDia, color = ALA)) +
        ylim(2, 5) +
        geom_point(size = 4) +
        theme_classic()
    if (pair != 2) dia_plot_A <- dia_plot_A + geom_hline(yintercept = actual_dia, color = "green")

    actual_len <- run_data_B$ResultTreatableLength.actual[1]
    actual_dia <- run_data_B$ResultMinTreatableDia.actual[1]
    if (pair == 2) actual_len <- 0

    len_plot_B <- ggplot(run_data_B, aes(x = ALA, y = ResultTreatableLength, color = ALA)) +
        geom_point(size = 4) +
        theme_classic() +
        geom_hline(yintercept = actual_len, color = "green")
    dia_plot_B <- ggplot(run_data_B, aes(x = ALA, y = ResultMinTreatableDia, color = ALA)) +
        ylim(2, 5) +
        geom_point(size = 4) +
        theme_classic()
    if (pair != 2) dia_plot_B <- dia_plot_B + geom_hline(yintercept = actual_dia, color = "green")

    A <- wrap_elements((len_plot_A + dia_plot_A) + plot_annotation(title = sprintf("Run %sA", pair)))
    B <- wrap_elements((len_plot_B + dia_plot_B) + plot_annotation(title = sprintf("Run %sB", pair)))
    all <- A + B

    ggsave(sprintf("%s/outcomes_pair_%s.jpeg", image_folder, pair), width = 12, height = 3, units = "in", all)
}

run_outcome_analysis <- function(ala_exp_data, image_folder) {
    # Plot length and diameter per run
    for (pair in unique(ala_exp_data$Pair)) {
        plot_pair_len_and_dia(ala_exp_data, image_folder, pair)
    }

    # Calculate difference of each result from the actual CRF outcome
    ala_exp_data$diff_len_from_actual <- ala_exp_data$ResultTreatableLength - ala_exp_data$ResultTreatableLength.actual
    ala_exp_data$diff_dia_from_actual <- ala_exp_data$ResultMinTreatableDia - ala_exp_data$ResultMinTreatableDia.actual
    ala_exp_data$abs_diff_len_from_actual <- abs(ala_exp_data$ResultTreatableLength - ala_exp_data$ResultTreatableLength.actual)
    ala_exp_data$abs_diff_dia_from_actual <- abs(ala_exp_data$ResultMinTreatableDia - ala_exp_data$ResultMinTreatableDia.actual)

    # separate pair 2 for the summary table
    ala_no_pair_two <- subset(ala_exp_data, !(Pair %in% c(2))) 
    summary <- ala_no_pair_two %>%
        dplyr::group_by(ALA, Run) %>%
        dplyr::summarize(
            mean_diff_len = round(mean(diff_len_from_actual, na.rm = TRUE), digits = 2),
            mean_diff_dia = round(mean(diff_dia_from_actual, na.rm = TRUE), digits = 2),
        )

    summary_ala <- ala_no_pair_two %>%
        dplyr::group_by(ALA) %>%
        dplyr::summarize(
            mean_diff_len = round(mean(diff_len_from_actual, na.rm = TRUE), digits = 2),
            mean_diff_dia = round(mean(diff_dia_from_actual, na.rm = TRUE), digits = 2),
            MAE_len = round(mean(abs_diff_len_from_actual, na.rm = TRUE), digits = 2),
            MAE_dia = round(mean(abs_diff_dia_from_actual, na.rm = TRUE), digits = 2),
            MAE_len_sd = round(sd(abs_diff_len_from_actual, na.rm = TRUE), digits = 2),
            MAE_dia_sd = round(sd(abs_diff_dia_from_actual, na.rm = TRUE), digits = 2),
        )
    print(summary_ala)

    # Display differences in a bar plot
    grouped_len <- bar_plot_with_sddev(
        df = summary_ala,
        x = "ALA",
        y = summary_ala$MAE_len,
        y_dev = summary_ala$MAE_len_sd,
        fill = "ALA"
    ) +
        labs(title = "Treatable length: absolute difference from actual", x = "ALA presence", y = "Treatable Length Absolute Difference (mm)")

    grouped_dia <- bar_plot_with_sddev(
        df = summary_ala,
        x = "ALA",
        y = summary_ala$MAE_dia,
        y_dev = summary_ala$MAE_dia_sd,
        fill = "ALA"
    ) +
        labs(title = "Minimum diameter: absolute difference from actual", x = "ALA presence", y = "Min Diameter Absolute Difference (mm)")

    title <- "Difference in Treatable Length and Diameter vs Actual"
    p_diff_len_dia <- grouped_len + grouped_dia + plot_annotation(tag_levels = "I")
    print(p_diff_len_dia)
    ggsave(sprintf("%s/outcomes_diff_vs_actual.jpeg", image_folder), p_diff_len_dia)

    # t-test for difference from actual: length, then diameter
    t.test.res.len <- t.test(ala_no_pair_two$abs_diff_len_from_actual ~ ala_no_pair_two$ALA)
    w.test.res.dia <- wilcox.test(ala_no_pair_two$abs_diff_dia_from_actual ~ ala_no_pair_two$ALA)
    print(sprintf("Diff from actual (no diff with >0.05): length, p=%g & dia, p=%g", t.test.res.len$p.value, w.test.res.dia$p.value))

    # Now join results as (length, diameter) pairs & calculate Euclidean distance
    dist_from_origin <- function(x, y) sqrt((x^2) + (y^2))
    ala_exp_data$diff_len_dia <- dist_from_origin(ala_exp_data$diff_len_from_actual, ala_exp_data$diff_dia_from_actual)
    # Calculate the percent improvement in accuracy due to ALA with Euclidean dist.
    diff_euclid <- ala_exp_data %>%
        group_by(ALA) %>%
        dplyr::summarize(mean_diff = mean(diff_len_dia, na.rm = TRUE))
    diff_euclid
    pct_change_euclid <- (diff_euclid$mean_diff[2] - diff_euclid$mean_diff[1]) / diff_euclid$mean_diff[1]
    print(sprintf("Percent change for Euclid distance: %g", pct_change_euclid))
    # 25% smaller/better with ALA, a lot more, but misleading bc it's only 2mm

    # Show plot with tester2 example
    example_distance <- ggplot(ala_exp_data) +
        geom_segment(aes(x = 0, y = 0, xend = -3, yend = -0.75), color = "green", arrow = arrow(length = unit(0.03, "npc"))) +
        geom_point(aes(x = 0, y = 0), color = "green", size = 2) +
        geom_point(aes(x = diff_len_from_actual, y = diff_dia_from_actual, color = ALA), size = 2) +
        theme_classic() +
        labs(
            x = "Difference in length from actual result (mm)", y = "Difference in diameter from actual result (mm)",
        )
    # title = "Vector length between Tester2 choice and actual: 3.09mm"
    ggsave(sprintf("%s/euclidean_distance_example.jpeg", image_folder),
        example_distance,
        width = 5.5, height = 5, dpi = 300
    )

    ## Mahalanobis Distance
    # treat (stent length, min diameter) as a 2D (x, y) point in space
    # center around zero and calculate distance using covariance

    ## Prepare data for Mahalanobis calculation, and plot data ellipse
    all_data <- dplyr::select(ala_exp_data, diff_len_from_actual, diff_dia_from_actual)
    # 1. get center & covariance
    all_data <- na.omit(all_data)
    all_data.center <- colMeans(all_data)
    all_data.cov <- cov(all_data)
    # 2. get ellipse
    all_data_ellipse <- get_data_ellipse(all_data, all_data.center, all_data.cov)

    all_data_with_ellipse <- ggplot(ala_exp_data, aes(x = diff_len_from_actual, y = diff_dia_from_actual)) +
        geom_point(aes(color = ALA), size = 4) +
        geom_point(aes(x = 0, y = 0), color = "green", size = 4) +
        geom_polygon(data = all_data_ellipse, fill = "grey80", color = "black", alpha = 0.3) +
        theme_classic() +
        labs(
            x = "Difference in length from actual result (mm)", y = "Difference in diameter from actual result (mm)",
            title = "Data ellipse for all results"
        )

    ## Create ellipse for ALA data
    ala <- dplyr::filter(ala_exp_data, ALA == TRUE)
    ala <- dplyr::select(ala, diff_len_from_actual, diff_dia_from_actual)
    # 1. get center & covariance
    ala <- na.omit(ala)
    ala.center <- colMeans(ala)
    ala.cov <- cov(ala)
    # 2. get ellipse
    ala_ellipse <- get_data_ellipse(ala, ala.center, ala.cov)

    ## Create Mahalanobis ellipse for non-ALA data
    no_ala <- dplyr::filter(ala_exp_data, ALA == FALSE)
    no_ala <- dplyr::select(no_ala, diff_len_from_actual, diff_dia_from_actual)
    # 1. get center & covariance
    no_ala <- na.omit(no_ala)
    no_ala.center <- colMeans(no_ala)
    no_ala.cov <- cov(no_ala)
    # 2. get ellipse
    no_ala_ellipse <- get_data_ellipse(no_ala, no_ala.center, no_ala.cov)

    # Graph ellipses super-imposed
    both_ellipses <- ggplot(ala_exp_data, aes(x = diff_len_from_actual, y = diff_dia_from_actual)) +
        geom_point(aes(color = ALA), size = 4) +
        geom_point(aes(x = 0, y = 0), color = "green", size = 4) +
        geom_polygon(data = ala_ellipse, fill = plot_blue, color = plot_blue, alpha = 0.4) +
        geom_polygon(data = no_ala_ellipse, fill = plot_red, color = plot_red, alpha = 0.2) +
        theme_classic() +
        labs(
            x = "Difference in length from actual result (mm)", y = "Difference in diameter from actual result (mm)",
            title = "Data ellipse, ALA vs non ALA results"
        )

    # both_title <- "Outcome accuracy: differences between participant choices and actual outcomes"
    both_ellipse_graphs <- (all_data_with_ellipse / both_ellipses) + plot_annotation(tag_levels = "I")
    ggsave(sprintf("%s/mahal_both_ellipse_graphs.jpeg", image_folder),
        both_ellipse_graphs,
        width = 8, height = 7.5, dpi = 300
    )

    ## Now, statistically compare Mahalanobis distance between ALA and non-ALA populations
    # For non-ALA, to compare to ALA, we need to use the ALA center & covariance
    # Otherwise the distances will scale to non-ALA and the comparison btw the two won't make sense
    ala$mdist <- mahalanobis(ala, ala.center, ala.cov)
    no_ala$mdist <- mahalanobis(no_ala, ala.center, ala.cov)

    # Plot histograms together. Compare distribution of distances for ala vs non-ala points
    # the ditsances are the lengths of all vectors to the centroid
    mahal_data <- rbind(
        data.frame(mahal_distances = ala$mdist, distribution = "ALA = TRUE"),
        data.frame(mahal_distances = no_ala$mdist, distribution = "ALA = FALSE")
    )

    mahal_hist <- ggplot(mahal_data, aes(mahal_distances, fill = distribution)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
        labs(
           # title = "Distribution of Mahalanobis Distances, Centered Around ALA Population",
            x = "Mahalanobis Distances With Respect to ALA Center & Covariance",
            y = "Frequency"
        ) +
        theme_classic()
    ggsave(sprintf("%s/mahal_histogram_both.jpeg", image_folder), mahal_hist, width = 8, height = 6.5, dpi = 300)

    ## Run Kolmogorov-Smirnov (K-S) test for a nonparametric sample comparison
    # Tells us if the ALA distance distribution is significantly smaller (more accurate) 
    # p > 0.05 means that they are essentially the same
    ks.test.result <- ks.test(ala$mdist, no_ala$mdist, alternative = "greater")
    ks.test.result

    # Re-calculate the percent improvement in accuracy due to ALA with Mahal. dist
    # for actual data, it is 30% smaller with ALA. pretty similar to Euclidean
    pct_change_mahal <- (mean(ala$mdist) - mean(no_ala$mdist)) / mean(no_ala$mdist)

    results_text <- "Mahal distances p-value %g (<0.05 is better); percent improvement in accuracy: %g %%"
    print(sprintf(results_text, ks.test.result$p.value, pct_change_mahal))

}
