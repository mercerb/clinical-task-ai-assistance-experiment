"
The outcome analysis seeks to answer 3 questions:
1) Does ALA have a meaningful impact on stent length or diameter?
2) Do users of ALA have more consistency (less standard deviation) than the norm?
3) Does ALA get results closer to the 'ideal' outcome? (define ideal as what actually happened at the CRF)

Here, focus on Question 1.
"

run_outcome_summary <- function(ala_exp_data, image_folder) {
    summary_all_runs <- ala_exp_data %>%
        group_by(ALA) %>%
        dplyr::summarize(
            mean.length = mean(ResultTreatableLength, na.rm = TRUE),
            mean.diameter = mean(ResultMinTreatableDia, na.rm = TRUE),
            sd.length = sd(ResultTreatableLength, na.rm = TRUE),
            sd.diameter = sd(ResultMinTreatableDia, na.rm = TRUE)
        )
    print(summary_all_runs)

    ## Plot: Diameter & length deviation, averaged across runs
    p_dia_deviation_all <- bar_plot_with_sddev(
        df = summary_all_runs,
        x = "ALA",
        y = summary_all_runs$mean.diameter,
        y_dev = summary_all_runs$sd.diameter,
        fill = "ALA"
    ) +
        labs(title = "Minimum Diameter Across Runs", x = "ALA presence", y = "Minimum Diameter (mm)")

    p_len_deviation_all <- bar_plot_with_sddev(
        df = summary_all_runs,
        x = "ALA",
        y = summary_all_runs$mean.length,
        y_dev = summary_all_runs$sd.length,
        fill = "ALA"
    ) +
        labs(title = "Treatable Length Across Runs", x = "ALA presence", y = "Treatable Length (mm)")

    p_all <- p_len_deviation_all + p_dia_deviation_all + plot_annotation(tag_levels = "I")
    # print(p_all)
    ggsave(sprintf("%s/outcome_deviation_both.jpeg", image_folder), p_all)

    ## Plot: Diameter & length deviation, per run
    summary <- ala_exp_data %>%
        group_by(ALA, Run) %>%
        dplyr::summarize(
            mean.length = mean(ResultTreatableLength, na.rm = TRUE),
            mean.diameter = mean(ResultMinTreatableDia, na.rm = TRUE),
            sd.length = sd(ResultTreatableLength, na.rm = TRUE),
            sd.diameter = sd(ResultMinTreatableDia, na.rm = TRUE)
        )

    p_length_deviation <- bar_plot_with_sddev(
        df = summary,
        x = "Run",
        y = summary$mean.length,
        y_dev = summary$sd.length,
        fill = "ALA"
    ) +
        labs(title = "Treatable length with & without ALA", x = "Run ID", y = "Treatable Length (mm)")

    p_dia_deviation <- bar_plot_with_sddev(
        df = summary,
        x = "Run",
        y = summary$mean.diameter,
        y_dev = summary$sd.diameter,
        fill = "ALA"
    ) +
        labs(title = "Minimum diameter with & without ALA", x = "Run ID", y = "Minimum Diameter (mm)")

    title <- "Average & Std. Dev for Stent Results, All Runs"
    p_deviation_len_dia <- p_length_deviation + p_dia_deviation + plot_annotation(tag_levels = "I")
    # print(p_deviation_len_dia)
    ggsave(sprintf("%s/outcome_deviation_both_all_runs.jpeg", image_folder), p_deviation_len_dia, width=9, height=4, dpi=300)

    ## Plot: Diameter & length deviation, per participant
    summary_tester <- ala_exp_data %>%
        group_by(ALA, Tester) %>%
        dplyr::summarize(
            mean.length = mean(ResultTreatableLength, na.rm = TRUE),
            mean.diameter = mean(ResultMinTreatableDia, na.rm = TRUE),
            sd.length = sd(ResultTreatableLength, na.rm = TRUE),
            sd.diameter = sd(ResultMinTreatableDia, na.rm = TRUE)
        )

    tester_map = c("Tester1" = "1", "Tester2" = "2", "Tester3" = "3", "Tester4" = "4", 
                "Tester5" = "5", "Tester6" = "6", "Tester7" = "7", "Tester8" = "8", "Tester9" = "9")
    p_length_deviation_tester <- bar_plot_with_sddev(
        df = summary_tester,
        x = "Tester",
        y = summary_tester$mean.length,
        y_dev = summary_tester$sd.length,
        fill = "ALA"
    ) +
        labs(title = "Treatable length with & without ALA", x = "Tester", y = "Treatable Length (mm)") + 
        scale_x_discrete(labels=tester_map)

    p_dia_deviation_tester <- bar_plot_with_sddev(
        df = summary_tester,
        x = "Tester",
        y = summary_tester$mean.diameter,
        y_dev = summary_tester$sd.diameter,
        fill = "ALA"
    ) +
        labs(title = "Minimum diameter with & without ALA", x = "Tester", y = "Minimum Diameter (mm)") +
        scale_x_discrete(labels=tester_map)

    title <- "Average & Std. Dev for Stent Results Per Tester"
    all_tester <- p_length_deviation_tester + p_dia_deviation_tester + plot_annotation(tag_levels = "I")
    p_deviation_len_dia_tester <- all_tester
    # print(p_deviation_len_dia_tester)
    ggsave(sprintf("%s/outcome_deviation_both_all_testers.jpeg", image_folder), p_deviation_len_dia_tester, width=9, height=4, dpi=300)

    ## Check normality
    # Plot distribution and Q-Q plot
    len_hist <- ggplot(ala_exp_data, aes(ResultTreatableLength, fill = ALA)) +
        geom_histogram(alpha = 0.8, position = "identity", bins = 20) +
        labs(
            title = "Distribution of Length Results (ALA vs Non ALA)",
            x = "Treatable Length (mm)"
        ) +
        theme_classic()
    dia_hist <- ggplot(ala_exp_data, aes(ResultMinTreatableDia, fill = ALA)) +
        geom_histogram(alpha = 0.8, position = "identity") +
        labs(
            title = "Distribution of Diameter Results (ALA vs Non ALA)",
            x = "Minimum Diameter (mm)"
        ) +
        theme_classic()
    qq_len <- ggqqplot(ala_exp_data$ResultTreatableLength) + theme_classic() + labs(title = "Q-Q plot for length results")
    qq_dia <- ggqqplot(ala_exp_data$ResultMinTreatableDia) + theme_classic() + labs(title = "Q-Q plot for diameter results")
    length_dia_all <- len_hist + dia_hist + qq_len + qq_dia + plot_annotation()
    # print(length_dia_all)
    ggsave(sprintf("%s/normality_length_diameter.jpeg", image_folder), length_dia_all)

    # Run Shapiro-Wilk test for normality
    shapiro.length <- shapiro.test(ala_exp_data$ResultTreatableLength)
    shapiro.dia <- shapiro.test(ala_exp_data$ResultMinTreatableDia)
    print(sprintf("Shapiro tests: P=%g for len, P=%g for dia", shapiro.length$p.value, shapiro.dia$p.value))

    # Test: are the means different?
    ttest.length <- t.test(ResultTreatableLength ~ ALA, data = ala_exp_data)
    wtest.dia <- wilcox.test(ResultMinTreatableDia ~ ALA, data = ala_exp_data)

    print(sprintf("t test for length: P=%g; wilcox for dia: P=%g", ttest.length$p.value, wtest.dia$p.value))

    # Test: are the variances different?
    ftest.length <- var.test(ResultTreatableLength ~ ALA, data = ala_exp_data)
    ltest.diameter <- car::leveneTest(ResultMinTreatableDia ~ ALA, data = ala_exp_data)

    print(sprintf("F test for length: P=%g; Levene for dia: P=%g", ftest.length$p.value, ltest.diameter$Pr))
}
