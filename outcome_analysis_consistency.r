"
The outcome analysis seeks to answer 3 questions:
1) Does ALA have a meaningful impact on stent length or diameter?
2) Do users of ALA have more consistency?
3) Does ALA get results closer to the 'ideal' outcome? (where ideal = what actually happened at the CRF)

Here, focus on Question 2.
"

run_outcome_consistency <- function(ala_exp_data, image_folder) {
    ## High-level summary of consistency (std deviation) & diff from outcome
    summary <- ala_exp_data %>%
        dplyr::group_by(Run, ALA) %>%
        dplyr::summarize(
            "Length" = round(mean(ResultTreatableLength, na.rm = TRUE), 2),
            "Length (SD)" = round(sd(ResultTreatableLength, na.rm = TRUE), 2),
            "Dia" = round(mean(ResultMinTreatableDia, na.rm = TRUE), 2),
            "Dia (SD)" = round(sd(ResultMinTreatableDia, na.rm = TRUE), 2)
        )
    print(summary)

    avg_sds <- summary %>%
        dplyr::group_by(ALA) %>%
        dplyr::summarize(
            "length_mean_sd" = round(mean("Length (SD)", na.rm = TRUE), 2),
            "dia_mean_sd" = round(mean("Dia (SD)", na.rm = TRUE), 2)
        )
    print(avg_sds)

    ## Investigate runs.
    # For a given run, are the AI users different than the non-AI users?
    # Visualize stent outcomes for each run, ALA vs non-ALA.
    # Each dot represents (length, diameter) pair
    p <- ggplot(data = ala_exp_data, aes(ResultTreatableLength, ResultMinTreatableDia, color = ALA)) +
        geom_point(size = 4) +
        stat_ellipse(type = "norm") +
        theme_classic() +
        labs(y = "Stent Diameter (mm)", x = "Stent Length (mm)") +
        facet_wrap(~Run, ncol = 2)

    # Add table with average and standard deviation details
    summary %>% ungroup()

    table <- lapply(split(summary, summary$Run), "[", -1)
    run_df <- tibble(
        x = rep(-Inf, length(table)),
        y = rep(Inf, length(table)),
        Run = levels(summary$Run),
        tbl = table
    )

    p <- p + geom_table(
        data = run_df, aes(x = x, y = y, label = tbl),
        hjust = 0, vjust = 1
    )
    ggsave(sprintf("%s/stent_outcomes_per_run.jpeg", image_folder), width = 9, height = 14, units = "in", p)

    # option to add actual answer as green dots - confusing, but interesting
    # p_with_outcomes <- p + geom_point(data = actual_outcomes, color="green", size=6)

    ## Calculate consistency for each pair-ALA group.
    # F-tests (for length) & Levene tests (for diameter) for each run.
    # By setting alternative = "greater", we test against the assumption that ALA leads to greater variance
    # If p<0.05, then results with ALA have significantly less variance
    for (run in levels(ala_exp_data$Run)) {
        print(sprintf("\n---- F/Levene test for length & diameter of run %s ----", run))
        run_data <- filter(ala_exp_data, (ala_exp_data$Run == run))
        print(sprintf("F test two-sided for length: p-value = %f", var.test(ResultTreatableLength ~ ALA, data = run_data)$p.value))
        print(sprintf("F test greater for length: p-value = %f", var.test(ResultTreatableLength ~ ALA, data = run_data, alternative = "greater")$p.value))
        print(sprintf("F test less for length: p-value = %f", var.test(ResultTreatableLength ~ ALA, data = run_data, alternative = "less")$p.value))
        if (run == "1A" || run == "2A") next

        print(sprintf("Levene's test two-sided for dia: p-value = %f", car::leveneTest(ResultMinTreatableDia ~ ALA, data = run_data)$Pr))
        print(sprintf("Levene's test greater for dia: p-value = %f", car::leveneTest(ResultMinTreatableDia ~ ALA, data = run_data, alternative = "greater")$Pr))
        print(sprintf("Levene's test less for dia: p-value = %f", car::leveneTest(ResultMinTreatableDia ~ ALA, data = run_data, alternative = "less")$Pr))
    }

    ## Investigate testers.
    # For a given group of testers, do they converge more with AI than without?
    # There are two groups: "A" and "B" testers (those who saw "A" runs with vs without ALA)
    a_testers <- c("Tester1", "Tester2", "Tester3", "Tester4", "Tester7")
    # how to automatically sort out the testers who have "A" and "ALA=TRUE", probably easy programatically
    tester_group_a <- subset(ala_exp_data, (Tester %in% a_testers))
    tester_group_b <- subset(ala_exp_data, !(Tester %in% a_testers))

    # group A stripplot
    group_a_len <- create_stripplot(tester_group_a, ResultTreatableLength, "length", "A", 1)
    group_a_dia <- create_stripplot(tester_group_a, ResultMinTreatableDia, "diameter", "A", 1)
    title <- "Group A Results: Consistency ALA vs non ALA"
    group_a <- (group_a_len / group_a_dia) + plot_annotation(tag_levels = "I")
    ggsave(sprintf("%s/consistency_group_a.png", image_folder), group_a, width = 7.5, height = 6, dpi = 300)

    # group A stats
    group_a_stats <- tester_group_a %>%
        dplyr::group_by(ALA, Run) %>%
        dplyr::summarize(
            "length_mean" = mean(ResultTreatableLength, na.rm = TRUE), 2,
            "dia_mean" = mean(ResultMinTreatableDia, na.rm = TRUE), 2,
            "length_sd" = sd(ResultTreatableLength, na.rm = TRUE), 2,
            "dia_sd" = sd(ResultMinTreatableDia, na.rm = TRUE), 2
        )

    avg_run_std_dev_a <- group_a_stats %>%
        dplyr::group_by(ALA) %>%
        dplyr::summarize(
            "length_mean_sd" = round(mean(length_sd, na.rm = TRUE), 2),
            "dia_mean_sd" = round(mean(dia_sd, na.rm = TRUE), 2)
        )
    print(avg_run_std_dev_a)
    # Calculate the percent improvement in consistency due to ALA for length & diameter (A)
    pct_improvement_len_a <- (avg_run_std_dev_a$length_mean_sd[2] - avg_run_std_dev_a$length_mean_sd[1]) / avg_run_std_dev_a$length_mean_sd[1]
    pct_improvement_dia_a <- (avg_run_std_dev_a$dia_mean_sd[2] - avg_run_std_dev_a$dia_mean_sd[1]) / avg_run_std_dev_a$dia_mean_sd[1]
    print(sprintf("group A pct improvement.  len: %f, dia: %f", pct_improvement_len_a, pct_improvement_dia_a))
    
    # GROUP A variance tests
    for (pair in levels(tester_group_a$Pair)) {
        print(sprintf("\n---- Group A: F/Levene test for length & diameter of pair %s ----", pair))
        pair_data <- filter(tester_group_a, (tester_group_a$Pair == pair))

        print(sprintf("F test greater for length: p-value = %f", var.test(ResultTreatableLength ~ ALA, data = pair_data, alternative = "greater")$p.value))
        print(sprintf("Levene's test two-sided for dia: p-value = %f", car::leveneTest(ResultMinTreatableDia ~ ALA, data = pair_data)$Pr))
    }

    # group B stripplot
    group_b_len <- create_stripplot(tester_group_b, ResultTreatableLength, "length", "B", 1)
    group_b_dia <- create_stripplot(tester_group_b, ResultMinTreatableDia, "diameter", "B", 1)
    title <- "Group B Results: Consistency ALA vs non ALA"
    group_b <- (group_b_len / group_b_dia) + plot_annotation(tag_levels = "I")
    ggsave(sprintf("%s/consistency_group_b.png", image_folder), group_b, width = 7.5, height = 6, dpi = 300)

    # group B stats
    group_b_stats <- tester_group_b %>%
        dplyr::group_by(ALA, Run) %>%
        dplyr::summarize(
            "length_mean" = mean(ResultTreatableLength, na.rm = TRUE), 2,
            "dia_mean" = mean(ResultMinTreatableDia, na.rm = TRUE), 2,
            "length_sd" = sd(ResultTreatableLength, na.rm = TRUE), 2,
            "dia_sd" = sd(ResultMinTreatableDia, na.rm = TRUE), 2
        )

    avg_run_std_dev_b <- group_b_stats %>%
        dplyr::group_by(ALA) %>%
        dplyr::summarize(
            "length_mean_sd" = round(mean(length_sd, na.rm = TRUE), 2),
            "dia_mean_sd" = round(mean(dia_sd, na.rm = TRUE), 2)
        )
    print(avg_run_std_dev_b)
    # Calculate the percent improvement in consistency due to ALA for length & diameter (B)
    pct_improvement_len_b <- (avg_run_std_dev_b$length_mean_sd[2] - avg_run_std_dev_b$length_mean_sd[1]) / avg_run_std_dev_b$length_mean_sd[1]
    pct_improvement_dia_b <- (avg_run_std_dev_b$dia_mean_sd[2] - avg_run_std_dev_b$dia_mean_sd[1]) / avg_run_std_dev_b$dia_mean_sd[1]
    print(sprintf("group B pct improvement.  len: %f, dia: %f", pct_improvement_len_b, pct_improvement_dia_b))
   
   # GROUP B variance tests
    for (pair in levels(tester_group_b$Pair)) {
        print(sprintf("\n---- Group B: F/Levene test for length & diameter of pair %s ----", pair))
        pair_data <- filter(tester_group_b, (tester_group_b$Pair == pair))

        print(sprintf("F test greater for length: p-value = %f", var.test(ResultTreatableLength ~ ALA, data = pair_data, alternative = "greater")$p.value))
        print(sprintf("Levene's test two-sided for dia: p-value = %f", car::leveneTest(ResultMinTreatableDia ~ ALA, data = pair_data)$Pr))
    }
}
