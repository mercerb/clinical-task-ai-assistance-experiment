"
Use duration as a measure of complexity

Use the average time spent without ALA for each run as a proxy for 
the 'complexity' of that run (longer times -> more complex)

Then compare to time savings due to ALA or increased consistency due to ALA
"

run_time_complexity_analysis <- function(ala_exp_data) {

    time_per_run <- ala_exp_data %>%
        group_by(Run, ALA) %>%
        dplyr::summarize(seconds = mean(TotalSeconds))
    time_per_run <- time_per_run %>%
        tidyr::spread(ALA, seconds) %>%
        dplyr::rename(
            "ala_seconds" = `TRUE`,
            "non_ala_seconds" = `FALSE`
        )
    time_per_run$ala_time_savings <- time_per_run$non_ala_seconds - time_per_run$ala_seconds
    ala_exp_data <- merge(ala_exp_data, time_per_run, by = "Run")
    summary(ala_exp_data$non_ala_seconds)

    # validate use of time as complexity metric. 
    # Based on anecdotal feedback about the run selection, this is the expection:
    # the runs in pairs 1 & 5 are the most complex; 2 & 4 are medium; 3 is easiest
    non_ala_duration_by_pair <- dplyr::filter(ala_exp_data, ALA == FALSE) %>%
        group_by(Pair) %>%
        dplyr::summarize(non_ala_seconds = mean(TotalSeconds)) %>%
        ggplot(aes(x = Pair, y = non_ala_seconds, fill = Pair)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_classic() +
        scale_fill_grey(start = .8, end = .2) +
        labs(
        # title = "Time spent (non ALA) as 'complexity' for each pair",
            x = "Pair",
            y = "Average duration (seconds) spent on non-ALA runs"
        )

    ggsave("images_normal/non_ala_duration_by_pair.jpeg",
        non_ala_duration_by_pair,
        width = 5.5, height = 5, dpi = 300
    )
    # Doesn't match the expectation - but still interesting.
    # Assume for now this is true complexity.

    # Now, relate time savings due to ALA against the 'complexity' seconds
    # Result: very slight trend of increased time savings with increased complexity
    time_savings_by_complexity_plot <- time_per_run %>%
        ggplot(aes(x = non_ala_seconds, y = ala_time_savings)) +
        theme_classic() +
        geom_line(color = "blue") +
        geom_smooth(method = "lm") +
        labs(
            # title = "Time saved by ALA as a function of 'complexity'",
            x = "Average duration (seconds) spent on non-ALA runs",
            y = "Average duration (seconds) saved on ALA runs"
        )
    print(time_savings_by_complexity_plot)
    ggsave("images_normal/time_savings_by_complexity_plot.jpeg",
        time_savings_by_complexity_plot,
        width = 5.5, height = 5, dpi = 300
    )

    title_both <- "Average time spent without ALA as a proxy for complexity"
    p_all <- non_ala_duration_by_pair + time_savings_by_complexity_plot + plot_annotation(tag_levels = "I")
    print(p_all)
    ggsave("images_normal/complexity_both.jpeg", p_all, width = 12, height = 8, dpi = 200)

    # Next, relate change in SD for length due to ALA against the 'complexity' seconds
    # Result: homoscedastic - the SDs are pretty much the same regardless of complexity
    # Esp looking by run. By pair, seems like ALA's impact is strongest with easier runs
    length_sd_per_run <- ala_exp_data %>%
        group_by(Run, ALA) %>%
        dplyr::summarize(length_sd = sd(ResultTreatableLength))
    length_sd_per_run <- length_sd_per_run %>% tidyr::spread(ALA, length_sd)
    length_sd_per_run$ala_diff <- length_sd_per_run$"FALSE" - length_sd_per_run$"TRUE"
    length_sd_per_run <- merge(length_sd_per_run, time_per_run, by = "Run")
    length_sd_change_by_run <- ggplot(data = subset(length_sd_per_run, !is.na(ala_diff)), aes(x = non_ala_seconds, y = ala_diff)) +
        geom_line() +
        geom_smooth(method = "lm") +
        labs(title = "Variation (SD) per run as a function of complexity", x = "Time spent per run (proxy of complexity)", y = "Std dev of length") +
        theme_classic()

    length_sd_per_run$Pair <- substr(length_sd_per_run$Run, 0, 1)
    length_sd_per_pair <- length_sd_per_run %>%
        dplyr::group_by(Pair) %>%
        dplyr::summarize(ala_diff = mean(ala_diff), non_ala_seconds = mean(non_ala_seconds))
    length_sd_change_by_pair <- ggplot(data = subset(length_sd_per_pair, !is.na(ala_diff)), aes(x = non_ala_seconds, y = ala_diff)) +
        geom_line() +
        geom_smooth(method = "lm") +
        labs(title = "Variation (SD) per pair as a function of complexity", x = "Time spent per run (proxy of complexity)", y = "Std dev of length") +
        theme_classic()

    length_sd_change_plot <- length_sd_change_by_run + length_sd_change_by_pair
    print(length_sd_change_plot)
}