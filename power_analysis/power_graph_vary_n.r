"
A priori power analysis based on the data from an initial pilot tester
(not included in final experiment results).
"
data <- read.csv("power_analysis/first_tester_data.csv")
data_no_outliers <- dplyr::filter(data, outlier == FALSE)

get_power <- function(samples, diff_mean, pooled_sd, alpha) {
    cohens_d <- diff_mean / pooled_sd # effect size

    # always assume "greater" as the hypothesis when doing no ALA minus ALA
    pwr.result <- pwr.t.test(
        n = samples,
        d = cohens_d,
        sig.level = alpha,
        type = "paired",
        alternative = "greater"
    )
    power_percentage <- 100 * pwr.result$power
    return(power_percentage)
}

get_n_for_80_power <- function(diff_mean, pooled_sd, alpha) {
    cohens_d <- diff_mean / pooled_sd # effect size
    pwr.result <- pwr.t.test(
        power = 0.80,
        d = cohens_d,
        sig.level = alpha,
        type = "paired",
        alternative = "greater"
    )
    return(pwr.result$n)
}

# Make plot for all data (with outliers)
diff_mean <- abs(mean(data$non_ala_seconds - data$ala_seconds))
pooled_sd <- sqrt((sd(data$non_ala_seconds)^2 + sd(data$ala_seconds)^2) / 2)
n_for_80_05 <- get_n_for_80_power(diff_mean, pooled_sd, 0.05)
n_for_80_10 <- get_n_for_80_power(diff_mean, pooled_sd, 0.10)
print(sprintf("Including outliers, samples required for 80%% power: %g (alpha = 0.05) or %g (alpha = 0.10)", n_for_80_05, n_for_80_10))

all_data <- ggplot(data.frame(n = c(2, 700)), aes(x = n)) +
    theme_classic() +
    ylab("Power level (>=80% is good)") + ylim(0, 100) +
    xlab("Number of samples (n)") +
    geom_ribbon(aes(ymin = 80, ymax = 100), alpha = 0.25, fill = "green") +
    stat_function(fun = get_power, args = list(diff_mean, pooled_sd, 0.05), aes(color = "a = 0.05")) +
    stat_function(fun = get_power, args = list(diff_mean, pooled_sd, 0.10), aes(color = "a = 0.10")) +
    scale_color_manual("Alpha Value", values = c("a = 0.05" = "blue", "a = 0.10" = "red"))

# Make plot for trimmed data (without outliers)
diff_mean <- abs(mean(data_no_outliers$non_ala_seconds - data_no_outliers$ala_seconds))
pooled_sd <- sqrt((sd(data_no_outliers$non_ala_seconds)^2 + sd(data_no_outliers$ala_seconds)^2) / 2)
n_for_80_05 <- get_n_for_80_power(diff_mean, pooled_sd, 0.05)
n_for_80_10 <- get_n_for_80_power(diff_mean, pooled_sd, 0.10)
print(sprintf("Without outliers, samples required for 80%% power: %g (alpha = 0.05) or %g (alpha = 0.10)", n_for_80_05, n_for_80_10))

no_outliers <- ggplot(data.frame(n = c(2, 25)), aes(x = n)) +
    theme_classic() +
    ylab("Power level (>=80% is good)") + ylim(0, 100) +
    xlab("Number of samples (n)") +
    geom_ribbon(aes(ymin = 80, ymax = 100), alpha = 0.25, fill = "green") +
    stat_function(fun = get_power, args = list(diff_mean, pooled_sd, 0.05), aes(color = "a = 0.05")) +
    stat_function(fun = get_power, args = list(diff_mean, pooled_sd, 0.10), aes(color = "a = 0.10")) +
    scale_color_manual("Alpha Value", values = c("a = 0.05" = "blue", "a = 0.10" = "red"))

# Combine plots
# title <- "Initial Power Analysis with all data (left) and outliers removed (right)"
p <- all_data + no_outliers & theme(legend.position = "bottom")
p <- p + plot_annotation(tag_levels = "I") + plot_layout(guides = "collect")
print(p)
ggsave("power_analysis/initial_graphs.png", p, width = 6, height = 4, dpi = 300)
