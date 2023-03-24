"
Plotting utilities and functions
"
# ggplot2 colors
plot_red <- "#f8766d"
plot_blue <- "#83b0fc"

## Function to generate a plot that shows mean
# and standard deviation for each group via error bars
bar_plot_with_sddev <- function(df, x, y, y_dev, fill) {
    error_min <- as.numeric(y) - as.numeric(y_dev)
    error_max <- as.numeric(y) + as.numeric(y_dev)
    p <- ggplot(data = df, aes_string(x = x, y = y, fill = fill)) +
        geom_bar(
            stat = "identity", color = "black",
            position = position_dodge()
        ) +
        geom_errorbar(aes(ymin = error_min, ymax = error_max),
            width = .2,
            position = position_dodge(.9)
        ) + 
        theme_classic()

    return(p)
}

## Get data ellipse for Mahalanobis analysis
get_data_ellipse <- function(df, center, cov) {
    # Use square root of chi-square distribution for ellipse radius
    rad <- sqrt(qchisq(p = 0.95, df = ncol(df)))

    # Calculate ellipse shape
    ellipse_df <- car::ellipse(center = center, shape = cov, radius = rad,
                                segments = 150, draw = FALSE)
    ellipse_df <- as.data.frame(ellipse_df)
    colnames(ellipse_df) <- colnames(df)

    return(ellipse_df)
}

## Function to generate a faceted stripplot plot for each group of testers
create_stripplot <- function(df, y, type, group, nrows) {
    # type = "length" or "diameter"
    # group = "A" or "B"
    ylab <- if(type=="diameter") "Minimum Treatable Diameter (mm)" else "Treatable Length (mm)"
    title <- sprintf("Tester Group %s: ALA vs non-ALA %s results for all pairs", group, type)
    
    p <- ggplot(df, aes(ALA, {{y}}, color = ALA)) +
        geom_point(size = 3) +
        facet_wrap(~Pair, nrow = nrows) +
        labs(
            title = title,
            x = "ALA vs non ALA results per pair",
            y = ylab
        ) +
        theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    return(p)
}

## Function to calculate the mean and the standard deviation for each group
# Source: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
data_summary <- function(data, varname, groupnames) {
    summary_func <- function(x, col) {
        c(
            mean = mean(x[[col]], na.rm = TRUE),
            sd = sd(x[[col]], na.rm = TRUE)
        )
    }
    data_sum <- ddply(data, groupnames,
        .fun = summary_func,
        varname
    )
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
}