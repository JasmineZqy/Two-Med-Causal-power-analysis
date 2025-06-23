library(mvtnorm)
library(tidyverse)
library(glue)
library(parallel)
library(ggplot2)
library(dplyr)
library("PowerMediators")

runSampsize <- function(
        n = 100,
        steps = 20,
        TarPow = 0.8,
        max_n = 1000,
        sig.adjust = c("no_adjust", "bonferroni", "modified_bon1", "modified_bon2"),
        mediation = c("IIE_M1", "IIE_M2"),
        power = c("familywise", "per-test"),
        plot = TRUE,
        verbose = TRUE,
        ...
) {
    power <- match.arg(power)  # validate power argument

    nstart <- n
    power_df <- data.frame()
    plot_obj <- NULL

    repeat {
        if (verbose) message("Running n = ", n)

        df <- runPower(n = n, ...)
        df <- df[df$sig.adjust %in% sig.adjust & df$mediation %in% mediation, ]
        df <- unique(df)
        power_df <- bind_rows(power_df, df)

        power_col <- if (power == "familywise") "power_FW" else "power_PT"

        if (nrow(df) > 0 && all(df[[power_col]] >= TarPow)) break
        n <- n + steps

        if (n >= max_n) {
            message("Target power not achieved before reaching max sample size.")
            break
        }


    }

    # Create result table
    if (power == "familywise") {
        power_target_table <- df[, c("mediation", "sig.adjust", "n", "power_FW")]
        colnames(power_target_table) <- c("Mediation", "SigAdjustment", "TargetSampleSize", "AchievedPower")
    } else {
        power_target_table <- df[, c("mediation", "effect", "sig.adjust", "n", "power_PT")]
        colnames(power_target_table) <- c("Mediation", "Effect", "SigAdjustment", "TargetSampleSize", "AchievedPower")
    }
    power_target_table <- unique(power_target_table)


    # Create plot
    if (plot) {
        base_plot <- ggplot2::ggplot(power_df,
                                     aes(x = n,
                                         y = if (power == "familywise") power_FW else power_PT,
                                         color = if (power == "familywise") mediation else effect,
                                         shape = sig.adjust))

        plot_obj <- base_plot +
            # geom_smooth(method = "loess", se = FALSE, linewidth = 0.5) +
            geom_point(size = 2) +
            geom_line( linewidth = 0.5) +
            geom_hline(yintercept = TarPow, linetype = "dotted", color = "red") +
            annotate("text", x = nstart, y = TarPow, label = paste0("Target Power = ", TarPow),
                     vjust = -0.5, hjust = 0, color = "red",size = 3.5,fontface = "italic") +
            scale_x_continuous(breaks = seq(nstart, n, by = steps)) +
            # scale_linetype_manual(values = c(
            #     "no_adjust" = "solid", "bonferroni" = "dashed",
            #     "modified_bon1" = "dotdash", "modified_bon2" = "longdash"
            # )) +
            scale_shape_manual(
                values = c(
                    "no_adjust" = 4,         # cross
                    "bonferroni" = 16,       # circle
                    "modified_bon1" = 17,    # triangle
                    "modified_bon2" = 15     # square
                ),
                labels = c(
                    "no_adjust" = "No Adjustment",
                    "bonferroni" = "Bonferroni",
                    "modified_bon1" = "Modified Bonferroni I",
                    "modified_bon2" = "Modified Bonferroni II"
                ),
                name = "Adjustment"
            ) +
            labs(
                title =  "Per-test Power Curve",
                x = "Sample Size",
                y = "Power",
                color = if (power == "familywise") "Mediation" else "Effect",
                linetype = "Adjustment"
            ) +
            {if (power == "per-test") facet_wrap(~ mediation) else NULL} +
            theme_bw(base_size = 12) +
            theme(
                panel.grid.major = element_line(color = "grey85", size = 0.3),
                panel.grid.minor = element_blank(),
                axis.text = element_text(color = "black"),
                axis.title = element_text(face = "bold"),
                plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                legend.position = "right",
                legend.key = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(face = "bold")
            )
    }

    return(list(
        power_df = power_df,
        target_power_table = power_target_table,
        plot = plot_obj
    ))
}

# out <- runSampsize(
#    seed_user = 1234,
#     n = 50,
#     steps = 20,
#     TarPow = 0.8,
#     max_n = 200,
#     sig.adjust = c("no_adjust", "modified_bon1"),
#     mediation = c("IIE_M1","IIE_M2"),
#     power = c("per-test"),
#     num_x = 2,
#     treat.prop = 0.3,
#     treat.randomized = FALSE,
#     a_on_x = sqrt(0.03),
#     m_on_a = rep(0.5, 2),
#     m_on_x = rep(sqrt(0.13), 2),
#     em_corr = 0.03,
#     M_binary = c(TRUE, FALSE),
#     y_on_a = 0.01,
#     y_on_m = rep(0.5, 2),
#     y_on_am_2way = rep(0.1, 2),
#     y_on_m_2way = rep(0.1, 1),
#     y_on_am_3way = rep(0.03, 1),
#     y_on_x = sqrt(0.02),
#     Y_binary = FALSE,
#     nboot = 1000,
#     n.draws = 1000,
#     sig.level = 0.05,
#     nsims = 5,
#     mc.cores = 1
# )
#
# out$plot
# out$target_power_table

