# ANOVA analysis in R using mtcars
# Quantitative variable: mpg
# Factors: am (2 levels), gear (3 levels)

options(repos = c(CRAN = "https://cloud.r-project.org"))

ensure_packages <- function(pkgs) {
  to_install <- pkgs[!(pkgs %in% rownames(installed.packages()))]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c("ggplot2", "dplyr", "car"))

plots_dir <- file.path("plots")
results_dir <- file.path("results")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# Data prep
library(dplyr)

df <- as_tibble(mtcars) %>%
  mutate(
    am = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual")),
    gear = factor(gear, levels = c(3, 4, 5), labels = c("3-gears", "4-gears", "5-gears"))
  )

# 1) Visualizations
library(ggplot2)

p1 <- ggplot(df, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.color = "#333333") +
  geom_jitter(width = 0.1, alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "MPG by Transmission (am)", x = "Transmission", y = "MPG") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")

ggsave(filename = file.path(plots_dir, "boxplot_mpg_by_am.png"), plot = p1, width = 7, height = 5, dpi = 150)

p2 <- ggplot(df, aes(x = gear, y = mpg, fill = gear)) +
  geom_boxplot(width = 0.6, alpha = 0.8, outlier.color = "#333333") +
  geom_jitter(width = 0.1, alpha = 0.7) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "MPG by Gear count", x = "Gears", y = "MPG") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")

ggsave(filename = file.path(plots_dir, "boxplot_mpg_by_gear.png"), plot = p2, width = 7, height = 5, dpi = 150)

# Interaction plot (means)
mean_df <- df %>% group_by(gear, am) %>% summarise(mean_mpg = mean(mpg), .groups = "drop")

p3 <- ggplot(mean_df, aes(x = gear, y = mean_mpg, color = am, group = am)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Interaction: MPG by Transmission across Gears", x = "Gears", y = "Mean MPG", color = "Transmission") +
  theme_minimal(base_size = 13)

ggsave(filename = file.path(plots_dir, "interaction_plot.png"), plot = p3, width = 7, height = 5, dpi = 150)

# Helper to capture output to file
write_out <- function(path, expr) {
  con <- file(path, open = "wt")
  sink(con)
  on.exit({ sink(); close(con) }, add = TRUE)
  cat(sprintf("Generated: %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  force(expr)
}

# 2) Assumption checks and ANOVAs

# One-way ANOVA: mpg ~ am
m_am <- aov(mpg ~ am, data = df)
res_am <- residuals(m_am)

write_out(file.path(results_dir, "one_way_am.txt"), {
  cat("One-way ANOVA: mpg ~ am\n\n")
  print(summary(m_am))
})

write_out(file.path(results_dir, "assumptions_am.txt"), {
  cat("Assumption checks for mpg ~ am\n\n")
  cat("Shapiro-Wilk test for residual normality:\n")
  print(shapiro.test(res_am))
  cat("\nLevene's test for homogeneity of variances:\n")
  print(car::leveneTest(mpg ~ am, data = df))
})

# Diagnostics plots for am model
png(file.path(plots_dir, "diagnostics_am.png"), width = 900, height = 900)
par(mfrow = c(2, 2))
plot(m_am)
dev.off()

# One-way ANOVA: mpg ~ gear
m_gear <- aov(mpg ~ gear, data = df)
res_gear <- residuals(m_gear)

write_out(file.path(results_dir, "one_way_gear.txt"), {
  cat("One-way ANOVA: mpg ~ gear\n\n")
  print(summary(m_gear))
})

write_out(file.path(results_dir, "assumptions_gear.txt"), {
  cat("Assumption checks for mpg ~ gear\n\n")
  cat("Shapiro-Wilk test for residual normality:\n")
  print(shapiro.test(res_gear))
  cat("\nLevene's test for homogeneity of variances:\n")
  print(car::leveneTest(mpg ~ gear, data = df))
})

# Diagnostics plots for gear model
png(file.path(plots_dir, "diagnostics_gear.png"), width = 900, height = 900)
par(mfrow = c(2, 2))
plot(m_gear)
dev.off()

# Tukey HSD for gear (3 levels)
write_out(file.path(results_dir, "tukey_gear.txt"), {
  cat("Tukey HSD pairwise comparisons for 'gear'\n\n")
  print(TukeyHSD(m_gear, "gear"))
})

# Two-way ANOVAs
m_add <- aov(mpg ~ am + gear, data = df)        # without interaction
m_int <- aov(mpg ~ am * gear, data = df)        # with interaction

write_out(file.path(results_dir, "two_way_no_interaction.txt"), {
  cat("Two-way ANOVA: mpg ~ am + gear (no interaction)\n\n")
  print(summary(m_add))
})

write_out(file.path(results_dir, "two_way_with_interaction.txt"), {
  cat("Two-way ANOVA: mpg ~ am * gear (with interaction)\n\n")
  print(summary(m_int))
})

# Compare models (test for interaction)
write_out(file.path(results_dir, "model_comparison.txt"), {
  cat("Model comparison: additive vs interaction\n\n")
  print(anova(m_add, m_int))
})

# Brief conclusions written to summary.txt
write_out(file.path(results_dir, "summary.txt"), {
  cat("Dataset: mtcars. Response: mpg. Factors: am (2 levels), gear (3 levels).\n\n")
  cat("Visual inspection suggests Manual transmissions have higher MPG; 4- and 5-gear groups show higher MPG than 3-gear.\n\n")
  cat("One-way ANOVA results:\n")
  s1 <- summary(m_am)[[1]]
  s2 <- summary(m_gear)[[1]]
  p_am <- s1[1, "Pr(>F)"]
  p_gear <- s2[1, "Pr(>F)"]
  cat(sprintf("  - mpg ~ am: p = %.4f (%s differences).\n", p_am, ifelse(p_am < 0.05, "significant", "not significant")))
  cat(sprintf("  - mpg ~ gear: p = %.4f (%s differences).\n\n", p_gear, ifelse(p_gear < 0.05, "significant", "not significant")))
  cat("Tukey HSD for 'gear' saved to tukey_gear.txt.\n\n")
  cat("Two-way ANOVA:\n")
  s_add <- summary(m_add)[[1]]
  s_int <- summary(m_int)[[1]]
  p_am2 <- s_add["am", "Pr(>F)"]
  p_gear2 <- s_add["gear", "Pr(>F)"]
  cat(sprintf("  - Additive model: am p = %.4f, gear p = %.4f.\n", p_am2, p_gear2))
  comp <- anova(m_add, m_int)
  p_interaction <- comp$`Pr(>F)`[2]
  cat(sprintf("  - Interaction test: p = %.4f (%s).\n\n", p_interaction, ifelse(p_interaction < 0.05, "interaction significant", "no evidence of interaction")))
  cat("See plots/ for figures and results/ for detailed outputs.\n")
})

cat("Analysis complete. Files written to 'plots/' and 'results/'.\n")
