library(tidyverse)
library(rstan)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(patchwork)
rstan_options (auto_write = TRUE)
options(mc.cores = parallel::detectCores(logical = FALSE)) # use all available cores by default when sampling

df_foi_high_uk = readRDS(file="foi_baseline_high_UK.Rda")
df_foi_low_uk = readRDS(file="foi_baseline_low_UK.Rda")
df_foi_high_sg = readRDS(file="foi_baseline_high_fixed_main.Rda")
df_foi_low_sg = readRDS(file="foi_baseline_low_fixed_main.Rda")

df_foi_high_uk$scenario <- "England (High-Risk)"
df_foi_low_uk$scenario <- "England (Low-Risk)"
df_foi_high_sg$scenario <- "Singapore (High-Risk)"
df_foi_low_sg$scenario <- "Singapore (Low-Risk)"

df_combined_foi <- bind_rows(df_foi_high_uk, df_foi_low_uk, df_foi_high_sg, df_foi_low_sg)
df_combined_foi$scenario <- factor(df_combined_foi$scenario, levels = c(
  "England (High-Risk)",
  "Singapore (High-Risk)",
  "England (Low-Risk)",
  "Singapore (Low-Risk)"
))
df_combined_foi$group <- factor(df_combined_foi$group, levels = sort(unique(as.numeric(as.character(df_combined_foi$group)))))
df_combined_foi <- df_combined_foi %>% complete(group, scenario, fill = list(ymin = NA, lower = NA, middle = NA, upper = NA, ymax = NA))

size <- 35
# size (28, 14)
p3 <- ggplot(df_combined_foi, aes(
  x = group,
  ymin = lower,
  lower = lower,
  middle = middle,
  upper = upper,
  ymax = upper,
  color = scenario,
  fill = scenario,
  group = interaction(group, scenario)  # for boxplot grouping only
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_line(aes(y = middle, group = scenario), position = position_dodge(width = 0.8), linewidth = 0.8) +
  geom_point(aes(y = middle), position = position_dodge(width = 0.8), size = 7, shape = 18) +
  labs(x = "Year", y = "Force of Infection") +
  scale_color_manual(
    name = NULL,
    values = c("England (High-Risk)" = "royalblue", 
               "England (Low-Risk)" = "firebrick", 
               "Singapore (High-Risk)" = "seagreen", 
               "Singapore (Low-Risk)" = "slateblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("England (High-Risk)" = "skyblue", 
               "England (Low-Risk)" = "moccasin", 
               "Singapore (High-Risk)" = "palegreen", 
               "Singapore (Low-Risk)" = "thistle")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete() +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.85),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.box.background = element_rect(color = "black"),
    legend.margin = margin(5, 5, 5, 5),
    legend.box.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = size),
    legend.key.height = unit(3, "lines"),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0), size = size),
    axis.title.x = element_text(size = size),
    axis.text.x = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
p3 <- p3 +
  labs(tag = "(2)") +
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0, face = "bold")
  )

########################################################## plot SG and England results ##########################################################
# time series of MSM syphilis cases
cases <- c(2030, 2123, 2379, 3485, 4052, 4605, 5440, 5610, 5761, 5969, 6313, 6169)

# times
# burnt in 5 years
n_years <- length(cases) 
t <- seq(0, n_years+6, by = 1)
t_0 = 0 
t <- t[-1]

fit_syphilis_negbin <- readRDS("fit_results_fixedinitialstate_burntin_UK.rds")

# posterior predictive check on number of cases
smr_pred <- cbind(as.data.frame(summary(
  fit_syphilis_negbin, pars = "pred_cases", probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary), t[1:(n_years)], cases[1:(n_years)])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

selected_years <- c(2011:2020, 2023:2024)
all_years <- 2011:2024
year_indices <- which(all_years %in% selected_years)

# box plot, output size (8, 6)
df <- data.frame(
  group = factor(all_years[year_indices]),
  ymin = smr_pred$X2.5.[1:(n_years)],  # minimum
  lower = smr_pred$X25.[1:(n_years)],  # Q1
  middle = smr_pred$X50.[1:(n_years)], # median
  upper = smr_pred$X75.[1:(n_years)],  # Q3
  ymax = smr_pred$X97.5.[1:(n_years)], # maximum
  observation = cases[1:(n_years)],    # observed
  year = all_years[year_indices]          # year
)

size = 35

p2 <- ggplot(df, aes(x = group, ymin = lower, lower = lower, middle = middle, upper = upper, ymax = upper, color = 'Predicted')) +
  geom_boxplot(stat = "identity", fill = "salmon", position = position_dodge(width = 0.8), width = 0.6) +
  geom_point(data = df, aes(x = group, y = observation, color = "Observed"), shape = 18, size = 10) +
  labs(title = "B: England") +
  labs(x = "Year", y = "Annual Number of Diagnosed Cases") +
  scale_color_manual(name = NULL, values = c("Predicted" = "darkred", "Observed" = "deepskyblue4")) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.20, 0.85),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.box.background = element_rect(color = "black"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0), size = size),
    axis.title.x = element_text(size = size, hjust = 0),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = size),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5)
  )

fit_syphilis_negbin <- readRDS("fit_results_fixedinitialstate_burntin_main.rds")
cases <- c(324, 365, 488, 346, 305, 197, 159, 187, 318, 308, 525, 496, 623, 573, 605) # main scenario (male incidence minus female incidence)
# times
# burnt in 5 years
n_years <- length(cases) 
t <- seq(0, n_years+6, by = 1)
t_0 = 0 
t <- t[-1]

# posterior predictive check on number of cases
smr_pred <- cbind(as.data.frame(summary(
  fit_syphilis_negbin, pars = "pred_cases", probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary), t[1:(n_years)], cases[1:(n_years)])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

# box plot, output size (48, 18)
df <- data.frame(
  group = factor(2004:2018),
  ymin = smr_pred$X2.5.[1:(n_years)],  # minimum
  lower = smr_pred$X25.[1:(n_years)],  # Q1
  middle = smr_pred$X50.[1:(n_years)], # median
  upper = smr_pred$X75.[1:(n_years)],  # Q3
  ymax = smr_pred$X97.5.[1:(n_years)], # maximum
  observation = cases[1:(n_years)],    # observed
  year = 2004:2018                       # year
)

p1 <- ggplot(df, aes(x = group, ymin = lower, lower = lower, middle = middle, upper = upper, ymax = upper, color = 'Predicted')) +
  geom_boxplot(stat = "identity", fill = "salmon", position = position_dodge(width = 0.8), width = 0.6) +
  geom_point(data = df, aes(x = group, y = observation, color = "Observed"), shape = 18, size = 10) +
  labs(title = "A: Singapore (Main)") +
  labs(x = "Year", y = "Annual Number of Diagnosed Cases") +
  scale_color_manual(name = NULL, values = c("Predicted" = "darkred", "Observed" = "deepskyblue4")) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.20, 0.85),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.box.background = element_rect(color = "black"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0), size = size),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5)
  )
p1 <- p1 +
  labs(tag = "(1)") +
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0, face = "bold")
  )

row_predicted <- (p1 + p2 + plot_layout(ncol = 2, guides = "collect")) &
  theme(legend.position = "right", legend.text = element_text(size = size)) &
  plot_annotation(
    title = NULL,
    theme = theme(
      axis.title.x = element_text(size = size, margin = margin(t = 10)),
      axis.title.y = element_text(size = size, margin = margin(r = 10))
    )
  ) &
  labs(
    x = "Year",
    y = "Annual Number of \n Diagnosed Cases"
  )

# (48, 32)
final_plot <- plot_spacer() / row_predicted / plot_spacer() / p3 + plot_layout(ncol = 1, heights = c(0.02, 1, 0.02, 1))
final_plot