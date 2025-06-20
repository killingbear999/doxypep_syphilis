library(deSolve)
library(dplyr)
library(tidyr)
library(ggplot2)

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
ggplot(df_combined_foi, aes(
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
  geom_line(aes(y = middle, group = scenario), position = position_dodge(width = 0.8), linewidth = 0.8) +   # group = scenario here
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
    legend.position.inside = c(0.25, 0.85),
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