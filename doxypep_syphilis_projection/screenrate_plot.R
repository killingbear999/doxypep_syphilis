library(ggplot2)
library(patchwork)

size = 35
############################ uptake rate of 0.1 ############################
# plot averted cases
averted_lowscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(644, 494, 496, 4276, 4335, 4277),
  middle = c(3287, 7606, 7607, 23542, 23586, 23543),
  ymax = c(6854, 51220, 51336, 86942, 87473, 87008)
)

averted_highscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(550, 395, 396, 6587, 6604, 6587),
  middle = c(2478, 4410, 4414, 24290, 24338, 24290),
  ymax = c(6087, 27684, 27750, 93185, 93243, 93185)
)

averted_lowscreeningrate_0.1$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.1$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.1, averted_highscreeningrate_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p1 <- ggplot(df_combined, aes(
  x = group,
  ymin = ymin,
  lower = ymin,
  middle = middle,
  upper = ymax,
  ymax = ymax,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  labs(title = "A: Uptake Rate of 10.0%") +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 150000)) +
  theme_minimal(base_size = 13) +
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
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
p1 <- p1 +
  labs(tag = "(1)") +
  theme(
    plot.tag.position = c(0, 0.95),
    plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
  )

# plot averted cases per prescription
averted_lowscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.17, 0.78, 0.62, 0.54, 0.20, 0.53),
  middle = c(0.87, 3.17, 2.56, 2.77, 1.12, 2.69),
  ymax = c(1.81, 6.78, 5.49, 6.99, 3.51, 6.18)
)

averted_highscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.15, 0.69, 0.55, 0.025, 0.007, 0.025),
  middle = c(0.65, 2.60, 2.18, 0.12, 0.034, 0.12),
  ymax = c(1.61, 6.25, 5.05, 1.52, 0.58, 1.49)
)

averted_lowscreeningrate_0.1$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.1$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.1, averted_highscreeningrate_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p4 <- ggplot(df_combined, aes(
  x = group,
  ymin = ymin,
  lower = ymin,
  middle = middle,
  upper = ymax,
  ymax = ymax,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  ) +
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
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
p4 <- p4 +
  labs(tag = "(2)") +
  theme(
    plot.tag.position = c(0, 0.95),
    plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
  )

############################ uptake rate of 0.33 ############################
# plot averted cases
averted_lowscreeningrate_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1911, 1404, 1404, 6791, 6798, 6791),
  middle = c(9962, 16164, 16168, 32620, 32675, 32621),
  ymax = c(22356, 93784, 93901, 139078, 139247, 139083)
)

averted_highscreeningrate_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1615, 1140, 1142, 6833, 6835, 6833),
  middle = c(7291, 9973, 9989, 24708, 24711, 24708),
  ymax = c(19051, 53127, 53268, 93599, 93601, 93599)
)

averted_lowscreeningrate_0.33$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.33$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.33, averted_highscreeningrate_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p2 <- ggplot(df_combined, aes(
  x = group,
  ymin = ymin,
  lower = ymin,
  middle = middle,
  upper = ymax,
  ymax = ymax,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  labs(title = "B: Uptake Rate of 33.0%") +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 150000), labels = function(x) format(x, scientific = FALSE, big.mark = "", trim = TRUE)) +
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
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

# plot averted cases per prescription
averted_lowscreeningrate_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.15, 0.76, 0.60, 0.28, 0.099, 0.28),
  middle = c(0.80, 3.02, 2.42, 1.33, 0.51, 1.31),
  ymax = c(1.79, 6.56, 5.26, 5.23, 2.13, 5.00)
)

averted_highscreeningrate_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.13, 0.67, 0.54, 0.017, 0.004, 0.017),
  middle = c(0.58, 2.46, 2.06, 0.075, 0.020, 0.075),
  ymax = c(1.52, 5.89, 4.78, 0.62, 0.22, 0.62)
)

averted_lowscreeningrate_0.33$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.33$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.33, averted_highscreeningrate_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p5 <- ggplot(df_combined, aes(
  x = group,
  ymin = ymin,
  lower = ymin,
  middle = middle,
  upper = ymax,
  ymax = ymax,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  ) +
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
    axis.title.x = element_text(size = size),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

############################ uptake rate of 0.66 ############################
# plot averted cases
averted_lowscreeningrate_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(3306, 2357, 2358, 7383, 7389, 7383),
  middle = c(17427, 21826, 21828, 34071, 34084, 34071),
  ymax = c(43359, 114649, 114716, 149057, 149134, 149058)
)

averted_highscreeningrate_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(2765, 1937, 1946, 6869, 6869, 6869),
  middle = c(12455, 13853, 13855, 24718, 24720, 24718),
  ymax = c(35778, 67012, 67090, 93602, 93604, 93602)
)

averted_lowscreeningrate_0.66$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.66$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.66, averted_highscreeningrate_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p3 <- ggplot(df_combined, aes(
  x = group,
  ymin = ymin,
  lower = ymin,
  middle = middle,
  upper = ymax,
  ymax = ymax,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  labs(title = "C: Uptake Rate of 66.0%") +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 150000)) +
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
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

# plot averted cases per prescription
averted_lowscreeningrate_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.13, 0.72, 0.57, 0.16, 0.058, 0.16),
  middle = c(0.70, 2.85, 2.30, 0.75, 0.28, 0.74),
  ymax = c(1.74, 6.30, 5.01, 3.23, 1.21, 3.18)
)

averted_highscreeningrate_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.11, 0.64, 0.52, 0.012, 0.003, 0.012),
  middle = c(0.50, 2.32, 1.94, 0.057, 0.015, 0.057),
  ymax = c(1.43, 5.58, 4.59, 0.37, 0.13, 0.37)
)

averted_lowscreeningrate_0.66$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.66$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.66, averted_highscreeningrate_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p6 <- ggplot(df_combined, aes(
  x = group,
  ymin = ymin,
  lower = ymin,
  middle = middle,
  upper = ymax,
  ymax = ymax,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  ) +
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
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

row_averted <- (p1 + p2 + p3 + plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "right", legend.text = element_text(size = size)) &
  plot_annotation(
    title = NULL,
    theme = theme(
      axis.title.x = element_text(size = size, margin = margin(t = 10)),
      axis.title.y = element_text(size = size, margin = margin(r = 10))
    )
  ) &
  labs(
    x = "Doxy-PEP Strategy",
    y = "Total Number of \n Averted Diagnosed Cases"
  )

row_averted_per_prescription <- (p4 + p5 + p6 + plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "right", legend.text = element_text(size = size)) &
  plot_annotation(
    title = NULL,
    theme = theme(
      axis.title.x = element_text(size = size, margin = margin(t = 10)),
      axis.title.y = element_text(size = size, margin = margin(r = 10))
    )
  ) &
  labs(
    x = "Doxy-PEP Strategy",
    y = "Number of Averted Diagnosed \n Cases per Prescription"
  )

# combine and plot
final_plot <- row_averted / row_averted_per_prescription
final_plot