library(ggplot2)
library(patchwork)

size = 35
############################ uptake rate of 0.1 ############################
# plot averted cases
averted_lowscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1964, 1509, 1510, 13064, 13230, 13067),
  middle = c(10090, 23278, 23286, 72051, 72507, 72059),
  ymax = c(21394, 160771, 161128, 269009, 269353, 269025)
)

averted_highscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1702, 1222, 1226, 20117, 20120, 20117),
  middle = c(7606, 13476, 13485, 74275, 74367, 74277),
  ymax = c(18513, 84823, 85130, 283930, 283938, 283930)
)

averted_lowscreeningrate_0.1$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.1$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.1, averted_highscreeningrate_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p1 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  labs(title = "A: Uptake Rate of 10.0%") +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  # ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 500000), labels = function(x) format(x, scientific = FALSE, big.mark = "", trim = TRUE)) +
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
    legend.key.height = unit(3, "lines"),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0), size = size),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
p1 <- p1 +
  labs(tag = "(1) Singapore") +
  theme(
    plot.tag.position = c(0, 0.97),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0)
  )

# plot averted cases per prescription
averted_lowscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.52, 2.40, 1.88, 1.66, 0.61, 1.63),
  middle = c(2.67, 9.76, 7.86, 8.45, 3.47, 8.25),
  ymax = c(5.65, 21.00, 17.13, 21.83, 10.99, 19.44)
)

averted_highscreeningrate_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.45, 2.12, 1.70, 0.077, 0.020, 0.077),
  middle = c(2.01, 8.05, 6.70, 0.37, 0.10, 0.37),
  ymax = c(4.89, 19.19, 15.63, 4.67, 1.77, 4.69)
)

averted_lowscreeningrate_0.1$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.1$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.1, averted_highscreeningrate_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p4 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  # ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 22)) +
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
    legend.key.height = unit(3, "lines"),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0), size = size),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
p4 <- p4 +
  labs(tag = "(2) Singapore") +
  theme(
    plot.tag.position = c(0, 0.97),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0)
  )

############################ uptake rate of 0.33 ############################
# plot averted cases
averted_lowscreeningrate_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(5836, 4297, 4298, 20606, 20626, 20606),
  middle = c(30547, 49461, 49508, 99568, 99664, 99572),
  ymax = c(69478, 294407, 294776, 436027, 437446, 436028)
)

averted_highscreeningrate_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(4985, 3507, 3526, 20720, 20721, 20720),
  middle = c(22386, 30652, 30705, 75471, 75484, 75471),
  ymax = c(58439, 162306, 162368, 283872, 283976, 283972)
)

averted_lowscreeningrate_0.33$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.33$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.33, averted_highscreeningrate_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p2 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  labs(title = "B: Uptake Rate of 33.0%") +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  # ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 500000), labels = function(x) format(x, scientific = FALSE, big.mark = "", trim = TRUE)) +
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
    legend.key.height = unit(3, "lines"),
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
  ymin = c(0.47, 2.31, 1.82, 0.85, 0.30, 0.85),
  middle = c(2.44, 9.26, 7.41, 4.04, 1.56, 4.01),
  ymax = c(5.56, 20.22, 16.34, 16.25, 6.61, 15.38)
)

averted_highscreeningrate_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.40, 2.06, 1.64, 0.051, 0.013, 0.05),
  middle = c(1.79, 7.61, 6.33, 0.23, 0.062, 0.23),
  ymax = c(4.68, 18.12, 14.89, 1.92, 0.69, 1.91)
)

averted_lowscreeningrate_0.33$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.33$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.33, averted_highscreeningrate_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p5 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  # ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 22)) +
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
    legend.key.height = unit(3, "lines"),
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
  ymin = c(10097, 7212, 7215, 22393, 22410, 22393),
  middle = c(52963, 66988, 66989, 104403, 104526, 104405),
  ymax = c(133516, 360697, 360695, 461768, 461822, 461768)
)

averted_highscreeningrate_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(8554, 5935, 5960, 21032, 21038, 21032),
  middle = c(38143, 42739, 42799, 75761, 75764, 75761),
  ymax = c(109313, 204607, 205162, 283973, 283977, 283973)
)

averted_lowscreeningrate_0.66$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.66$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.66, averted_highscreeningrate_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p3 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  labs(title = "C: Uptake Rate of 66.0%") +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  # ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 500000)) +
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
    legend.key.height = unit(3, "lines"),
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
  ymin = c(0.40, 2.22, 1.75, 0.49, 0.18, 0.49),
  middle = c(2.12, 8.74, 7.04, 2.29, 0.86, 2.28),
  ymax = c(5.34, 19.38, 15.65, 9.97, 3.73, 9.96)
)

averted_highscreeningrate_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.34, 1.99, 1.59, 0.036, 0.010, 0.036),
  middle = c(1.53, 7.17, 5.97, 0.18, 0.047, 0.18),
  ymax = c(4.37, 17.15, 14.26, 1.15, 0.41, 1.14)
)

averted_lowscreeningrate_0.66$scenario <- "Low Screening Rate"
averted_highscreeningrate_0.66$scenario <- "High Screening Rate"

df_combined <- rbind(averted_lowscreeningrate_0.66, averted_highscreeningrate_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))

p6 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  scale_color_manual(
    name = NULL,
    values = c("Low Screening Rate" = "purple", "High Screening Rate" = "darkorange")
  ) +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c("Low Screening Rate" = "plum", "High Screening Rate" = "moccasin")
  # ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 22)) +
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
    legend.key.height = unit(3, "lines"),
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
    y = "Total Number of \n Averted Cases"
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
    y = "Number of Averted \n Cases per Prescription"
  )

# combine and plot
final_plot <- row_averted / row_averted_per_prescription
final_plot