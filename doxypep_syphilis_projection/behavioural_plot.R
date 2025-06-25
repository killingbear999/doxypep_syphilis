library(ggplot2)
library(patchwork)

size = 35
############################ Singapore #####################################
############################ uptake rate of 0.1 ############################
# plot averted cases
averted_lowbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(972, 700, 702, 19499, 19507, 19499),
  middle = c(4279, 8310, 8321, 73221, 73249, 73221),
  ymax = c(10117, 55178, 55321, 277301, 277752, 277204)
)

averted_normalbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1702, 1222, 1226, 20117, 20120, 20117),
  middle = c(7606, 13476, 13485, 74275, 74367, 74277),
  ymax = c(18513, 84823, 85130, 283930, 283938, 283930)
)

averted_highbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(2673, 1927, 1933, 20592, 20601, 20592),
  middle = c(12251, 19467, 19479, 74628, 74647, 74628),
  ymax = c(31121, 115829, 115851, 283940, 283947, 283940)
)

averted_lowbehaviouralpattern_0.1$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.1$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.1$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.1, averted_normalbehaviouralpattern_0.1, averted_highbehaviouralpattern_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

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
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 290000), labels = function(x) format(x, scientific = FALSE, big.mark = "", trim = TRUE)) +
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
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0, face = "bold")
  )

# plot averted cases per prescription
averted_lowbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.26, 1.19, 0.96, 0.049, 0.014, 0.049),
  middle = c(1.13, 4.56, 3.78, 0.26, 0.078, 0.26),
  ymax = c(2.67, 10.76, 8.75, 3.65, 1.44, 3.60)
)

averted_normalbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.45, 2.12, 1.70, 0.077, 0.020, 0.077),
  middle = c(2.01, 8.05, 6.70, 0.37, 0.10, 0.37),
  ymax = c(4.89, 19.19, 15.63, 4.67, 1.77, 4.69)
)

averted_highbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.71, 3.45, 2.76, 0.12, 0.030, 0.12),
  middle = c(3.24, 13.01, 10.87, 0.53, 0.14, 0.52),
  ymax = c(8.22, 31.62, 25.76, 5.25, 1.95, 5.25)
)

averted_lowbehaviouralpattern_0.1$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.1$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.1$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.1, averted_normalbehaviouralpattern_0.1, averted_highbehaviouralpattern_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

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
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 32)) +
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
p4 <- p4 +
  labs(tag = "(2) Singapore") +
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0, face = "bold")
  )

############################ uptake rate of 0.33 ############################
# plot averted cases
averted_lowbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(3009, 2124, 2132, 20712, 20713, 20712),
  middle = c(13200, 21156, 21182, 75181, 75196, 75181),
  ymax = c(32679, 122282, 122851, 283971, 283975, 283971)
)

averted_normalbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(4985, 3507, 3526, 20720, 20721, 20720),
  middle = c(22386, 30652, 30705, 75471, 75484, 75471),
  ymax = c(58439, 162306, 162368, 283872, 283976, 283972)
)

averted_highbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(7294, 5166, 5179, 20937, 21003, 20937),
  middle = c(33383, 39818, 39900, 75532, 75554, 75532),
  ymax = c(94504, 195468, 195900, 283972, 283976, 283972)
)

averted_lowbehaviouralpattern_0.33$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.33$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.33$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.33, averted_normalbehaviouralpattern_0.33, averted_highbehaviouralpattern_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

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
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 290000), labels = function(x) format(x, scientific = FALSE, big.mark = "", trim = TRUE)) +
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
averted_lowbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.24, 1.17, 0.94, 0.033, 0.0086, 0.033),
  middle = c(1.06, 4.36, 3.61, 0.15, 0.042, 0.15),
  ymax = c(2.62, 10.23, 8.42, 1.62, 0.60, 1.60)
)

averted_normalbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.40, 2.06, 1.64, 0.051, 0.013, 0.05),
  middle = c(1.79, 7.61, 6.33, 0.23, 0.062, 0.23),
  ymax = c(4.68, 18.12, 14.89, 1.92, 0.69, 1.91)
)

averted_highbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.58, 3.32, 2.65, 0.072, 0.020, 0.072),
  middle = c(2.67, 12.21, 10.20, 0.34, 0.091, 0.34),
  ymax = c(7.56, 30.19, 24.87, 2.20, 0.79, 2.19)
)

averted_lowbehaviouralpattern_0.33$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.33$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.33$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.33, averted_normalbehaviouralpattern_0.33, averted_highbehaviouralpattern_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

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
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 32)) +
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

############################ uptake rate of 0.66 ############################
# plot averted cases
averted_lowbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(5499, 3818, 3839, 21030, 21036, 21030),
  middle = c(24251, 32459, 32509, 75686, 75707, 75686),
  ymax = c(63183, 167472, 168138, 283973, 283976, 283973)
)

averted_normalbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(8554, 5935, 5960, 21032, 21038, 21032),
  middle = c(38143, 42739, 42799, 75761, 75764, 75761),
  ymax = c(109313, 204607, 205162, 283973, 283977, 283973)
)

averted_highbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(11268, 8193, 8198, 21157, 21157, 21157),
  middle = c(50904, 51058, 51080, 75762, 75765, 75762),
  ymax = c(166182, 213123, 231724, 283974, 283977, 283974)
)

averted_lowbehaviouralpattern_0.66$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.66$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.66$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.66, averted_normalbehaviouralpattern_0.66, averted_highbehaviouralpattern_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

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
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 290000)) +
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
averted_lowbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.22, 1.14, 0.92, 0.025, 0.0068, 0.025),
  middle = c(0.97, 4.17, 3.46, 0.12, 0.032, 0.12),
  ymax = c(2.53, 9.77, 8.06, 0.98, 0.35, 0.98)
)

averted_normalbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.34, 1.99, 1.59, 0.036, 0.010, 0.036),
  middle = c(1.53, 7.17, 5.97, 0.18, 0.047, 0.18),
  ymax = c(4.37, 17.15, 14.26, 1.15, 0.41, 1.14)
)

averted_highbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.45, 3.17, 2.52, 0.045, 0.014, 0.045),
  middle = c(2.04, 11.51, 9.61, 0.25, 0.068, 0.25),
  ymax = c(6.65, 29.06, 24.04, 1.59, 0.48, 1.59)
)

averted_lowbehaviouralpattern_0.66$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.66$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.66$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.66, averted_normalbehaviouralpattern_0.66, averted_highbehaviouralpattern_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

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
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 32)) +
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

############################ England #######################################
############################ uptake rate of 0.1 ############################
# plot averted cases
averted_lowbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(11154, 40373, 40378, 306439, 306450, 306439),
  middle = c(23040, 171876, 171895, 829379, 829393, 829379),
  ymax = c(38062, 601689, 601729, 2152428, 2152455, 2152428)
)

averted_normalbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(20239, 66678, 66703, 321794, 321802, 321794),
  middle = c(42166, 266565, 266584, 835412, 835496, 835412),
  ymax = c(70180, 894168, 894421, 2163202, 2163289, 2163202)
)

averted_highbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(32912, 99544, 99544, 324112, 324118, 324112),
  middle = c(70991, 369583, 369660, 842222, 842334, 842222),
  ymax = c(119119, 1205907, 1206016, 2208121, 2208124, 2208121)
)

averted_lowbehaviouralpattern_0.1$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.1$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.1$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.1, averted_normalbehaviouralpattern_0.1, averted_highbehaviouralpattern_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

p7 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  labs(title = "") +
  scale_color_manual(
    name = NULL,
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 2300000), labels = function(x) format(x, scientific = FALSE, big.mark = "", trim = TRUE)) +
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
p7 <- p7 +
  labs(tag = "(3) England") +
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0, face = "bold")
  )

# plot averted cases per prescription
averted_lowbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.62, 3.81, 3.47, 0.20, 0.041, 0.20),
  middle = c(1.28, 8.13, 7.44, 0.73, 0.17, 0.73),
  ymax = c(2.11, 13.34, 12.09, 7.22, 2.46, 7.12)
)

averted_normalbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1.12, 6.82, 6.21, 0.31, 0.063, 0.31),
  middle = c(2.34, 14.70, 13.50, 1.03, 0.22, 1.03),
  ymax = c(3.90, 24.38, 22.21, 9.79, 3.58, 9.75)
)

averted_highbehaviouralpattern_0.1 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1.83, 11.28, 10.22, 0.48, 0.095, 0.48),
  middle = c(3.94, 24.81, 22.68, 1.49, 0.30, 1.49),
  ymax = c(6.62, 42.46, 38.37, 11.76, 4.03, 11.76)
)

averted_lowbehaviouralpattern_0.1$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.1$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.1$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.1, averted_normalbehaviouralpattern_0.1, averted_highbehaviouralpattern_0.1)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

p10 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  scale_color_manual(
    name = NULL,
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 45)) +
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
p10 <- p10 +
  labs(tag = "(4) England") +
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = size, hjust = 0, vjust = 0, face = "bold")
  )

############################ uptake rate of 0.33 ############################
# plot averted cases
averted_lowbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(36088, 104749, 104783, 328010, 328013, 328010),
  middle = c(74988, 380772, 380773, 848667, 848736, 848667),
  ymax = c(124738, 1214961, 1215062, 2208154, 2208155, 2208154)
)

averted_normalbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(62906, 150356, 150375, 332906, 332929, 332906),
  middle = c(135690, 503320, 503329, 853239, 853309, 853239),
  ymax = c(227587, 1536342, 1536394, 2208156, 2208156, 2208156)
)

averted_highbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(97257, 193025, 193037, 334208, 334258, 334208),
  middle = c(221473, 608638, 608646, 853959, 854013, 853959),
  ymax = c(385361, 1750123, 1750170, 2252347, 2252359, 2252348)
)

averted_lowbehaviouralpattern_0.33$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.33$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.33$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.33, averted_normalbehaviouralpattern_0.33, averted_highbehaviouralpattern_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

p8 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  labs(title = "") +
  scale_color_manual(
    name = NULL,
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 2300000), labels = function(x) format(x, scientific = FALSE, big.mark = "", trim = TRUE)) +
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
averted_lowbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.61, 3.67, 3.33, 0.13, 0.027, 0.13),
  middle = c(1.26, 7.82, 7.13, 0.43, 0.086, 0.43),
  ymax = c(2.10, 12.99, 11.76, 3.67, 1.26, 3.67)
)

averted_normalbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1.06, 6.47, 5.86, 0.20, 0.042, 0.20),
  middle = c(2.28, 14.02, 12.77, 0.65, 0.13, 0.65),
  ymax = c(3.83, 23.76, 21.37, 4.21, 1.41, 4.21)
)

averted_highbehaviouralpattern_0.33 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1.64, 10.52, 9.54, 0.27, 0.061, 0.27),
  middle = c(3.73, 23.69, 21.53, 0.98, 0.19, 0.98),
  ymax = c(6.49, 41.51, 37.22, 5.45, 1.53, 5.44)
)

averted_lowbehaviouralpattern_0.33$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.33$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.33$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.33, averted_normalbehaviouralpattern_0.33, averted_highbehaviouralpattern_0.33)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

p11 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  scale_color_manual(
    name = NULL,
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 45)) +
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
averted_lowbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(68169, 156852, 156871, 334554, 334558, 334554),
  middle = c(147136, 519778, 519797, 854865, 854949, 854865),
  ymax = c(246199, 1564865, 1564916, 2208157, 2208157, 2208157)
)

averted_normalbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(114546, 206914, 206926, 334775, 334808, 334775),
  middle = c(258450, 628937, 628971, 856642, 856736, 856642),
  ymax = c(447327, 1815654, 1815698, 2264549, 2264663, 2264540)
)

averted_highbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(162333, 246174, 246222, 334842, 334874, 334842),
  middle = c(398896, 709552, 709556, 856722, 856812, 856722),
  ymax = c(736496, 2019576, 2019912, 2276154, 2276255, 2276154)
)

averted_lowbehaviouralpattern_0.66$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.66$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.66$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.66, averted_normalbehaviouralpattern_0.66, averted_highbehaviouralpattern_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

p9 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  labs(title = "") +
  scale_color_manual(
    name = NULL,
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 2300000)) +
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
averted_lowbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.58, 3.51, 3.18, 0.10, 0.021, 0.10),
  middle = c(1.24, 7.52, 6.84, 0.33, 0.065, 0.33),
  ymax = c(2.07, 12.66, 11.39, 2.16, 0.72, 2.16)
)

averted_normalbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(0.96, 6.10, 5.52, 0.14, 0.031, 0.14),
  middle = c(2.18, 13.37, 12.16, 0.50, 0.098, 0.50),
  ymax = c(3.77, 23.07, 20.83, 2.86, 0.79, 2.86)
)

averted_highbehaviouralpattern_0.66 <- data.frame(
  group = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'),
  ymin = c(1.37, 9.86, 8.92, 0.17, 0.042, 0.17),
  middle = c(3.36, 22.55, 20.59, 0.72, 0.15, 0.72),
  ymax = c(6.20, 40.32, 36.31, 4.05, 0.93, 4.03)
)

averted_lowbehaviouralpattern_0.66$scenario <- "Low Adherence Behavioural Pattern"
averted_normalbehaviouralpattern_0.66$scenario <- "Normal Adherence Behavioural Pattern"
averted_highbehaviouralpattern_0.66$scenario <- "High Adherence Behavioural Pattern"

df_combined <- rbind(averted_lowbehaviouralpattern_0.66, averted_normalbehaviouralpattern_0.66, averted_highbehaviouralpattern_0.66)
df_combined$group <- factor(df_combined$group, levels = c('DbE', 'DoD(H)', 'DoD', 'DoA(H)', 'DoA', 'DaR'))
df_combined$scenario <- factor(df_combined$scenario, levels = c("High Adherence Behavioural Pattern", "Normal Adherence Behavioural Pattern", "Low Adherence Behavioural Pattern"))

p12 <- ggplot(df_combined, aes(
  x = group,
  y = middle,
  ymin = ymin,
  ymax = ymax,
  color = scenario
)) + 
  geom_pointrange(position = position_dodge(width = 0.4), shape = 18, size = 3, linewidth = 2) +
  scale_color_manual(
    name = NULL,
    values = c("Low Adherence Behavioural Pattern" = "darkgreen", "Normal Adherence Behavioural Pattern" = "goldenrod", "High Adherence Behavioural Pattern" = "darkred")
  ) +
  labs(title = "") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 45)) +
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

row_averted_uk <- (p7 + p8 + p9 + plot_layout(ncol = 3, guides = "collect")) &
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

row_averted_per_prescription_uk <- (p10 + p11 + p12 + plot_layout(ncol = 3, guides = "collect")) &
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
# (48, 24)
final_plot <- plot_spacer() / row_averted /
  plot_spacer() /
  row_averted_per_prescription /
  plot_spacer() /
  row_averted_uk /
  plot_spacer() /
  row_averted_per_prescription_uk +
  plot_layout(ncol = 1, heights = c(0.08, 1, 0.08, 1, 0.08, 1, 0.08, 1))
final_plot