library(tidyverse)
library(rstan)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(patchwork)
rstan_options (auto_write = TRUE)
options(mc.cores = parallel::detectCores(logical = FALSE)) # use all available cores by default when sampling

# time series of MSM syphilis cases
cases <- c(2030, 2123, 2379, 3485, 4052, 4605, 5440, 5610, 5761, 5969, 6313, 6169)

# Initial population size of MSM in 2004
N_t0 <- 600000

# Annual MSM population entrants (at age 15)
alpha <- 12000

# Proportion of the MSM population in group j
q_H <- 0.15
q_L <- 0.85

# Annual rate of partner change in group j
c_H <- 15.6
c_L <- 0.6

# Years spent in the sexually-active population
gamma <- 50

# times
# burnt in 5 years
n_years <- length(cases) 
t <- seq(0, n_years+6, by = 1)
t_0 = 0 
t <- t[-1]

psi_T <- 1/20 # 20 years (Late latent to Tertiary)
nu <- -log(1-128/399)/40 # 40 years (Death) with around 30% probability of death at tertiary stage

# initial syphilis incidence guess
temp = 2500

# initial conditions
# assume that for each group j
# 10% of incidences at incubation
# 20% of incidences at primary stage
# 25% of incidences at secondary stage
# 30% of incidences at early latent stage
# 10% of incidences at late latent stage
# 5% of incidences at teritary stage
# 5% of incidences at recovery stage
p_H = 0.9
p_L = 0.1
U_N_H = (N_t0 - temp) * q_H;
I_N_H = 0.1 * temp * p_H
P_N_H = 0.1 * temp * p_H
S_N_H = 0 * temp * p_H
E_N_H = 0 * temp * p_H
L_N_H = 0 * temp * p_H
T_N_H = 0 * temp * p_H
R_N_H = 0.8 * temp * p_H
U_N_L = (N_t0 - temp) * q_L
I_N_L = 0.1 * temp * p_L
P_N_L = 0.1 * temp * p_L
S_N_L = 0 * temp * p_L
E_N_L = 0 * temp * p_L
L_N_L = 0 * temp * p_L
T_N_L = 0 * temp * p_L
R_N_L = 0.8 * temp * p_L
y0 = c(U_N_H=U_N_H, I_N_H=I_N_H, P_N_H=P_N_H, S_N_H=S_N_H, E_N_H=E_N_H, L_N_H=L_N_H, T_N_H=T_N_H, R_N_H=R_N_H,
       U_N_L=U_N_L, I_N_L=I_N_L, P_N_L=P_N_L, S_N_L=S_N_L, E_N_L=E_N_L, L_N_L=L_N_L, T_N_L=T_N_L, R_N_L=R_N_L)

# data for Stan
data_syphilis <- list(n_years = n_years, y0 = y0, ts = t, t_0 = t_0, q_H = q_H, c_H = c_H, c_L = c_L, q_L = q_L, cases = cases, alpha = alpha, N_t0 = N_t0, gamma = gamma, psi_T=psi_T, nu=nu)

# run MCMC
model <- stan_model("syphilis_fixedinitialstate_burntin.stan")
fit_syphilis_negbin <- sampling(model,
                                data = data_syphilis,
                                iter = 2000,
                                warmup = 1000,
                                control = list(adapt_delta = 0.995, max_treedepth = 15),
                                chains = 6,
                                cores = 6,
                                seed = 42, # seed = 42 for UK
                                verbose=TRUE)

saveRDS(fit_syphilis_negbin, file = "fit_results_fixedinitialstate_burntin_UK.rds")
fit_syphilis_negbin <- readRDS("fit_results_fixedinitialstate_burntin_UK.rds")

# print the data
options(max.print = 1000000)
sink("output_fixedinitialstate_burntin_uk.txt")
print(fit_syphilis_negbin)
sink()

# get the parameter median and 95% CI
samples <- rstan::extract(fit_syphilis_negbin, pars = "kappa_D")$kappa_D
posterior_median <- median(samples)
ci_lower <- quantile(samples, 0.025)
ci_upper <- quantile(samples, 0.975)
cat(sprintf("Posterior median: %.5f\n95%% credible interval: [%.5f, %.5f]\n",
            posterior_median, ci_lower, ci_upper))

# print the mcmc results
pars=c('beta', 'phi_beta', 'epsilon', 'rho', 'eta_H_init', 'phi_eta', 'omega', 'sigma', 'mu', 'psi_S', 'psi_E', 'psi_L', 'kappa_D')
print(fit_syphilis_negbin, pars = pars)

# trace plots to assess mixing of a chain
traceplot(fit_syphilis_negbin, pars = pars)

# marginal posterior densities
stan_dens(fit_syphilis_negbin, pars = pars, separate_chains = TRUE)

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

row_predicted
