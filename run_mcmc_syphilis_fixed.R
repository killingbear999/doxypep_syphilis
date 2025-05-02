library(tidyverse)
library(rstan)
library(ggplot2)
library(gridExtra)
library(dplyr)
rstan_options (auto_write = TRUE)
options(mc.cores = parallel::detectCores(logical = FALSE)) # use all available cores by default when sampling

# time series of MSM syphilis cases
cases <- c(324, 365, 488, 346, 305, 197, 159, 187, 318, 308, 525, 496, 623, 573, 605, 381, 317) # main scenario (male incidence minus female incidence)

# Initial population size of MSM in 2004
N_t0 <- 97189

# Annual MSM population entrants (at age 15)
alpha <- 2524

# Proportion of the MSM population in group j
q_H <- 0.207
q_L <- 0.793

# Annual rate of partner change in group j
c_H <- 14.866
c_L <- 1.989

# Years spent in the sexually-active population
gamma <- 50

# transition rate
sigma <- 365/23 # 23 days (Incubation to Primary)
psi_S <- 365/42 # 6 weeks (Primary to Secondary)
psi_E <- 365/(365/2) # 6 months (Secondary to Early latent)
psi_L <- 1 # 1 year (Early latent to Late latent)
psi_T <- 1/20 # 20 years (Late latent to Tertiary)
nu <- 1/40 # 40 years (Death)
beta_nu <- 128/399 # Probability of death at tertiary stage

# times
n_years <- length(cases) 
t <- seq(0, n_years, by = 1)
t_0 = 0 
t <- t[-1]

# initial syphilis incidence guess
# temp = 324

# initial conditions
# assume that for each group j
# 10% of incidences at incubation
# 20% of incidences at primary stage
# 25% of incidences at secondary stage
# 30% of incidences at early latent stage
# 10% of incidences at late latent stage
# 5% of incidences at teritary stage
# 5% of incidences at recovery stage
# U_N_H = (N_t0 - temp) * q_H;
# I_N_H = 0.1 * temp * q_H;
# P_N_H = 0.15 * temp * q_H;
# S_N_H = 0.25 * temp * q_H;
# E_N_H = 0.3 * temp * q_H;
# L_N_H = 0.1 * temp * q_H;
# T_N_H = 0.05 * temp * q_H;
# R_N_H = 0.05 * temp * q_H;
# U_N_L = (N_t0 - temp) * q_L;
# I_N_L = 0.1 * temp * q_L;
# P_N_L = 0.15 * temp * q_L;
# S_N_L = 0.25 * temp * q_L;
# E_N_L = 0.3 * temp * q_L;
# L_N_L = 0.1 * temp * q_L;
# T_N_L = 0.05 * temp * q_L;
# R_N_L = 0.05 * temp * q_L;
# y0 = c(U_N_H=U_N_H, I_N_H=I_N_H, P_N_H=P_N_H, S_N_H=S_N_H, E_N_H=E_N_H, L_N_H=L_N_H, T_N_H=T_N_H, R_N_H=R_N_H,
#        U_N_L=U_N_L, I_N_L=I_N_L, P_N_L=P_N_L, S_N_L=S_N_L, E_N_L=E_N_L, L_N_L=L_N_L, T_N_L=T_N_L, R_N_L=R_N_L)

# data for Stan
data_syphilis <- list(n_years = n_years, ts = t, t_0 = t_0, q_H = q_H, c_H = c_H, c_L = c_L, q_L = q_L, cases = cases, alpha = alpha, N_t0 = N_t0, gamma = gamma, sigma=sigma, psi_S=psi_S, psi_E=psi_E, psi_L=psi_L, psi_T=psi_T, nu=nu, beta_nu=beta_nu)

# parameter initialization for rstan for debugging
init_fun <- function() {
  list(beta = 0.1, phi_beta = 0.1, epsilon=0.1, rho=20.94, eta_H_init=0.1, phi_eta=0.1, omega=0.451, mu=239.5, kappa_D=0.1, beta_nu=0.8)
}

# run MCMC
model <- stan_model("syphilis_fixed_transition_rate.stan")
fit_syphilis_negbin <- sampling(model,
                                data = data_syphilis,
                                # algorithm = "Fixed_param", # for debugging
                                iter = 2000,
                                warmup = 1000,
                                control = list(adapt_delta = 0.999, max_treedepth = 20),
                                chains = 4,
                                cores = 8,
                                seed = 42,
                                # init=init_fun, # for debugging
                                # diagnostic_file = "diagnostics.csv" # for debugging
                                verbose=TRUE)

saveRDS(fit_syphilis_negbin, file = "fit_results_main.rds")
fit_syphilis_negbin <- readRDS("fit_results_main.rds")

# print the mcmc results
pars=c('beta', 'phi_beta', 'epsilon', 'rho', 'eta_H_init', 'phi_eta', 'omega', 'mu', 'kappa_D', 'beta_nu')
print(fit_syphilis_negbin, pars = pars)

# trace plots to assess mixing of a chain
traceplot(fit_syphilis_negbin, pars = pars)

# marginal posterior densities
stan_dens(fit_syphilis_negbin, pars = pars, separate_chains = TRUE)

# posterior predictive check on number of cases
smr_pred <- cbind(as.data.frame(summary(
  fit_syphilis_negbin, pars = "pred_cases", probs = c(0.05, 0.5, 0.95))$summary), t[1:(n_years-1)], cases[1:(n_years-1)])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

main_data <- data.frame(
  Year = 2004:2018,
  Median = smr_pred$X50.[1:(n_years-2)],
  Lower = smr_pred$X5.[1:(n_years-2)],
  Upper = smr_pred$X95.[1:(n_years-2)],
  Observation = cases[1:(n_years-2)]
)

plot_data <- main_data |>
  pivot_longer(cols = c("Median", "Observation"), names_to = "Type", values_to = "Value")

# Base plot
ggplot() +
  # Credible interval
  geom_linerange(
    data = main_data,
    aes(x = Year, ymin = Lower, ymax = Upper, color = "95% Credible Interval"),
    size = 5, alpha = 0.5
  ) +
  # Median (discrete horizontal line)
  geom_segment(
    data = main_data,
    aes(x = Year - 0.15, xend = Year + 0.15, y = Median, yend = Median, color = "Median"),
    size = 1.2
  ) +
  # Observed points
  geom_point(
    data = main_data,
    aes(x = Year, y = Observation, color = "Observed"), shape = 18, size = 2.5
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "95% Credible Interval" = "salmon",
      "Median" = "darkred",
      "Observed" = "deepskyblue4"
    )
  ) +
  labs(
    title = "Main Scenario: Model Projections vs Observed",
    y = "Annual incidence of diagnosed",
    x = "Year"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )