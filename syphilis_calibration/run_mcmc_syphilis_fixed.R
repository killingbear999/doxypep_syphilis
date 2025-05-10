library(tidyverse)
library(rstan)
library(ggplot2)
library(gridExtra)
library(dplyr)
rstan_options (auto_write = TRUE)
options(mc.cores = parallel::detectCores(logical = FALSE)) # use all available cores by default when sampling

# time series of MSM syphilis cases
cases <- c(324, 365, 488, 346, 305, 197, 159, 187, 318, 308, 525, 496, 623, 573, 605, 381, 317) # main scenario (male incidence minus female incidence)
# cases <- c(518, 632, 840, 564, 599, 532, 407, 536, 875, 801, 935, 832, 894, 804, 863, 515, 365) # upper bound (all male incidence)
# cases <- c(31, 38, 51, 34, 36, 32, 24, 32, 53, 48, 56, 50, 54, 49, 52, 31, 22) # lower bound (percentage of MSM among male population, assuming syphilis incidence rate is the same among MSM and MSF)

# Initial population size of MSM in 2004
N_t0 <- 97189 # applied the percentage of MSM among male in 2019 to male population in 2004

# Annual MSM population entrants (at age 15)
alpha <- 2524 # applied the percentage of MSM among male in 2019 to male population entrants (at age 15) from 2004 to 2018 and took the average

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
                                iter = 4000,
                                warmup = 2000,
                                control = list(adapt_delta = 0.999, max_treedepth = 20),
                                chains = 6,
                                cores = 6,
                                seed = 42,
                                # init=init_fun, # for debugging
                                # diagnostic_file = "diagnostics.csv" # for debugging
                                verbose=TRUE)

# save the rstan results
# saveRDS(fit_syphilis_negbin, file = "fit_results_main.rds")

# adjust the parameter 'cases' before plotting
fit_syphilis_negbin <- readRDS("fit_results_main.rds")

# print the data
options(max.print = 1000000)
sink("output_main.txt")
print(fit_syphilis_negbin)
sink()

# print the mcmc results
pars=c('beta', 'phi_beta', 'epsilon', 'rho', 'eta_H_init', 'phi_eta', 'omega', 'mu', 'kappa_D')
print(fit_syphilis_negbin, pars = pars)

# get the parameter value
phi_beta_samples <- rstan::extract(fit_syphilis_negbin, pars = "phi_beta")$phi_beta
mean(phi_beta_samples)

# trace plots to assess mixing of a chain, output size (10, 5)
traceplot(fit_syphilis_negbin, pars = pars)

# marginal posterior densities, output size (10, 5)
stan_dens(fit_syphilis_negbin, pars = pars, separate_chains = TRUE)

# posterior predictive check on number of cases
smr_pred <- cbind(as.data.frame(summary(
  fit_syphilis_negbin, pars = "pred_cases", probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary), t[1:(n_years-1)], cases[1:(n_years-1)])
colnames(smr_pred) <- make.names(colnames(smr_pred)) # to remove % in the col names

# box plot, output size (8, 6)
df <- data.frame(
  group = factor(2004:2018),
  ymin = smr_pred$X2.5.[1:(n_years-2)],    # minimum
  lower = smr_pred$X25.[1:(n_years-2)],  # Q1
  middle = smr_pred$X50.[1:(n_years-2)], # median
  upper = smr_pred$X75.[1:(n_years-2)],  # Q3
  ymax = smr_pred$X97.5.[1:(n_years-2)],  # maximum
  observation = cases[1:(n_years-2)],    # observed
  year = 2004:2018                       # year
)

ggplot(df, aes(x = group, ymin = lower, lower = lower, middle = middle, upper = upper, ymax = upper, color = 'Predicted')) +
  geom_boxplot(stat = "identity", fill = "salmon") +
  geom_point(data = df, aes(x = group, y = observation, color = "Observed"), shape = 18, size = 2.5) +
  labs(x = "Year", y = "Annual Incidence of Diagnosed") +
  scale_color_manual(name = NULL, values = c("Predicted" = "darkred", "Observed" = "deepskyblue4")) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85, 0.85),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.box.background = element_rect(color = "black"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )