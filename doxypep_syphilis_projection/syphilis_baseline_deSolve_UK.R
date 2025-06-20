library(deSolve)
library(ggplot2)

# Helper functions
get_C <- function(I, P, S, E) {
  return(I + P + S + E)
}

get_N <- function(U, I, P, S, E, L, T, R) {
  return(U + I + P + S + E + L + T + R)
}

get_pi <- function(c_target, N_target, c_remain, N_remain) {
  return((c_target * N_target) / (c_target * N_target + c_remain * N_remain))
}

get_lambda <- function(t, t_0, c, beta, phi_beta, epsilon, C_target, N_target, pi_target, C_remain, N_remain, pi_remain, isFixed) {
  if (isFixed && t > 17) {
    t <- 17
  }
  return(c * beta * (1 + phi_beta * (t - t_0)) * (epsilon * C_target / N_target + (1 - epsilon) *
       (pi_target * C_target / N_target + pi_remain * C_remain / N_remain)))
}

get_eta <- function(t, t_0, eta_H_init, phi_eta, isFixed) {
  if (isFixed && t > 17) {
    t <- 17
  }
  return(eta_H_init * (1 + phi_eta * (t - t_0)))
}

# ODE system
syphilis_model <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    # two baselines: 1. the inferred trends in the time-varying behavioural parameters stabilise
    #                2. the trends continue until the end of the modelled period
    
    isFixed <- TRUE
    
    # Population sizes
    C_H <- get_C(I_N_H, P_N_H, S_N_H, E_N_H)
    C_L <- get_C(I_N_L, P_N_L, S_N_L, E_N_L)
    N_H <- get_N(U_N_H, I_N_H, P_N_H, S_N_H, E_N_H, L_N_H, T_N_H, R_N_H)
    N_L <- get_N(U_N_L, I_N_L, P_N_L, S_N_L, E_N_L, L_N_L, T_N_L, R_N_L)
    
    # Mixing
    pi_H <- get_pi(c_H, N_H, c_L, N_L)
    pi_L <- get_pi(c_L, N_L, c_H, N_H)
    lambda_H <- get_lambda(t, t_0, c_H, beta, phi_beta, epsilon, C_H, N_H, pi_H, C_L, N_L, pi_L, isFixed)
    lambda_L <- get_lambda(t, t_0, c_L, beta, phi_beta, epsilon, C_L, N_L, pi_L, C_H, N_H, pi_H, isFixed)
    
    # Screening
    eta_H <- get_eta(t, t_0, eta_H_init, phi_eta, isFixed)
    eta_L <- omega * eta_H
    
    # ODEs - High risk
    dU_N_H <- q_H * alpha - (lambda_H + 1/gamma) * U_N_H + rho * R_N_H
    dI_N_H <- lambda_H * U_N_H - (sigma + 1/gamma) * I_N_H
    dP_N_H <- sigma * I_N_H - (mu + psi_S + 1/gamma) * P_N_H
    dS_N_H <- psi_S * P_N_H - (mu + psi_E + 1/gamma) * S_N_H
    dE_N_H <- psi_E * S_N_H - (eta_H + psi_L + 1/gamma) * E_N_H
    dL_N_H <- psi_L * E_N_H - (eta_H + psi_T + 1/gamma) * L_N_H
    dT_N_H <- psi_T * L_N_H - (mu + nu + 1/gamma) * T_N_H
    dR_N_H <- mu * (P_N_H + S_N_H + T_N_H) + eta_H * (E_N_H + L_N_H) - (rho + 1/gamma) * R_N_H
    
    # ODEs - Low risk
    dU_N_L <- q_L * alpha - (lambda_L + 1/gamma) * U_N_L + rho * R_N_L
    dI_N_L <- lambda_L * U_N_L - (sigma + 1/gamma) * I_N_L
    dP_N_L <- sigma * I_N_L - (mu + psi_S + 1/gamma) * P_N_L
    dS_N_L <- psi_S * P_N_L - (mu + psi_E + 1/gamma) * S_N_L
    dE_N_L <- psi_E * S_N_L - (eta_L + psi_L + 1/gamma) * E_N_L
    dL_N_L <- psi_L * E_N_L - (eta_L + psi_T + 1/gamma) * L_N_L
    dT_N_L <- psi_T * L_N_L - (mu + nu + 1/gamma) * T_N_L
    dR_N_L <- mu * (P_N_L + S_N_L + T_N_L) + eta_L * (E_N_L + L_N_L) - (rho + 1/gamma) * R_N_L
    
    # Return as list
    list(c(dU_N_H, dI_N_H, dP_N_H, dS_N_H, dE_N_H, dL_N_H, dT_N_H, dR_N_H,
           dU_N_L, dI_N_L, dP_N_L, dS_N_L, dE_N_L, dL_N_L, dT_N_L, dR_N_L))
  })
}

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

# Transition rate
psi_T <- 1/20 # 20 years (Late latent to Tertiary)
nu <- -log(1-128/399)/40 # 40 years (Death) with around 30% probability of death at tertiary stage

# load calibrated parameters
fit_syphilis_negbin <- readRDS("fit_results_fixedinitialstate_burntin_UK.rds")

# extract all posterior samples for these parameters as a list (permuted = TRUE merges chains)
pars=c('beta', 'phi_beta', 'epsilon', 'rho', 'eta_H_init', 'phi_eta', 'omega', 'sigma', 'mu', 'psi_S', 'psi_E', 'psi_L', 'kappa_D')
posterior_samples <- rstan::extract(fit_syphilis_negbin, pars = pars, permuted = TRUE)

# convert the list of arrays to a data frame where each row is a posterior draw
posterior_df <- as.data.frame(posterior_samples)

# run forward simulations
set.seed(42) # for reproducibility
n_iter <- 1000
random_integers <- sample(1:6000, size = n_iter, replace = FALSE) # draw random integers without replacement
print(random_integers)
n_years <- 15
cases <- matrix(NA, nrow = n_iter, ncol = n_years)
cases_low <- matrix(NA, nrow = n_iter, ncol = n_years)
cases_high <- matrix(NA, nrow = n_iter, ncol = n_years)
cases_primary <- matrix(NA, nrow = n_iter, ncol = n_years)
cases_secondary <- matrix(NA, nrow = n_iter, ncol = n_years)
cases_others <- matrix(NA, nrow = n_iter, ncol = n_years)
cases_diagnosed <- matrix(NA, nrow = n_iter, ncol = n_years)
foi_high <- matrix(NA, nrow = n_iter, ncol = n_years-3)
foi_low <- matrix(NA, nrow = n_iter, ncol = n_years-3)
for (i in 1:n_iter) {
  # times
  t <- seq(17, 17+n_years+1, by = 1)
  t_0 = 0 
  t <- t[-1] # start with t = 21
  
  # get index for posterior sample 
  idx <- random_integers[i]
  
  # initial conditions
  samples_y <- rstan::extract(fit_syphilis_negbin, pars = "y", permuted = TRUE)
  U_N_H = median(samples_y$y[, 17, 1])
  I_N_H = median(samples_y$y[, 17, 2])
  P_N_H = median(samples_y$y[, 17, 3])
  S_N_H = median(samples_y$y[, 17, 4])
  E_N_H = median(samples_y$y[, 17, 5])
  L_N_H = median(samples_y$y[, 17, 6])
  T_N_H = median(samples_y$y[, 17, 7])
  R_N_H = median(samples_y$y[, 17, 8])
  U_N_L = median(samples_y$y[, 17, 9])
  I_N_L = median(samples_y$y[, 17, 10])
  P_N_L = median(samples_y$y[, 17, 11])
  S_N_L = median(samples_y$y[, 17, 12])
  E_N_L = median(samples_y$y[, 17, 13])
  L_N_L = median(samples_y$y[, 17, 14])
  T_N_L = median(samples_y$y[, 17, 15])
  R_N_L = median(samples_y$y[, 17, 16])
  y0 = c(U_N_H=U_N_H, I_N_H=I_N_H, P_N_H=P_N_H, S_N_H=S_N_H, E_N_H=E_N_H, L_N_H=L_N_H, T_N_H=T_N_H, R_N_H=R_N_H,
         U_N_L=U_N_L, I_N_L=I_N_L, P_N_L=P_N_L, S_N_L=S_N_L, E_N_L=E_N_L, L_N_L=L_N_L, T_N_L=T_N_L, R_N_L=R_N_L)
  
  params <- list(
    q_H = q_H, c_H = c_H, c_L = c_L, q_L = q_L,
    t_0 = t_0, alpha = alpha, gamma = gamma,
    sigma = posterior_df$sigma[idx], psi_S = posterior_df$psi_S[idx], psi_E = posterior_df$psi_E[idx], psi_L = posterior_df$psi_L[idx], 
    psi_T = psi_T, nu = nu, beta = posterior_df$beta[idx], phi_beta = posterior_df$phi_beta[idx], epsilon=posterior_df$epsilon[idx], rho=posterior_df$rho[idx], 
    eta_H_init=posterior_df$eta_H_init[idx], phi_eta=posterior_df$phi_eta[idx], omega=posterior_df$omega[idx], mu=posterior_df$mu[idx]
  )
  
  # solve the system
  out <- ode(y = y0, times = t, func = syphilis_model, parms = params)
  out <- as.data.frame(out)
  
  # compute incidences
  incidence <- numeric(n_years)
  incidence_low <- numeric(n_years)
  incidence_high <- numeric(n_years)
  incidence_primary <- numeric(n_years)
  incidence_secondary <- numeric(n_years)
  incidence_others <- numeric(n_years)
  incidence_diagnosed <- numeric(n_years)
  lambda_high <- numeric(n_years-3)
  lambda_low <- numeric(n_years-3)
  for (t in 1:(n_years)) {
    # Trapezoidal rule: (f(a) + f(b)) / 2 * (b - a)
    I = 0.5 * (params$sigma) * (out[t, 3] + out[t + 1, 3] + out[t, 11] + out[t + 1, 11])
    incidence_diagnosed[t] = 0.5 * (params$rho) * (out[t, 9] + out[t + 1, 9] + out[t, 17] + out[t + 1, 17])
    incidence_primary[t] = 0.5 * (params$mu + params$psi_S) * (out[t, 4] + out[t + 1, 4] + out[t, 12] + out[t + 1, 12])
    incidence_secondary[t] = 0.5 * (params$mu + params$psi_E) * (out[t, 5] + out[t + 1, 5] + out[t, 13] + out[t + 1, 13])
    
    isFixed = TRUE
    eta_H_t <- get_eta(t+17, t_0, params$eta_H_init, params$phi_eta, isFixed)
    eta_H_t1 <- get_eta(t+17+1, t_0, params$eta_H_init, params$phi_eta, isFixed)
    eta_L_t <- params$omega * eta_H_t
    eta_L_t1 <- params$omega * eta_H_t1
    incidence_others[t] = 0.5 * ((eta_H_t + params$psi_L) * out[t, 6] + (eta_L_t + params$psi_L) * out[t, 14] 
                                 + (eta_H_t1 + params$psi_L) * out[t + 1, 6] + (eta_L_t1 + params$psi_L) * out[t + 1, 14] 
                                 + (eta_H_t + params$psi_T) * out[t, 7] + (eta_L_t + params$psi_T) * out[t, 15] 
                                 + (eta_H_t1 + params$psi_T) * out[t + 1, 7] + (eta_L_t1 + params$psi_T) * out[t + 1, 15]
                                 + (params$mu + params$nu) * (out[t, 8] + out[t + 1, 8] + out[t, 16] + out[t + 1, 16]))
    
    incidence[t] = I + incidence_primary[t] + incidence_secondary[t] + incidence_others[t] + incidence_diagnosed[t]
    
    if (t <= 12) {
      # Population sizes
      C_H_t <- get_C(out[t, 3], out[t, 4], out[t, 5], out[t, 6])
      C_L_t <- get_C(out[t, 11], out[t, 12], out[t, 13], out[t, 14])
      N_H_t <- get_N(out[t, 2], out[t, 3], out[t, 4], out[t, 5], out[t, 6], out[t, 7], out[t, 8], out[t, 9])
      N_L_t <- get_N(out[t, 10], out[t, 11], out[t, 12], out[t, 13], out[t, 14], out[t, 15], out[t, 16], out[t, 17])
      
      # Mixing
      pi_H_t <- get_pi(c_H, N_H_t, c_L, N_L_t)
      pi_L_t <- get_pi(c_L, N_L_t, c_H, N_H_t)
      lambda_H_t <- get_lambda(t+5, t_0, c_H, params$beta, params$phi_beta, params$epsilon, C_H_t, N_H_t, pi_H_t, C_L_t, N_L_t, pi_L_t, isFixed)
      lambda_L_t <- get_lambda(t+5, t_0, c_L, params$beta, params$phi_beta, params$epsilon, C_L_t, N_L_t, pi_L_t, C_H_t, N_H_t, pi_H_t, isFixed)
      
      lambda_high[t] = lambda_H_t
      lambda_low[t] = lambda_L_t
    }
  }
  
  cases[i,] <- incidence
  cases_low[i,] <- incidence_low
  cases_high[i,] <- incidence_high
  cases_primary[i,] <- incidence_primary
  cases_secondary[i,] <- incidence_secondary
  cases_others[i,] <- incidence_others
  cases_diagnosed[i,] <- incidence_diagnosed
  foi_high[i,] <- lambda_high
  foi_low[i,] <- lambda_low
}

saveRDS(cases,file="cases_baseline_fixed_main.Rda")
saveRDS(cases_primary,file="cases_baseline_primary_fixed_main.Rda")
saveRDS(cases_secondary,file="cases_baseline_secondary_fixed_main.Rda")
saveRDS(cases_others,file="cases_baseline_others_fixed_main.Rda")
saveRDS(cases_diagnosed,file="cases_baseline_diagnosed_fixed_main.Rda")

selected_years <- c(2011:2020, 2023:2024)
all_years <- 2011:2024
year_indices <- which(all_years %in% selected_years)
n_years = 12

# compute quantiles for each row
probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
smr_pred <- t(apply(foi_high, 2, quantile, probs = probs, na.rm = TRUE))
colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
smr_pred <- as.data.frame(smr_pred)

# box plot, output size (8, 6)
df_foi_high <- data.frame(
  group = factor(all_years[year_indices]),
  ymin = smr_pred$X2.5.[1:(n_years)],  # minimum
  lower = smr_pred$X25.[1:(n_years)],  # Q1
  middle = smr_pred$X50.[1:(n_years)], # median
  upper = smr_pred$X75.[1:(n_years)],  # Q3
  ymax = smr_pred$X97.5.[1:(n_years)], # maximum
  observation = cases[1:(n_years)],    # observed
  year = all_years[year_indices]          # year
)

saveRDS(df_foi_high,file="foi_baseline_high_UK.Rda")

# compute quantiles for each row
probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
smr_pred <- t(apply(foi_low, 2, quantile, probs = probs, na.rm = TRUE))
colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
smr_pred <- as.data.frame(smr_pred)

# box plot, output size (8, 6)
df_foi_low <- data.frame(
  group = factor(all_years[year_indices]),
  ymin = smr_pred$X2.5.[1:(n_years)],  # minimum
  lower = smr_pred$X25.[1:(n_years)],  # Q1
  middle = smr_pred$X50.[1:(n_years)], # median
  upper = smr_pred$X75.[1:(n_years)],  # Q3
  ymax = smr_pred$X97.5.[1:(n_years)], # maximum
  observation = cases[1:(n_years)],    # observed
  year = all_years[year_indices]          # year
)

saveRDS(df_foi_low,file="foi_baseline_low_UK.Rda")

# # compute quantiles for each row
# probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
# smr_pred <- t(apply(cases_low, 2, quantile, probs = probs, na.rm = TRUE))
# colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
# smr_pred <- as.data.frame(smr_pred)
# 
# # box plot, output size (8, 6)
# df <- data.frame(
#   group = factor(2026:2040),
#   ymin = smr_pred$X2.5.[1:15],  # minimum
#   lower = smr_pred$X25.[1:15],  # Q1
#   middle = smr_pred$X50.[1:15], # median
#   upper = smr_pred$X75.[1:15],  # Q3
#   ymax = smr_pred$X97.5.[1:15], # maximum
#   year = 2026:2040              # year
# )
# 
# saveRDS(df,file="data_baseline_lowrisk_fixed_main.Rda")
# 
# # compute quantiles for each row
# probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
# smr_pred <- t(apply(cases_high, 2, quantile, probs = probs, na.rm = TRUE))
# colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
# smr_pred <- as.data.frame(smr_pred)
# 
# # box plot, output size (8, 6)
# df <- data.frame(
#   group = factor(2026:2040),
#   ymin = smr_pred$X2.5.[1:15],  # minimum
#   lower = smr_pred$X25.[1:15],  # Q1
#   middle = smr_pred$X50.[1:15], # median
#   upper = smr_pred$X75.[1:15],  # Q3
#   ymax = smr_pred$X97.5.[1:15], # maximum
#   year = 2026:2040              # year
# )
# 
# saveRDS(df,file="data_baseline_highrisk_fixed_main.Rda")

# compute quantiles for each row
probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
smr_pred <- t(apply(cases, 2, quantile, probs = probs, na.rm = TRUE))
colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
smr_pred <- as.data.frame(smr_pred)

# box plot, output size (8, 6)
df <- data.frame(
  group = factor(2026:2040),
  ymin = smr_pred$X2.5.[1:15],  # minimum
  lower = smr_pred$X25.[1:15],  # Q1
  middle = smr_pred$X50.[1:15], # median
  upper = smr_pred$X75.[1:15],  # Q3
  ymax = smr_pred$X97.5.[1:15], # maximum
  year = 2026:2040              # year
)

saveRDS(df,file="data_baseline_fixed_main.Rda")

ggplot(df, aes(x = group, ymin = ymin, lower = ymin, middle = middle, upper = ymax, ymax = ymax, color = 'Baseline')) +
  geom_boxplot(stat = "identity", fill = "salmon") +
  labs(x = "Year", y = "Annual Number of Cases") +
  scale_color_manual(name = NULL, values = c("Baseline" = "darkred")) +
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    plot.margin = margin(t = 5, r = 0, b = 0, l = 0)
  )