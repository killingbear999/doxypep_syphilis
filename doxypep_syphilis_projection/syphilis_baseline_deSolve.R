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
  if (isFixed && t > 20) {
    t <- 20
  }
  return(c * beta * (1 + phi_beta * (t - t_0)) * (epsilon * C_target / N_target + (1 - epsilon) *
       (pi_target * C_target / N_target + pi_remain * C_remain / N_remain)))
}

get_eta <- function(t, t_0, eta_H_init, phi_eta, isFixed) {
  if (isFixed && t > 20) {
    t <- 20
  }
  return(eta_H_init * (1 + phi_eta * (t - t_0)))
}

# Enforce non-negativity via events
non_negative <- function(time, state, parms) {
  state[state < 0] <- 0
  return(state)
}

# ODE system
syphilis_model <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    # two baselines: 1. the inferred trends in the time-varying behavioural parameters stabilise
    #                2. the trends continue until the end of the modelled period
    
    isFixed <- FALSE
    
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
alpha <- 2524

# Proportion of the MSM population in group j
q_H <- 0.207
q_L <- 0.793

# Annual rate of partner change in group j
c_H <- 14.866
c_L <- 1.989

# Years spent in the sexually-active population
gamma <- 50

# Transition rate
psi_T <- 1/20 # 20 years (Late latent to Tertiary)
nu <- -log(1-128/399)/40 # 40 years (Death) with around 30% probability of death at tertiary stage

# load calibrated parameters
fit_syphilis_negbin <- readRDS("fit_results_fixedinitialstate_burntin_upper.rds")

# extract all posterior samples for these parameters as a list (permuted = TRUE merges chains)
pars=c('beta', 'phi_beta', 'epsilon', 'rho', 'eta_H_init', 'phi_eta', 'omega', 'sigma', 'mu', 'psi_S', 'psi_E', 'psi_L', 'kappa_D')
posterior_samples <- rstan::extract(fit_syphilis_negbin, pars = pars, permuted = TRUE)

# convert the list of arrays to a data frame where each row is a posterior draw
posterior_df <- as.data.frame(posterior_samples)

# run forward simulations
set.seed(42) # for reproducibility
n_iter <- 1000
random_integers <- sample(1:2000, size = n_iter, replace = FALSE) # draw random integers without replacement
# print(random_integers)
n_years <- 15
cases <- matrix(NA, nrow = n_iter, ncol = n_years)
for (i in 1:n_iter) {
  # times
  t <- seq(20, 20+n_years+1, by = 1)
  t_0 = 0 
  t <- t[-1] # start with t = 21
  
  # get index for posterior sample 
  idx <- random_integers[i]
  
  # initial conditions
  samples_y <- rstan::extract(fit_syphilis_negbin, pars = "y", permuted = TRUE)
  U_N_H = median(samples_y$y[, 20, 1])
  I_N_H = median(samples_y$y[, 20, 2])
  P_N_H = median(samples_y$y[, 20, 3])
  S_N_H = median(samples_y$y[, 20, 4])
  E_N_H = median(samples_y$y[, 20, 5])
  L_N_H = median(samples_y$y[, 20, 6])
  T_N_H = median(samples_y$y[, 20, 7])
  R_N_H = median(samples_y$y[, 20, 8])
  U_N_L = median(samples_y$y[, 20, 9])
  I_N_L = median(samples_y$y[, 20, 10])
  P_N_L = median(samples_y$y[, 20, 11])
  S_N_L = median(samples_y$y[, 20, 12])
  E_N_L = median(samples_y$y[, 20, 13])
  L_N_L = median(samples_y$y[, 20, 14])
  T_N_L = median(samples_y$y[, 20, 15])
  R_N_L = median(samples_y$y[, 20, 16])
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
  for (t in 1:(n_years)) {
    # Trapezoidal rule: (f(a) + f(b)) / 2 * (b - a)
    incidence[t] = 0.5 * params$rho * (out[t, 9] + out[t + 1, 9] + out[t, 17] + out[t + 1, 17]);
  }
  
  # apply negative binomial distribution to obtain case
  kappa_D <- posterior_df$kappa_D[i]
  cases[i,] <- rnbinom(n = length(incidence), size = kappa_D, mu = incidence)
}

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

# saveRDS(df,file="data_baseline_timevarying.Rda")
# df_baseline <- readRDS(file="data_baseline_timevarying.Rda")

ggplot(df, aes(x = group, ymin = lower, lower = lower, middle = middle, upper = upper, ymax = upper, color = 'Baseline')) +
  geom_boxplot(stat = "identity", fill = "salmon") +
  labs(x = "Year", y = "Annual Incidence of Diagnosed Cases") +
  scale_color_manual(name = NULL, values = c("Baseline" = "darkred")) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.85),
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