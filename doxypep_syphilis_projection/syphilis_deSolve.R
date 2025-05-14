library(deSolve)
library(ggplot2)

# Helper functions
get_C <- function(I, P, S, E) {
  I + P + S + E
}

get_N <- function(U, I, P, S, E, L, T, R) {
  U + I + P + S + E + L + T + R
}

get_pi <- function(c_target, N_target, c_remain, N_remain) {
  (c_target * N_target) / (c_target * N_target + c_remain * N_remain)
}

get_lambda <- function(t, t_0, c, beta, phi_beta, epsilon, C_target, N_target, pi_target, C_remain, N_remain, pi_remain) {
  c * beta * (1 + phi_beta * (t - t_0)) *
    (epsilon * C_target / N_target + (1 - epsilon) *
       (pi_target * C_target / N_target + pi_remain * C_remain / N_remain))
}

get_eta <- function(t, t_0, eta_H_init, phi_eta) {
  eta_H_init * (1 + phi_eta * (t - t_0))
}

# ODE system
syphilis_model <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    # two baselines: 1. the inferred trends in the time-varying behavioural parameters stabilise (t=2018 and t0=2004)
    #                2. the trends continue until the end of the modelled period (do not fix t and t_0)
    
    # Population sizes
    C_H <- get_C(I_N_H, P_N_H, S_N_H, E_N_H)
    C_L <- get_C(I_N_L, P_N_L, S_N_L, E_N_L)
    N_H <- get_N(U_N_H, I_N_H, P_N_H, S_N_H, E_N_H, L_N_H, T_N_H, R_N_H)
    N_L <- get_N(U_N_L, I_N_L, P_N_L, S_N_L, E_N_L, L_N_L, T_N_L, R_N_L)
    
    # Mixing
    pi_H <- get_pi(c_H, N_H, c_L, N_L)
    pi_L <- get_pi(c_L, N_L, c_H, N_H)
    lambda_H <- get_lambda(2018, 2004, c_H, beta, phi_beta, epsilon, C_H, N_H, pi_H, C_L, N_L, pi_L)
    lambda_L <- get_lambda(2018, 2004, c_L, beta, phi_beta, epsilon, C_L, N_L, pi_L, C_H, N_H, pi_H)
    
    # Screening
    eta_H <- get_eta(2018, 2004, eta_H_init, phi_eta)
    eta_L <- omega * eta_H
    
    # ODEs - High risk
    dU_N_H <- q_H * alpha - (lambda_H + 1/gamma) * U_N_H + rho * R_N_H
    dI_N_H <- lambda_H * U_N_H - (sigma + 1/gamma) * I_N_H
    dP_N_H <- sigma * I_N_H - (mu + psi_S + 1/gamma) * P_N_H
    dS_N_H <- psi_S * P_N_H - (mu + psi_E + 1/gamma) * S_N_H
    dE_N_H <- psi_E * S_N_H - (eta_H + psi_L + 1/gamma) * E_N_H
    dL_N_H <- psi_L * E_N_H - (eta_H + psi_T + 1/gamma) * L_N_H
    dT_N_H <- psi_T * L_N_H - (mu + beta_nu * nu + 1/gamma) * T_N_H
    dR_N_H <- mu * (P_N_H + S_N_H + T_N_H) + eta_H * (E_N_H + L_N_H) - (rho + 1/gamma) * R_N_H
    
    # ODEs - Low risk
    dU_N_L <- q_L * alpha - (lambda_L + 1/gamma) * U_N_L + rho * R_N_L
    dI_N_L <- lambda_L * U_N_L - (sigma + 1/gamma) * I_N_L
    dP_N_L <- sigma * I_N_L - (mu + psi_S + 1/gamma) * P_N_L
    dS_N_L <- psi_S * P_N_L - (mu + psi_E + 1/gamma) * S_N_L
    dE_N_L <- psi_E * S_N_L - (eta_L + psi_L + 1/gamma) * E_N_L
    dL_N_L <- psi_L * E_N_L - (eta_L + psi_T + 1/gamma) * L_N_L
    dT_N_L <- psi_T * L_N_L - (mu + beta_nu * nu + 1/gamma) * T_N_L
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

# transition rate
sigma <- 365/23 # 23 days (Incubation to Primary)
psi_S <- 365/42 # 6 weeks (Primary to Secondary)
psi_E <- 365/(365/2) # 6 months (Secondary to Early latent)
psi_L <- 1 # 1 year (Early latent to Late latent)
psi_T <- 1/20 # 20 years (Late latent to Tertiary)
nu <- 1/40 # 40 years (Death)
beta_nu <- 128/399 # Probability of death at tertiary stage

# load calibrated parameters
fit_syphilis_negbin <- readRDS("fit_results_main.rds")

# extract all posterior samples for these parameters as a list (permuted = TRUE merges chains)
pars=c('beta', 'phi_beta', 'epsilon', 'rho', 'eta_H_init', 'phi_eta', 'omega', 'mu', 'kappa_D')
posterior_samples <- rstan::extract(fit_syphilis_negbin, pars = pars, permuted = TRUE)

# convert the list of arrays to a data frame where each row is a posterior draw
posterior_df <- as.data.frame(posterior_samples)

# run forward simulations
set.seed(42) # for reproducibility
n_iter <- 2000
random_integers <- sample(1:12000, size = n_iter, replace = FALSE) # draw random integers without replacement
print(random_integers)
n_years <- 16
cases <- matrix(NA, nrow = n_iter, ncol = n_years-1)
for (i in 1:n_iter) {
  # times
  t <- seq(0, n_years, by = 1)
  t_0 = 0 
  t <- t[-1]
  
  # get index for posterior sample 
  idx <- random_integers[i]
  
  # initial conditions
  samples_y <- rstan::extract(fit_syphilis_negbin, pars = "y", permuted = TRUE)
  U_N_H <- median(samples_y$y[, 15, 1])
  I_N_H = median(samples_y$y[, 15, 2])
  P_N_H = median(samples_y$y[, 15, 3])
  S_N_H = median(samples_y$y[, 15, 4])
  E_N_H = median(samples_y$y[, 15, 5])
  L_N_H = median(samples_y$y[, 15, 6])
  T_N_H = median(samples_y$y[, 15, 7])
  R_N_H = median(samples_y$y[, 15, 8])
  U_N_L = median(samples_y$y[, 15, 9])
  I_N_L = median(samples_y$y[, 15, 10])
  P_N_L = median(samples_y$y[, 15, 11])
  S_N_L = median(samples_y$y[, 15, 12])
  E_N_L = median(samples_y$y[, 15, 13])
  L_N_L = median(samples_y$y[, 15, 14])
  T_N_L = median(samples_y$y[, 15, 15])
  R_N_L = median(samples_y$y[, 15, 16])
  y0 = c(U_N_H=U_N_H, I_N_H=I_N_H, P_N_H=P_N_H, S_N_H=S_N_H, E_N_H=E_N_H, L_N_H=L_N_H, T_N_H=T_N_H, R_N_H=R_N_H,
         U_N_L=U_N_L, I_N_L=I_N_L, P_N_L=P_N_L, S_N_L=S_N_L, E_N_L=E_N_L, L_N_L=L_N_L, T_N_L=T_N_L, R_N_L=R_N_L)
  
  params <- list(
    q_H = q_H, c_H = c_H, c_L = c_L, q_L = q_L,
    t_0 = 0, alpha = alpha, gamma = gamma,
    sigma = sigma, psi_S = psi_S, psi_E = psi_E, psi_L = psi_L, psi_T = psi_T,
    nu = nu, beta = posterior_df$beta[idx], phi_beta = posterior_df$phi_beta[idx], epsilon=posterior_df$epsilon[idx], rho=posterior_df$rho[idx], 
    eta_H_init=posterior_df$eta_H_init[idx], phi_eta=posterior_df$phi_eta[idx], omega=posterior_df$omega[idx], mu=posterior_df$mu[idx], beta_nu=beta_nu
  )
  
  # solve the system
  out <- ode(y = y0, times = c(t_0, t), func = syphilis_model, parms = params)
  out <- as.data.frame(out)
  
  # compute incidences
  incidence <- numeric(n_years - 1)
  for (t in 1:(n_years - 1)) {
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

ggplot(df, aes(x = group, ymin = lower, lower = lower, middle = middle, upper = upper, ymax = upper, color = 'Baseline')) +
  geom_boxplot(stat = "identity", fill = "salmon") +
  labs(x = "Year", y = "Annual Incidence of Diagnosed") +
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