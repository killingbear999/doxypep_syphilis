library(deSolve)
library(ggplot2)

# Helper functions
get_C <- function(I_N, P_N, S_N, E_N, I_X, P_X, S_X, E_X, I_D, P_D, S_D, E_D, I_M, P_M, S_M, E_M) {
  return(I_N + P_N + S_N + E_N + I_X + P_X + S_X + E_X + I_D + P_D + S_D + E_D + I_M + P_M + S_M + E_M)
}

get_N <- function(U_N, I_N, P_N, S_N, E_N, L_N, T_N, R_N, U_X, I_X, P_X, S_X, E_X, L_X, T_X, R_X, U_D, I_D, P_D, S_D, E_D, L_D, T_D, R_D, U_M, I_M, P_M, S_M, E_M, L_M, T_M, R_M) {
  return(U_N + I_N + P_N + S_N + E_N + L_N + T_N + R_N +
    U_X + I_X + P_X + S_X + E_X + L_X + T_X + R_X +
    U_D + I_D + P_D + S_D + E_D + L_D + T_D + R_D +
    U_M + I_M + P_M + S_M + E_M + L_M + T_M + R_M)
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

# ODE system
doxypep_model <- function(t, y, parameters) {
  with(as.list(c(y, parameters)), {
    # two cases: 1. the inferred trends in the time-varying behavioural parameters stabilise
    #            2. the trends continue until the end of the modelled period
    isFixed <- TRUE
    
    C_H <- get_C(I_N_H, P_N_H, S_N_H, E_N_H, I_X_H, P_X_H, S_X_H, E_X_H, I_D_H, P_D_H, S_D_H, E_D_H, I_M_H, P_M_H, S_M_H, E_M_H)
    C_L <- get_C(I_N_L, P_N_L, S_N_L, E_N_L, I_X_L, P_X_L, S_X_L, E_X_L, I_D_L, P_D_L, S_D_L, E_D_L, I_M_L, P_M_L, S_M_L, E_M_L)
    N_H <- get_N(U_N_H, I_N_H, P_N_H, S_N_H, E_N_H, L_N_H, T_N_H, R_N_H, U_X_H, I_X_H, P_X_H, S_X_H, E_X_H, L_X_H, T_X_H, R_X_H,
                 U_D_H, I_D_H, P_D_H, S_D_H, E_D_H, L_D_H, T_D_H, R_D_H, U_M_H, I_M_H, P_M_H, S_M_H, E_M_H, L_M_H, T_M_H, R_M_H)
    N_L <- get_N(U_N_L, I_N_L, P_N_L, S_N_L, E_N_L, L_N_L, T_N_L, R_N_L, U_X_L, I_X_L, P_X_L, S_X_L, E_X_L, L_X_L, T_X_L, R_X_L,
                 U_D_L, I_D_L, P_D_L, S_D_L, E_D_L, L_D_L, T_D_L, R_D_L, U_M_L, I_M_L, P_M_L, S_M_L, E_M_L, L_M_L, T_M_L, R_M_L)
    
    pi_H <- get_pi(c_H, N_H, c_L, N_L)
    pi_L <- get_pi(c_L, N_L, c_H, N_H)
    lambda_H <- get_lambda(t, t_0, c_H, beta, phi_beta, epsilon, C_H, N_H, pi_H, C_L, N_L, pi_L, isFixed)
    lambda_L <- get_lambda(t, t_0, c_L, beta, phi_beta, epsilon, C_L, N_L, pi_L, C_H, N_H, pi_H, isFixed)
    
    eta_H <- get_eta(t, t_0, eta_H_init, phi_eta, isFixed)
    eta_L <- omega * eta_H
    
    # ODEs
    # high-risk group
    # non-doxy-pep (N)
    dU_N_H = q_H * alpha * (1 - p_DbE) - (lambda_H + p_DoS_H * eta_H + 1/gamma) * U_N_H + (1 - p_DoD_H) * rho * R_N_H + xi_N * U_D_H
    dI_N_H = lambda_H * U_N_H - (sigma + 1/gamma) * I_N_H + xi_N * I_D_H
    dP_N_H = sigma * I_N_H - (mu + psi_S + 1/gamma) * P_N_H + xi_N * P_D_H
    dS_N_H = psi_S * P_N_H - (mu + psi_E + 1/gamma) * S_N_H + xi_N * S_D_H
    dE_N_H = psi_E * S_N_H - (eta_H + psi_L + 1/gamma) * E_N_H + xi_N * E_D_H
    dL_N_H = psi_L * E_N_H - (eta_H + psi_T + 1/gamma) * L_N_H + xi_N * L_D_H
    dT_N_H = psi_T * L_N_H - (mu + nu + 1/gamma) * T_N_H + xi_N * T_D_H
    dR_N_H = mu * (P_N_H + S_N_H + T_N_H) + eta_H * (E_N_H + L_N_H) - (rho + 1/gamma) * R_N_H + xi_N * R_D_H
    
    # doxy-inconsistent (X)
    dU_X_H = xi_X * U_D_H + rho * R_X_H - ((1 - zeta * e_d) * lambda_H + 1/gamma) * U_X_H
    dI_X_H = xi_X * I_D_H + (1 - zeta * e_d) * lambda_H * U_X_H - (sigma + 1/gamma) * I_X_H
    dP_X_H = xi_X * P_D_H + sigma * I_X_H - (mu + psi_S + 1/gamma) * P_X_H
    dS_X_H = xi_X * S_D_H + psi_S * P_X_H - (mu + psi_E + 1/gamma) * S_X_H
    dE_X_H = xi_X * E_D_H + psi_E * S_X_H - (eta_H + psi_L + 1/gamma) * E_X_H
    dL_X_H = xi_X * L_D_H + psi_L * E_X_H - (eta_H + psi_T + 1/gamma) * L_X_H
    dT_X_H = xi_X * T_D_H + psi_T * L_X_H - (mu + nu + 1/gamma) * T_X_H
    dR_X_H = xi_X * R_D_H + mu * (P_X_H + S_X_H + T_X_H) + eta_H * (E_X_H + L_X_H) - (rho + 1/gamma) * R_X_H
    
    # doxy-pep (D)
    dU_D_H = q_H * alpha * p_DbE + (p_DoS_H * eta_H) * U_N_H + p_DoD_H * rho * R_N_H + rho * R_D_H - ((1 - e_d) * lambda_H + 1/gamma + xi_X + xi_N + xi_M) * U_D_H
    dI_D_H = (1 - e_d) * lambda_H * U_D_H - (sigma + 1/gamma + xi_X + xi_N + xi_M) * I_D_H
    dP_D_H = sigma * I_D_H - (mu + psi_S + 1/gamma + xi_X + xi_N + xi_M) * P_D_H
    dS_D_H = psi_S * P_D_H - (mu + psi_E + 1/gamma + xi_X + xi_N + xi_M) * S_D_H
    dE_D_H = psi_E * S_D_H - (eta_H + psi_L + 1/gamma + xi_X + xi_N + xi_M) * E_D_H
    dL_D_H = psi_L * E_D_H - (eta_H + psi_T + 1/gamma + xi_X + xi_N + xi_M) * L_D_H
    dT_D_H = psi_T * L_D_H - (mu + nu + 1/gamma + xi_X + xi_N + xi_M) * T_D_H
    dR_D_H = mu * (P_D_H + S_D_H + T_D_H) + eta_H * (E_D_H + L_D_H) - (rho + 1/gamma + xi_X + xi_N + xi_M) * R_D_H
    
    # doxy_intolerant (M)
    dU_M_H = xi_M * U_D_H + rho * R_M_H - (lambda_H + 1/gamma) * U_M_H
    dI_M_H = xi_M * I_D_H + lambda_H * U_M_H - (sigma + 1/gamma) * I_M_H
    dP_M_H = xi_M * P_D_H + sigma * I_M_H - (mu + psi_S + 1/gamma) * P_M_H
    dS_M_H = xi_M * S_D_H + psi_S * P_M_H - (mu + psi_E + 1/gamma) * S_M_H
    dE_M_H = xi_M * E_D_H + psi_E * S_M_H - (eta_H + psi_L + 1/gamma) * E_M_H
    dL_M_H = xi_M * L_D_H + psi_L * E_M_H - (eta_H + psi_T + 1/gamma) * L_M_H
    dT_M_H = xi_M * T_D_H + psi_T * L_M_H - (mu + nu + 1/gamma) * T_M_H
    dR_M_H = xi_M * R_D_H + mu * (P_M_H + S_M_H + T_M_H) + eta_H * (E_M_H + L_M_H) - (rho + 1/gamma) * R_M_H
    
    # low-risk group
    # non-doxy-pep (N)
    dU_N_L = q_L * alpha * (1 - p_DbE) - (lambda_L + p_DoS_L * eta_L + 1/gamma) * U_N_L + (1 - p_DoD_L) * rho * R_N_L + xi_N * U_D_L
    dI_N_L = lambda_L * U_N_L - (sigma + 1/gamma) * I_N_L + xi_N * I_D_L
    dP_N_L = sigma * I_N_L - (mu + psi_S + 1/gamma) * P_N_L + xi_N * P_D_L
    dS_N_L = psi_S * P_N_L - (mu + psi_E + 1/gamma) * S_N_L + xi_N * S_D_L
    dE_N_L = psi_E * S_N_L - (eta_L + psi_L + 1/gamma) * E_N_L + xi_N * E_D_L
    dL_N_L = psi_L * E_N_L - (eta_L + psi_T + 1/gamma) * L_N_L + xi_N * L_D_L
    dT_N_L = psi_T * L_N_L - (mu + nu + 1/gamma) * T_N_L + xi_N * T_D_L
    dR_N_L = mu * (P_N_L + S_N_L + T_N_L) + eta_L * (E_N_L + L_N_L) - (rho + 1/gamma) * R_N_L + xi_N * R_D_L
    
    # doxy-inconsistent (X)
    dU_X_L = xi_X * U_D_L + rho * R_X_L - ((1 - zeta * e_d) * lambda_L + 1/gamma) * U_X_L
    dI_X_L = xi_X * I_D_L + (1 - zeta * e_d) * lambda_L * U_X_L - (sigma + 1/gamma) * I_X_L
    dP_X_L = xi_X * P_D_L + sigma * I_X_L - (mu + psi_S + 1/gamma) * P_X_L
    dS_X_L = xi_X * S_D_L + psi_S * P_X_L - (mu + psi_E + 1/gamma) * S_X_L
    dE_X_L = xi_X * E_D_L + psi_E * S_X_L - (eta_L + psi_L + 1/gamma) * E_X_L
    dL_X_L = xi_X * L_D_L + psi_L * E_X_L - (eta_L + psi_T + 1/gamma) * L_X_L
    dT_X_L = xi_X * T_D_L + psi_T * L_X_L - (mu + nu + 1/gamma) * T_X_L
    dR_X_L = xi_X * R_D_L + mu * (P_X_L + S_X_L + T_X_L) + eta_L * (E_X_L + L_X_L) - (rho + 1/gamma) * R_X_L
    
    # doxy-pep (D)
    dU_D_L = q_L * alpha * p_DbE + (p_DoS_L * eta_L) * U_N_L + p_DoD_L * rho * R_N_L + rho * R_D_L - ((1 - e_d) * lambda_L + 1/gamma + xi_X + xi_N + xi_M) * U_D_L
    dI_D_L = (1 - e_d) * lambda_L * U_D_L - (sigma + 1/gamma + xi_X + xi_N + xi_M) * I_D_L
    dP_D_L = sigma * I_D_L - (mu + psi_S + 1/gamma + xi_X + xi_N + xi_M) * P_D_L
    dS_D_L = psi_S * P_D_L - (mu + psi_E + 1/gamma + xi_X + xi_N + xi_M) * S_D_L
    dE_D_L = psi_E * S_D_L - (eta_L + psi_L + 1/gamma + xi_X + xi_N + xi_M) * E_D_L
    dL_D_L = psi_L * E_D_L - (eta_L + psi_T + 1/gamma + xi_X + xi_N + xi_M) * L_D_L
    dT_D_L = psi_T * L_D_L - (mu + nu + 1/gamma + xi_X + xi_N + xi_M) * T_D_L
    dR_D_L = mu * (P_D_L + S_D_L + T_D_L) + eta_L * (E_D_L + L_D_L) - (rho + 1/gamma + xi_X + xi_N + xi_M) * R_D_L
    
    # doxy_intolerant (M)
    dU_M_L = xi_M * U_D_L + rho * R_M_L - (lambda_L + 1/gamma) * U_M_L
    dI_M_L = xi_M * I_D_L + lambda_L * U_M_L - (sigma + 1/gamma) * I_M_L
    dP_M_L = xi_M * P_D_L + sigma * I_M_L - (mu + psi_S + 1/gamma) * P_M_L
    dS_M_L = xi_M * S_D_L + psi_S * P_M_L - (mu + psi_E + 1/gamma) * S_M_L
    dE_M_L = xi_M * E_D_L + psi_E * S_M_L - (eta_L + psi_L + 1/gamma) * E_M_L
    dL_M_L = xi_M * L_D_L + psi_L * E_M_L - (eta_L + psi_T + 1/gamma) * L_M_L
    dT_M_L = xi_M * T_D_L + psi_T * L_M_L - (mu + nu + 1/gamma) * T_M_L
    dR_M_L = xi_M * R_D_L + mu * (P_M_L + S_M_L + T_M_L) + eta_L * (E_M_L + L_M_L) - (rho + 1/gamma) * R_M_L
    
    list(c(
      dU_N_H, dI_N_H, dP_N_H, dS_N_H, dE_N_H, dL_N_H, dT_N_H, dR_N_H,
      dU_X_H, dI_X_H, dP_X_H, dS_X_H, dE_X_H, dL_X_H, dT_X_H, dR_X_H,
      dU_D_H, dI_D_H, dP_D_H, dS_D_H, dE_D_H, dL_D_H, dT_D_H, dR_D_H,
      dU_M_H, dI_M_H, dP_M_H, dS_M_H, dE_M_H, dL_M_H, dT_M_H, dR_M_H,
      dU_N_L, dI_N_L, dP_N_L, dS_N_L, dE_N_L, dL_N_L, dT_N_L, dR_N_L,
      dU_X_L, dI_X_L, dP_X_L, dS_X_L, dE_X_L, dL_X_L, dT_X_L, dR_X_L,
      dU_D_L, dI_D_L, dP_D_L, dS_D_L, dE_D_L, dL_D_L, dT_D_L, dR_D_L,
      dU_M_L, dI_M_L, dP_M_L, dS_M_L, dE_M_L, dL_M_L, dT_M_L, dR_M_L
    ))
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

# Efficacy of doxycycline against infections
e_d <- 0.83

# Scaling factor accounting for doxycycline inefficacy due to inconsistent and irregular usage
zeta <- 0.33

# Intolerance rate of doxycycline
xi_M <- 0.0135

# Discontinuation rate of doxy-PEP
xi_N <- 0.362

# Suboptimal adherence rate of doxy-PEP
xi_X <- 0.420

# Probability of uptake of doxycycline before entry into the sexually-active population
p_DbE <- 0

# Probability of uptake of doxycycline on diagnosis in group H
p_DoD_H <- 0.1

# Probability of uptake of doxycycline on diagnosis in group L
p_DoD_L <- 0

# Probability of uptake of doxycycline on screening with negative results in group H
p_DoS_H <- 0

# Probability of uptake of doxycycline on screening with negative results in group L
p_DoS_L <- 0

# load calibrated parameters
fit_syphilis_negbin <- readRDS("fit_results_fixedinitialstate_burntin_main.rds")

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
prescriptions <- matrix(NA, nrow = n_iter, ncol = n_years)
for (i in 1:n_iter) {
  # times
  t <- seq(20, 20+n_years+1, by = 1)
  t_0 = 0 
  t <- t[-1]
  
  # get index for posterior sample 
  idx <- random_integers[i]
  
  # initial conditions
  samples_y <- rstan::extract(fit_syphilis_negbin, pars = "y", permuted = TRUE)
  # high-risk group
  # non-doxy-pep (N)
  U_N_H = median(samples_y$y[, 20, 1])
  I_N_H = median(samples_y$y[, 20, 2])
  P_N_H = median(samples_y$y[, 20, 3])
  S_N_H = median(samples_y$y[, 20, 4])
  E_N_H = median(samples_y$y[, 20, 5])
  L_N_H = median(samples_y$y[, 20, 6])
  T_N_H = median(samples_y$y[, 20, 7])
  R_N_H = median(samples_y$y[, 20, 8])
  # doxy-inconsistent (X)
  U_X_H = 0
  I_X_H = 0
  P_X_H = 0
  S_X_H = 0
  E_X_H = 0
  L_X_H = 0
  T_X_H = 0
  R_X_H = 0
  # doxy-pep (D)
  U_D_H = 0
  I_D_H = 0
  P_D_H = 0
  S_D_H = 0
  E_D_H = 0
  L_D_H = 0
  T_D_H = 0
  R_D_H = 0
  # doxy_intolerant (M)
  U_M_H = 0
  I_M_H = 0
  P_M_H = 0
  S_M_H = 0
  E_M_H = 0
  L_M_H = 0
  T_M_H = 0
  R_M_H = 0
  # low-risk group
  # non-doxy-pep (N)
  U_N_L = median(samples_y$y[, 20, 9])
  I_N_L = median(samples_y$y[, 20, 10])
  P_N_L = median(samples_y$y[, 20, 11])
  S_N_L = median(samples_y$y[, 20, 12])
  E_N_L = median(samples_y$y[, 20, 13])
  L_N_L = median(samples_y$y[, 20, 14])
  T_N_L = median(samples_y$y[, 20, 15])
  R_N_L = median(samples_y$y[, 20, 16])
  # doxy-inconsistent (X)
  U_X_L = 0
  I_X_L = 0
  P_X_L = 0
  S_X_L = 0
  E_X_L = 0
  L_X_L = 0
  T_X_L = 0
  R_X_L = 0
  # doxy-pep (D)
  U_D_L = 0
  I_D_L = 0
  P_D_L = 0
  S_D_L = 0
  E_D_L = 0
  L_D_L = 0
  T_D_L = 0
  R_D_L = 0
  # doxy_intolerant (M)
  U_M_L = 0
  I_M_L = 0
  P_M_L = 0
  S_M_L = 0
  E_M_L = 0
  L_M_L = 0
  T_M_L = 0
  R_M_L = 0
  y0 = c(U_N_H=U_N_H, I_N_H=I_N_H, P_N_H=P_N_H, S_N_H=S_N_H, E_N_H=E_N_H, L_N_H=L_N_H, T_N_H=T_N_H, R_N_H=R_N_H,
         U_X_H=U_X_H, I_X_H=I_X_H, P_X_H=P_X_H, S_X_H=S_X_H, E_X_H=E_X_H, L_X_H=L_X_H, T_X_H=T_X_H, R_X_H=R_X_H,
         U_D_H=U_D_H, I_D_H=I_D_H, P_D_H=P_D_H, S_D_H=S_D_H, E_D_H=E_D_H, L_D_H=L_D_H, T_D_H=T_D_H, R_D_H=R_D_H,
         U_M_H=U_M_H, I_M_H=I_M_H, P_M_H=P_M_H, S_M_H=S_M_H, E_M_H=E_M_H, L_M_H=L_M_H, T_M_H=T_M_H, R_M_H=R_M_H,
         U_N_L=U_N_L, I_N_L=I_N_L, P_N_L=P_N_L, S_N_L=S_N_L, E_N_L=E_N_L, L_N_L=L_N_L, T_N_L=T_N_L, R_N_L=R_N_L,
         U_X_L=U_X_L, I_X_L=I_X_L, P_X_L=P_X_L, S_X_L=S_X_L, E_X_L=E_X_L, L_X_L=L_X_L, T_X_L=T_X_L, R_X_L=R_X_L,
         U_D_L=U_D_L, I_D_L=I_D_L, P_D_L=P_D_L, S_D_L=S_D_L, E_D_L=E_D_L, L_D_L=L_D_L, T_D_L=T_D_L, R_D_L=R_D_L,
         U_M_L=U_M_L, I_M_L=I_M_L, P_N_L=P_M_L, S_M_L=S_M_L, E_M_L=E_M_L, L_M_L=L_M_L, T_M_L=T_M_L, R_M_L=R_M_L)
  
  params <- list(
    q_H = q_H, c_H = c_H, c_L = c_L, q_L = q_L,
    t_0 = t_0, alpha = alpha, gamma = gamma,
    sigma = posterior_df$sigma[idx], psi_S = posterior_df$psi_S[idx], psi_E = posterior_df$psi_E[idx], psi_L = posterior_df$psi_L[idx], 
    psi_T = psi_T, nu = nu, beta = posterior_df$beta[idx], phi_beta = posterior_df$phi_beta[idx], epsilon=posterior_df$epsilon[idx], rho=posterior_df$rho[idx], 
    eta_H_init=posterior_df$eta_H_init[idx], phi_eta=posterior_df$phi_eta[idx], omega=posterior_df$omega[idx], mu=posterior_df$mu[idx],
    e_d = e_d, zeta = zeta, xi_M = xi_M, xi_N = xi_N, xi_X = xi_X, p_DbE = p_DbE, p_DoD_H = p_DoD_H, p_DoD_L = p_DoD_L,p_DoS_H = p_DoS_H, p_DoS_L = p_DoS_L
  )
  
  # solve the system
  out <- ode(y = y0, times = t, func = doxypep_model, parms = params)
  out <- as.data.frame(out)
  
  # compute incidences and prescriptions
  incidence <- numeric(n_years)
  presctiption <- numeric(n_years)
  
  for (t in 1:(n_years)) {
    isFixed = TRUE
    
    # Trapezoidal rule: (f(a) + f(b)) / 2 * (b - a)
    incidence[t] = 0.5 * params$rho * (out[t, 9] + out[t + 1, 9] + out[t, 17] + out[t + 1, 17] + out[t, 25] + out[t + 1, 25] + out[t, 33] + out[t + 1, 33] + 
                                         out[t, 41] + out[t + 1, 41] + out[t, 49] + out[t + 1, 49] + out[t, 57] + out[t + 1, 57] + out[t, 65] + out[t + 1, 65]);
    
    eta_H_t <- get_eta(t+20, t_0, params$eta_H_init, params$phi_eta, isFixed)
    eta_H_t1 <- get_eta(t+20+1, t_0, params$eta_H_init, params$phi_eta, isFixed)
    Y_U_N_H <- 0.5 * (eta_H_t * out[t, 2] + eta_H_t1 * out[t + 1, 2])
    Y_D_N_H <- 0.5 * params$rho * (out[t, 9] + out[t + 1, 9])
    
    eta_L_t <- params$omega * eta_H_t
    eta_L_t1 <- params$omega * eta_H_t1
    Y_U_N_L <- 0.5 * (eta_L_t * out[t, 34] + eta_L_t1 * out[t + 1, 34])
    Y_D_N_L <- 0.5 * params$rho * (out[t, 41] + out[t + 1, 41])
    
    presctiption[t] = alpha * p_DbE + p_DoS_H * Y_U_N_H + p_DoS_L * Y_U_N_L + p_DoD_H * Y_D_N_H + p_DoD_L * Y_D_N_L
  }
  
  cases[i,] <- incidence
  prescriptions[i,] <- presctiption
}

# load baseline cases
cases_baseline <- readRDS(file="cases_baseline_fixed_main.Rda")

# compute total number of averted cases
averted_cases = cases_baseline - cases
total_averted_cases = rowSums(averted_cases)
smr_averted = as.data.frame(t(quantile(total_averted_cases, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
colnames(smr_averted) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")

# compute total number of averted cases in percentage
total_averted_cases_percentage = total_averted_cases / rowSums(cases_baseline) * 100
smr_averted_percentage = as.data.frame(t(quantile(total_averted_cases_percentage, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
colnames(smr_averted_percentage) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")

# compute total number of prescriptions
total_prescriptions = rowSums(prescriptions)
smr_prescriptions = as.data.frame(t(quantile(total_prescriptions, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
colnames(smr_prescriptions) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")

# compute number of averted cases per prescription
acerted_per_prescription = total_averted_cases / total_prescriptions
acerted_per_prescription = as.data.frame(t(quantile(acerted_per_prescription, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
colnames(acerted_per_prescription) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")

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

# load baseline statistics
df_baseline <- readRDS(file="data_baseline_fixed_main.Rda")

df$scenario <- "Intervention"
df_baseline$scenario <- "Baseline"

df_combined <- rbind(df, df_baseline)

ggplot(df_combined, aes(
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
  labs(x = "Year", y = "Annual Number of Diagnosed Cases") +
  scale_color_manual(
    name = NULL,
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
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
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    plot.margin = margin(5, 0, 0, 0)
  )