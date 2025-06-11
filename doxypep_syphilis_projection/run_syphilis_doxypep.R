library(deSolve)
library(ggplot2)
library(patchwork)

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
    dU_N_H = q_H * alpha * (1 - p_DbE) - (lambda_H + p_DoS_H * eta_H + 1/gamma) * U_N_H + (1 - p_DoD_H) * rho * R_N_H + xi_N * U_D_H + xi_XN * U_X_H
    dI_N_H = lambda_H * U_N_H - (sigma + 1/gamma) * I_N_H + xi_N * I_D_H  + xi_XN * I_X_H
    dP_N_H = sigma * I_N_H - (mu + psi_S + 1/gamma) * P_N_H + xi_N * P_D_H + xi_XN * P_X_H
    dS_N_H = psi_S * P_N_H - (mu + psi_E + 1/gamma) * S_N_H + xi_N * S_D_H + xi_XN * S_X_H
    dE_N_H = psi_E * S_N_H - (eta_H + psi_L + 1/gamma) * E_N_H + xi_N * E_D_H + xi_XN * E_X_H
    dL_N_H = psi_L * E_N_H - (eta_H + psi_T + 1/gamma) * L_N_H + xi_N * L_D_H + xi_XN * L_X_H
    dT_N_H = psi_T * L_N_H - (mu + nu + 1/gamma) * T_N_H + xi_N * T_D_H + xi_XN * T_X_H
    dR_N_H = mu * (P_N_H + S_N_H + T_N_H) + eta_H * (E_N_H + L_N_H) - (rho + 1/gamma) * R_N_H + xi_N * R_D_H + xi_XN * R_X_H
    
    # doxy-inconsistent (X)
    dU_X_H = xi_X * U_D_H - ((1 - zeta * e_d) * lambda_H + 1/gamma + xi_XN + p_DoS_H * eta_H) * U_X_H + (1 - p_DoD_H) * rho * R_X_H
    dI_X_H = xi_X * I_D_H + (1 - zeta * e_d) * lambda_H * U_X_H - (sigma + 1/gamma + xi_XN) * I_X_H
    dP_X_H = xi_X * P_D_H + sigma * I_X_H - (mu + psi_S + 1/gamma + xi_XN) * P_X_H
    dS_X_H = xi_X * S_D_H + psi_S * P_X_H - (mu + psi_E + 1/gamma + xi_XN) * S_X_H
    dE_X_H = xi_X * E_D_H + psi_E * S_X_H - (eta_H + psi_L + 1/gamma + xi_XN) * E_X_H
    dL_X_H = xi_X * L_D_H + psi_L * E_X_H - (eta_H + psi_T + 1/gamma + xi_XN) * L_X_H
    dT_X_H = xi_X * T_D_H + psi_T * L_X_H - (mu + nu + 1/gamma + xi_XN) * T_X_H
    dR_X_H = xi_X * R_D_H + mu * (P_X_H + S_X_H + T_X_H) + eta_H * (E_X_H + L_X_H) - (rho + 1/gamma + xi_XN) * R_X_H
    
    # doxy-pep (D)
    dU_D_H = q_H * alpha * p_DbE + (p_DoS_H * eta_H) * (U_N_H + U_X_H) + p_DoD_H * rho * (R_N_H + R_X_H) + rho * R_D_H - ((1 - e_d) * lambda_H + 1/gamma + xi_X + xi_N + xi_M) * U_D_H
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
    dU_N_L = q_L * alpha * (1 - p_DbE) - (lambda_L + p_DoS_L * eta_L + 1/gamma) * U_N_L + (1 - p_DoD_L) * rho * R_N_L + xi_N * U_D_L + xi_XN * U_X_L
    dI_N_L = lambda_L * U_N_L - (sigma + 1/gamma) * I_N_L + xi_N * I_D_L + xi_XN * I_X_L
    dP_N_L = sigma * I_N_L - (mu + psi_S + 1/gamma) * P_N_L + xi_N * P_D_L + xi_XN * P_X_L
    dS_N_L = psi_S * P_N_L - (mu + psi_E + 1/gamma) * S_N_L + xi_N * S_D_L + xi_XN * S_X_L
    dE_N_L = psi_E * S_N_L - (eta_L + psi_L + 1/gamma) * E_N_L + xi_N * E_D_L + xi_XN * E_X_L
    dL_N_L = psi_L * E_N_L - (eta_L + psi_T + 1/gamma) * L_N_L + xi_N * L_D_L + xi_XN * L_X_L
    dT_N_L = psi_T * L_N_L - (mu + nu + 1/gamma) * T_N_L + xi_N * T_D_L + xi_XN * T_X_L
    dR_N_L = mu * (P_N_L + S_N_L + T_N_L) + eta_L * (E_N_L + L_N_L) - (rho + 1/gamma) * R_N_L + xi_N * R_D_L + xi_XN * R_X_L
    
    # doxy-inconsistent (X)
    dU_X_L = xi_X * U_D_L - ((1 - zeta * e_d) * lambda_L + 1/gamma + xi_XN + p_DoS_L * eta_L) * U_X_L + (1 - p_DoD_L) * rho * R_X_L
    dI_X_L = xi_X * I_D_L + (1 - zeta * e_d) * lambda_L * U_X_L - (sigma + 1/gamma + xi_XN) * I_X_L
    dP_X_L = xi_X * P_D_L + sigma * I_X_L - (mu + psi_S + 1/gamma + xi_XN) * P_X_L
    dS_X_L = xi_X * S_D_L + psi_S * P_X_L - (mu + psi_E + 1/gamma + xi_XN) * S_X_L
    dE_X_L = xi_X * E_D_L + psi_E * S_X_L - (eta_L + psi_L + 1/gamma + xi_XN) * E_X_L
    dL_X_L = xi_X * L_D_L + psi_L * E_X_L - (eta_L + psi_T + 1/gamma + xi_XN) * L_X_L
    dT_X_L = xi_X * T_D_L + psi_T * L_X_L - (mu + nu + 1/gamma + xi_XN) * T_X_L
    dR_X_L = xi_X * R_D_L + mu * (P_X_L + S_X_L + T_X_L) + eta_L * (E_X_L + L_X_L) - (rho + 1/gamma + xi_XN) * R_X_L
    
    # doxy-pep (D)
    dU_D_L = q_L * alpha * p_DbE + (p_DoS_L * eta_L) * (U_N_L + U_X_L) + p_DoD_L * rho * (R_N_L + R_X_L) + rho * R_D_L - ((1 - e_d) * lambda_L + 1/gamma + xi_X + xi_N + xi_M) * U_D_L
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

run_doxypep <- function(p_DbE, p_DoD_H, p_DoD_L, p_DoS_H, p_DoS_L) {
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
  xi_M <- -log(1-0.02)/1.5
  
  # Discontinuation rate of doxy-PEP
  xi_N <- -log(1-0.304)/1
  
  # Suboptimal adherence rate of doxy-PEP
  xi_X <- -log(1-0.343)/1
  
  # Discontinuation rate of doxy-PEP for strata X
  xi_XN <- -log(1-0.608)/1
  
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
  cases_low <- matrix(NA, nrow = n_iter, ncol = n_years)
  cases_high <- matrix(NA, nrow = n_iter, ncol = n_years)
  cases_primary <- matrix(NA, nrow = n_iter, ncol = n_years)
  cases_secondary <- matrix(NA, nrow = n_iter, ncol = n_years)
  cases_others <- matrix(NA, nrow = n_iter, ncol = n_years)
  prescriptions <- matrix(NA, nrow = n_iter, ncol = n_years)
  nondoxy_U <- matrix(NA, nrow = n_iter, ncol = n_years)
  inconsistentdoxy_U <- matrix(NA, nrow = n_iter, ncol = n_years)
  doxy_U <- matrix(NA, nrow = n_iter, ncol = n_years)
  intolerantdoxy_U <- matrix(NA, nrow = n_iter, ncol = n_years)
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
           U_M_L=U_M_L, I_M_L=I_M_L, P_M_L=P_M_L, S_M_L=S_M_L, E_M_L=E_M_L, L_M_L=L_M_L, T_M_L=T_M_L, R_M_L=R_M_L)
    
    params <- list(
      q_H = q_H, c_H = c_H, c_L = c_L, q_L = q_L,
      t_0 = t_0, alpha = alpha, gamma = gamma,
      sigma = posterior_df$sigma[idx], psi_S = posterior_df$psi_S[idx], psi_E = posterior_df$psi_E[idx], psi_L = posterior_df$psi_L[idx], 
      psi_T = psi_T, nu = nu, beta = posterior_df$beta[idx], phi_beta = posterior_df$phi_beta[idx], epsilon=posterior_df$epsilon[idx], rho=posterior_df$rho[idx], 
      eta_H_init=posterior_df$eta_H_init[idx], phi_eta=posterior_df$phi_eta[idx], omega=posterior_df$omega[idx], mu=posterior_df$mu[idx],
      e_d = e_d, zeta = zeta, xi_M = xi_M, xi_N = xi_N, xi_X = xi_X, p_DbE = p_DbE, p_DoD_H = p_DoD_H, p_DoD_L = p_DoD_L,p_DoS_H = p_DoS_H, p_DoS_L = p_DoS_L, xi_XN = xi_XN
    )
    
    # solve the system
    out <- ode(y = y0, times = t, func = doxypep_model, parms = params)
    out <- as.data.frame(out)
    
    # compute incidences and prescriptions
    incidence <- numeric(n_years)
    incidence_low <- numeric(n_years)
    incidence_high <- numeric(n_years)
    incidence_primary <- numeric(n_years)
    incidence_secondary <- numeric(n_years)
    incidence_others <- numeric(n_years)
    presctiption <- numeric(n_years)
    nondoxy_U_H <- numeric(n_years)
    inconsistentdoxy_U_H <- numeric(n_years)
    doxy_U_H <- numeric(n_years)
    intolerantdoxy_U_H <- numeric(n_years)
    
    for (t in 1:(n_years)) {
      isFixed = TRUE
      
      nondoxy_U_H[t] = out[t, 2]
      inconsistentdoxy_U_H[t] = out[t,10]
      doxy_U_H[t] = out[t, 18]
      intolerantdoxy_U_H[t] = out[t, 26]
      
      # Trapezoidal rule: (f(a) + f(b)) / 2 * (b - a)
      incidence[t] = 0.5 * params$rho * (out[t, 9] + out[t + 1, 9] + out[t, 17] + out[t + 1, 17] + out[t, 25] + out[t + 1, 25] + out[t, 33] + out[t + 1, 33] + 
                                           out[t, 41] + out[t + 1, 41] + out[t, 49] + out[t + 1, 49] + out[t, 57] + out[t + 1, 57] + out[t, 65] + out[t + 1, 65])
      
      incidence_low[t] = 0.5 * params$rho * (out[t, 41] + out[t + 1, 41] + out[t, 49] + out[t + 1, 49] + out[t, 57] + out[t + 1, 57] + out[t, 65] + out[t + 1, 65])
      
      incidence_high[t] = 0.5 * params$rho * (out[t, 9] + out[t + 1, 9] + out[t, 17] + out[t + 1, 17] + out[t, 25] + out[t + 1, 25] + out[t, 33] + out[t + 1, 33])
      
      incidence_primary[t] = 0.5 * params$mu * (out[t, 4] + out[t + 1, 4] + out[t, 12] + out[t + 1, 12] + out[t, 20] + out[t + 1, 20] + out[t, 28] + out[t + 1, 28] + 
                                                  out[t, 36] + out[t + 1, 36] + out[t, 44] + out[t + 1, 44] + out[t, 52] + out[t + 1, 52] + out[t, 60] + out[t + 1, 60])
      
      incidence_secondary[t] = 0.5 * params$mu * (out[t, 5] + out[t + 1, 5] + out[t, 13] + out[t + 1, 13] + out[t, 21] + out[t + 1, 21] + out[t, 29] + out[t + 1, 29] + 
                                                  out[t, 37] + out[t + 1, 37] + out[t, 45] + out[t + 1, 45] + out[t, 53] + out[t + 1, 53] + out[t, 61] + out[t + 1, 61])
      
      eta_H_t <- get_eta(t+20, t_0, params$eta_H_init, params$phi_eta, isFixed)
      eta_H_t1 <- get_eta(t+20+1, t_0, params$eta_H_init, params$phi_eta, isFixed)
      Y_U_N_H <- 0.5 * (eta_H_t * out[t, 2] + eta_H_t1 * out[t + 1, 2])
      Y_U_X_H <- 0.5 * (eta_H_t * out[t, 10] + eta_H_t1 * out[t + 1, 10])
      Y_D_N_H <- 0.5 * params$rho * (out[t, 9] + out[t + 1, 9])
      Y_D_X_H <- 0.5 * params$rho * (out[t, 17] + out[t + 1, 17])
      
      eta_L_t <- params$omega * eta_H_t
      eta_L_t1 <- params$omega * eta_H_t1
      Y_U_N_L <- 0.5 * (eta_L_t * out[t, 34] + eta_L_t1 * out[t + 1, 34])
      Y_U_X_L <- 0.5 * (eta_L_t * out[t, 42] + eta_L_t1 * out[t + 1, 42])
      Y_D_N_L <- 0.5 * params$rho * (out[t, 41] + out[t + 1, 41])
      Y_D_X_L <- 0.5 * params$rho * (out[t, 49] + out[t + 1, 49])
      
      presctiption[t] = alpha * p_DbE + p_DoS_H * (Y_U_N_H + Y_U_X_H) + p_DoS_L * (Y_U_N_L + Y_U_X_L) + p_DoD_H * (Y_D_N_H + Y_D_X_H) + p_DoD_L * (Y_D_N_L + Y_D_X_L)
      
      incidence_others[t] = 0.5 * (eta_H_t * (out[t, 6] + out[t, 14] + out[t, 22] + out[t, 30] + out[t, 7] + out[t, 15] + out[t, 23] + out[t, 31]) 
                                   + eta_H_t1 * (out[t + 1, 6] + out[t + 1, 14] + out[t + 1, 22] + out[t + 1, 30] + out[t + 1, 7] + out[t + 1, 15] + out[t + 1, 23] + out[t + 1, 31])
                                   + eta_L_t * (out[t, 38] + out[t, 46] + out[t, 54] + out[t, 62] + out[t, 39] + out[t, 47] + out[t, 55] + out[t, 63]) 
                                   + eta_L_t1 * (out[t + 1, 38] + out[t + 1, 46] + out[t + 1, 54] + out[t + 1, 62] + out[t + 1, 39] + out[t + 1, 47] + out[t + 1, 55] + out[t + 1, 63]) 
                                   + params$mu * (out[t, 8] + out[t + 1, 8] + out[t, 16] + out[t + 1, 16] + out[t, 24] + out[t + 1, 24] + out[t, 32] + out[t + 1, 32] + 
                                                    out[t, 40] + out[t + 1, 40] + out[t, 48] + out[t + 1, 48] + out[t, 56] + out[t + 1, 56] + out[t, 64] + out[t + 1, 64]))
    }
    
    cases[i,] <- incidence
    cases_low[i,] <- incidence_low
    cases_high[i,] <- incidence_high
    cases_primary[i,] <- incidence_primary
    cases_secondary[i,] <- incidence_secondary
    cases_others[i,] <- incidence_others
    prescriptions[i,] <- presctiption
    nondoxy_U[i,] = nondoxy_U_H
    inconsistentdoxy_U[i,] = inconsistentdoxy_U_H
    doxy_U[i,] = doxy_U_H
    intolerantdoxy_U[i,] = intolerantdoxy_U_H
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
  averted_per_prescription = total_averted_cases / total_prescriptions
  averted_per_prescription = as.data.frame(t(quantile(averted_per_prescription, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
  colnames(averted_per_prescription) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  
  # compute total number of averted primary cases
  cases_baseline_primary <- readRDS(file="cases_baseline_primary_fixed_main.Rda")
  averted_cases_primary = cases_baseline_primary - cases_primary
  total_averted_cases_primary = rowSums(averted_cases_primary)
  smr_averted_primary = as.data.frame(t(quantile(total_averted_cases_primary, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
  colnames(smr_averted_primary) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  
  # compute total number of averted secondary cases
  cases_baseline_secondary <- readRDS(file="cases_baseline_secondary_fixed_main.Rda")
  averted_cases_secondary = cases_baseline_secondary - cases_secondary
  total_averted_cases_secondary = rowSums(averted_cases_secondary)
  smr_averted_secondary = as.data.frame(t(quantile(total_averted_cases_secondary, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
  colnames(smr_averted_secondary) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  
  # compute total number of averted other cases
  cases_baseline_others <- readRDS(file="cases_baseline_others_fixed_main.Rda")
  averted_cases_others = cases_baseline_others - cases_others
  total_averted_cases_others = rowSums(averted_cases_others)
  smr_averted_others = as.data.frame(t(quantile(total_averted_cases_others, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)))
  colnames(smr_averted_others) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  
  ################# plot intervention vs. baseline for annual cases
  # compute quantiles for each row for cases
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  smr_pred <- t(apply(cases, 2, quantile, probs = probs, na.rm = TRUE))
  colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  smr_pred <- as.data.frame(smr_pred)
  
  # box plot, output size (8, 6)
  df <- data.frame(
    group = factor(2026:2040),
    ymin = pmax(smr_pred$X2.5.[1:15], 0),  # minimum
    lower = pmax(smr_pred$X25.[1:15], 0),  # Q1
    middle = pmax(smr_pred$X50.[1:15], 0), # median
    upper = pmax(smr_pred$X75.[1:15], 0),  # Q3
    ymax = pmax(smr_pred$X97.5.[1:15], 0), # maximum
    year = 2026:2040              # year
  )
  
  # load baseline statistics
  df_baseline <- readRDS(file="data_baseline_fixed_main.Rda")
  
  df$scenario <- "Intervention"
  df_baseline$scenario <- "Baseline"
  
  df_combined_cases <- rbind(df, df_baseline)
  
  ################ plot intervention vs. baseline for annual cases at low-risk group
  # compute quantiles for each row for cases
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  smr_pred <- t(apply(cases_low, 2, quantile, probs = probs, na.rm = TRUE))
  colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  smr_pred <- as.data.frame(smr_pred)
  
  # box plot, output size (8, 6)
  df <- data.frame(
    group = factor(2026:2040),
    ymin = pmax(smr_pred$X2.5.[1:15], 0),  # minimum
    lower = pmax(smr_pred$X25.[1:15], 0),  # Q1
    middle = pmax(smr_pred$X50.[1:15], 0), # median
    upper = pmax(smr_pred$X75.[1:15], 0),  # Q3
    ymax = pmax(smr_pred$X97.5.[1:15], 0), # maximum
    year = 2026:2040              # year
  )
  
  # load baseline statistics
  df_baseline <- readRDS(file="data_baseline_lowrisk_fixed_main.Rda")
  
  df$scenario <- "Intervention"
  df_baseline$scenario <- "Baseline"
  
  df_combined_cases_low <- rbind(df, df_baseline)
  
  ################# plot intervention vs. baseline for annual cases at high-risk group
  # compute quantiles for each row for cases
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  smr_pred <- t(apply(cases_high, 2, quantile, probs = probs, na.rm = TRUE))
  colnames(smr_pred) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  smr_pred <- as.data.frame(smr_pred)
  
  # box plot, output size (8, 6)
  df <- data.frame(
    group = factor(2026:2040),
    ymin = pmax(smr_pred$X2.5.[1:15], 0),  # minimum
    lower = pmax(smr_pred$X25.[1:15], 0),  # Q1
    middle = pmax(smr_pred$X50.[1:15], 0), # median
    upper = pmax(smr_pred$X75.[1:15], 0),  # Q3
    ymax = pmax(smr_pred$X97.5.[1:15], 0), # maximum
    year = 2026:2040              # year
  )
  
  # load baseline statistics
  df_baseline <- readRDS(file="data_baseline_highrisk_fixed_main.Rda")
  
  df$scenario <- "Intervention"
  df_baseline$scenario <- "Baseline"
  
  df_combined_cases_high <- rbind(df, df_baseline)
  
  # plot annual susceptible MSM at high-risk in each strata
  # compute quantiles for each row for nondoxy U
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  smr_pred_nondoxyU <- t(apply(nondoxy_U, 2, quantile, probs = probs, na.rm = TRUE))
  colnames(smr_pred_nondoxyU) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  smr_pred_nondoxyU <- as.data.frame(smr_pred_nondoxyU)
  
  df_nondoxyU <- data.frame(
    group = factor(2026:2040),
    ymin = pmax(smr_pred_nondoxyU$X2.5.[1:15], 0),  # minimum
    lower = pmax(smr_pred_nondoxyU$X25.[1:15], 0),  # Q1
    middle = pmax(smr_pred_nondoxyU$X50.[1:15], 0), # median
    upper = pmax(smr_pred_nondoxyU$X75.[1:15], 0),  # Q3
    ymax = pmax(smr_pred_nondoxyU$X97.5.[1:15], 0), # maximum
    year = 2026:2040              # year
  )
  
  # compute quantiles for each row for inconsistentdoxy U
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  smr_pred_inconsistentdoxyU <- t(apply(inconsistentdoxy_U, 2, quantile, probs = probs, na.rm = TRUE))
  colnames(smr_pred_inconsistentdoxyU) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  smr_pred_inconsistentdoxyU <- as.data.frame(smr_pred_inconsistentdoxyU)
  
  df_inconsistentdoxyU <- data.frame(
    group = factor(2026:2040),
    ymin = pmax(smr_pred_inconsistentdoxyU$X2.5.[1:15], 0),  # minimum
    lower = pmax(smr_pred_inconsistentdoxyU$X25.[1:15], 0),  # Q1
    middle = pmax(smr_pred_inconsistentdoxyU$X50.[1:15], 0), # median
    upper = pmax(smr_pred_inconsistentdoxyU$X75.[1:15], 0),  # Q3
    ymax = pmax(smr_pred_inconsistentdoxyU$X97.5.[1:15], 0), # maximum
    year = 2026:2040              # year
  )
  
  # compute quantiles for each row for doxy U
  probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  smr_pred_doxyU <- t(apply(doxy_U, 2, quantile, probs = probs, na.rm = TRUE))
  colnames(smr_pred_doxyU) <- c("X2.5.", "X25.", "X50.", "X75.", "X97.5.")
  smr_pred_doxyU <- as.data.frame(smr_pred_doxyU)
  
  df_doxyU <- data.frame(
    group = factor(2026:2040),
    ymin = pmax(smr_pred_doxyU$X2.5.[1:15], 0),  # minimum
    lower = pmax(smr_pred_doxyU$X25.[1:15], 0),  # Q1
    middle = pmax(smr_pred_doxyU$X50.[1:15], 0), # median
    upper = pmax(smr_pred_doxyU$X75.[1:15], 0),  # Q3
    ymax = pmax(smr_pred_doxyU$X97.5.[1:15], 0), # maximum
    year = 2026:2040              # year
  )
  
  df_nondoxyU$scenario <- "Non-Doxy-PEP"
  df_inconsistentdoxyU$scenario <- "Doxy-Inconsistent"
  df_doxyU$scenario <- "Doxy-PEP"
  
  df_combined_susceptible <- rbind(df_nondoxyU, df_inconsistentdoxyU, df_doxyU)
  df_combined_susceptible$scenario <- factor(
    df_combined_susceptible$scenario,
    levels = c("Doxy-PEP", "Non-Doxy-PEP", "Doxy-Inconsistent")
  )
  
  return(list(df_combined_cases = df_combined_cases, df_combined_susceptible = df_combined_susceptible, df_combined_cases_low = df_combined_cases_low, df_combined_cases_high = df_combined_cases_high,
              smr_averted = smr_averted, smr_prescriptions = smr_prescriptions, averted_per_prescription = averted_per_prescription, smr_averted_primary = smr_averted_primary,
              smr_averted_secondary = smr_averted_secondary, smr_averted_others = smr_averted_others))
}

############################################################## get results ##############################################################
uptake = 0.10
# DbE
result_DbE <- run_doxypep(p_DbE = uptake, p_DoD_H = 0, p_DoD_L = 0, p_DoS_H = 0, p_DoS_L = 0)
df_combined_DbE_cases <- result_DbE$df_combined_cases
df_combined_DbE_susceptible <- result_DbE$df_combined_susceptible
df_combined_DbE_cases_low <- result_DbE$df_combined_cases_low
df_combined_DbE_cases_high <- result_DbE$df_combined_cases_high

# DoD(H)
result_DoD_H <- run_doxypep(p_DbE = 0, p_DoD_H = uptake, p_DoD_L = 0, p_DoS_H = 0, p_DoS_L = 0)
df_combined_DoD_H_cases <- result_DoD_H$df_combined_cases
df_combined_DoD_H_susceptible <- result_DoD_H$df_combined_susceptible
df_combined_DoD_H_cases_low <- result_DoD_H$df_combined_cases_low
df_combined_DoD_H_cases_high <- result_DoD_H$df_combined_cases_high

# DoD
result_DoD <- run_doxypep(p_DbE = 0, p_DoD_H = uptake, p_DoD_L = uptake, p_DoS_H = 0, p_DoS_L = 0)
df_combined_DoD_cases <- result_DoD$df_combined_cases
df_combined_DoD_susceptible <- result_DoD$df_combined_susceptible
df_combined_DoD_cases_low <- result_DoD$df_combined_cases_low
df_combined_DoD_cases_high <- result_DoD$df_combined_cases_high

# DoA(H)
result_DoA_H <- run_doxypep(p_DbE = 0, p_DoD_H = uptake, p_DoD_L = 0, p_DoS_H = uptake, p_DoS_L = 0)
df_combined_DoA_H_cases <- result_DoA_H$df_combined_cases
df_combined_DoA_H_susceptible <- result_DoA_H$df_combined_susceptible
df_combined_DoA_H_cases_low <- result_DoA_H$df_combined_cases_low
df_combined_DoA_H_cases_high <- result_DoA_H$df_combined_cases_high

# DoA
result_DoA <- run_doxypep(p_DbE = 0, p_DoD_H = uptake, p_DoD_L = uptake, p_DoS_H = uptake, p_DoS_L = uptake)
df_combined_DoA_cases <- result_DoA$df_combined_cases
df_combined_DoA_susceptible <- result_DoA$df_combined_susceptible
df_combined_DoA_cases_low <- result_DoA$df_combined_cases_low
df_combined_DoA_cases_high <- result_DoA$df_combined_cases_high

# DaR
result_DaR <- run_doxypep(p_DbE = 0, p_DoD_H = uptake, p_DoD_L = uptake, p_DoS_H = uptake, p_DoS_L = 0)
df_combined_DaR_cases <- result_DaR$df_combined_cases
df_combined_DaR_susceptible <- result_DaR$df_combined_susceptible
df_combined_DaR_cases_low <- result_DaR$df_combined_cases_low
df_combined_DaR_cases_high <- result_DaR$df_combined_cases_high

############################################################## plot cases ##############################################################
df_combined_DbE_cases$group <- factor(paste0("'", 26:40))
df_combined_DoD_H_cases$group <- factor(paste0("'", 26:40))
df_combined_DoD_cases$group <- factor(paste0("'", 26:40))
df_combined_DoA_H_cases$group <- factor(paste0("'", 26:40))
df_combined_DoA_cases$group <- factor(paste0("'", 26:40))
df_combined_DaR_cases$group <- factor(paste0("'", 26:40))
size = 35

p1 <- ggplot(df_combined_DbE_cases, aes(
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
  labs(title = "A: DbE") +
  scale_color_manual(
    name = NULL,
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
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

p2 <- ggplot(df_combined_DoD_H_cases, aes(
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
  labs(title = "B: DoD(H)") +
  scale_color_manual(
    name = NULL,
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p2 <- p2 +
#   labs(tag = "(B)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p3 <- ggplot(df_combined_DoD_cases, aes(
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
  labs(title = "C: DoD") +
  scale_color_manual(
    name = NULL,
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p3 <- p3 +
#   labs(tag = "(C)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p4 <- ggplot(df_combined_DoA_H_cases, aes(
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
  labs(title = "D: DoA(H)") +
  scale_color_manual(
    name = NULL,
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p4 <- p4 +
#   labs(tag = "(D)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p5 <- ggplot(df_combined_DoA_cases, aes(
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
  labs(title = "E: DoA") +
  scale_color_manual(
    name = NULL,
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p5 <- p5 +
#   labs(tag = "(E)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p6 <- ggplot(df_combined_DaR_cases, aes(
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
  labs(title = "F: DaR") +
  scale_color_manual(
    name = NULL,
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p6 <- p6 +
#   labs(tag = "(F)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

row_cases <- (p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 6, guides = "collect")) &
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

############################################################## plot high-risk cases ##############################################################
df_combined_DbE_cases_high$group <- factor(paste0("'", 26:40))
df_combined_DoD_H_cases_high$group <- factor(paste0("'", 26:40))
df_combined_DoD_cases_high$group <- factor(paste0("'", 26:40))
df_combined_DoA_H_cases_high$group <- factor(paste0("'", 26:40))
df_combined_DoA_cases_high$group <- factor(paste0("'", 26:40))
df_combined_DaR_cases_high$group <- factor(paste0("'", 26:40))
size = 35

p7 <- ggplot(df_combined_DbE_cases_high, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p7 <- p7 + 
#   labs(tag = "(B)") + 
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p8 <- ggplot(df_combined_DoD_H_cases_high, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p9 <- ggplot(df_combined_DoD_cases_high, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p10 <- ggplot(df_combined_DoA_H_cases_high, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p11 <- ggplot(df_combined_DoA_cases_high, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p12 <- ggplot(df_combined_DaR_cases_high, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

row_cases_high <- (p7 + p8 + p9 + p10 + p11 + p12 + plot_layout(ncol = 6, guides = "collect")) &
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
    y = "Annual Number of \n Diagnosed Cases \n (High-Risk)"
  )

############################################################## plot low-risk cases ##############################################################
df_combined_DbE_cases_low$group <- factor(paste0("'", 26:40))
df_combined_DoD_H_cases_low$group <- factor(paste0("'", 26:40))
df_combined_DoD_cases_low$group <- factor(paste0("'", 26:40))
df_combined_DoA_H_cases_low$group <- factor(paste0("'", 26:40))
df_combined_DoA_cases_low$group <- factor(paste0("'", 26:40))
df_combined_DaR_cases_low$group <- factor(paste0("'", 26:40))
size = 35

p13 <- ggplot(df_combined_DbE_cases_low, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p13 <- p13 + 
#   labs(tag = "(C)") + 
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p14 <- ggplot(df_combined_DoD_H_cases_low, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p15 <- ggplot(df_combined_DoD_cases_low, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p16 <- ggplot(df_combined_DoA_H_cases_low, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p17 <- ggplot(df_combined_DoA_cases_low, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

p18 <- ggplot(df_combined_DaR_cases_low, aes(
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
    values = c("Baseline" = "darkred", "Intervention" = "steelblue")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Baseline" = "salmon", "Intervention" = "lightblue")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )

row_cases_low <- (p13 + p14 + p15 + p16 + p17 + p18 + plot_layout(ncol = 6, guides = "collect")) &
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
    y = "Annual Number of \n Diagnosed Cases \n (Low-Risk)"
  )

############################################################## plot susceptible ##############################################################
df_combined_DbE_susceptible$group <- factor(paste0("'", 26:40))
df_combined_DoD_H_susceptible$group <- factor(paste0("'", 26:40))
df_combined_DoD_susceptible$group <- factor(paste0("'", 26:40))
df_combined_DoA_H_susceptible$group <- factor(paste0("'", 26:40))
df_combined_DoA_susceptible$group <- factor(paste0("'", 26:40))
df_combined_DaR_susceptible$group <- factor(paste0("'", 26:40))
size = 35

p19 <- ggplot(df_combined_DbE_susceptible, aes(
  x = group,
  ymin = lower,
  lower = lower,
  middle = middle,
  upper = upper,
  ymax = upper,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#073E7F", "Doxy-Inconsistent" = "#F49600", "Doxy-PEP" = "#BE0E23")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#83A0BE", "Doxy-Inconsistent" = "#F9CB80", "Doxy-PEP" = "#E18791")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title.y = element_text(size = size),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
p19 <- p19 +
  labs(tag = "(2)") +
  theme(
    plot.tag.position = c(0, 0.95),
    plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
  )

p20 <- ggplot(df_combined_DoD_H_susceptible, aes(
  x = group,
  ymin = lower,
  lower = lower,
  middle = middle,
  upper = upper,
  ymax = upper,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#073E7F", "Doxy-Inconsistent" = "#F49600", "Doxy-PEP" = "#BE0E23")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#83A0BE", "Doxy-Inconsistent" = "#F9CB80", "Doxy-PEP" = "#E18791")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p20 <- p20 +
#   labs(tag = "(H)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p21 <- ggplot(df_combined_DoD_susceptible, aes(
  x = group,
  ymin = lower,
  lower = lower,
  middle = middle,
  upper = upper,
  ymax = upper,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#073E7F", "Doxy-Inconsistent" = "#F49600", "Doxy-PEP" = "#BE0E23")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#83A0BE", "Doxy-Inconsistent" = "#F9CB80", "Doxy-PEP" = "#E18791")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p21 <- p21 +
#   labs(tag = "(I)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p22 <- ggplot(df_combined_DoA_H_susceptible, aes(
  x = group,
  ymin = lower,
  lower = lower,
  middle = middle,
  upper = upper,
  ymax = upper,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#073E7F", "Doxy-Inconsistent" = "#F49600", "Doxy-PEP" = "#BE0E23")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#83A0BE", "Doxy-Inconsistent" = "#F9CB80", "Doxy-PEP" = "#E18791")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.title.x = element_text(size = size, hjust = 0),
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p22 <- p22 +
#   labs(tag = "(J)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p23 <- ggplot(df_combined_DoA_susceptible, aes(
  x = group,
  ymin = lower,
  lower = lower,
  middle = middle,
  upper = upper,
  ymax = upper,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#073E7F", "Doxy-Inconsistent" = "#F49600", "Doxy-PEP" = "#BE0E23")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#83A0BE", "Doxy-Inconsistent" = "#F9CB80", "Doxy-PEP" = "#E18791")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
  )
# p23 <- p23 +
#   labs(tag = "(K)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

p24 <- ggplot(df_combined_DaR_susceptible, aes(
  x = group,
  ymin = lower,
  lower = lower,
  middle = middle,
  upper = upper,
  ymax = upper,
  color = scenario,
  fill = scenario
)) +
  geom_boxplot(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_color_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#073E7F", "Doxy-Inconsistent" = "#F49600", "Doxy-PEP" = "#BE0E23")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Non-Doxy-PEP" = "#83A0BE", "Doxy-Inconsistent" = "#F9CB80", "Doxy-PEP" = "#E18791")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  scale_x_discrete(breaks = c("'26", "'33", "'40")) + 
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
    axis.text.x = element_text(size = size),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# p24 <- p24 +
#   labs(tag = "(L)") +
#   theme(
#     plot.tag.position = c(0, 0.95),
#     plot.tag = element_text(size = size+10, hjust = 0, vjust = 0)
#   )

row_susceptible <- (p19 + p20 + p21 + p22 + p23 + p24 + plot_layout(ncol = 6, guides = "collect")) &
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
    y = "Annual Number of \n Susceptible MSM \n (High-Risk)"
  )

# combine and plot
final_plot <- row_cases / row_susceptible
final_plot

save.image("workspace_0.10_fixed_main.RData")
