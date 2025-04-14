functions {
  real get_C(real I_N, real P_N, real S_N, real E_N, real I_X, real P_X, real S_X, real E_X, real I_D, real P_D, real S_D, real E_D, real I_M, real P_M, real S_M, real E_M) {
    # compute infectious population size of MSM in group j
    return(I_N + P_N + S_N + E_N + I_X + P_X + S_X + E_X + I_D + P_D + S_D + E_D + I_M + P_M + S_M + E_M);
  }
  real get_N(real U_N, real I_N, real P_N, real S_N, real E_N, real L_N, real T_N, real R_N, real U_X, real I_X, real P_X, real S_X, real E_X, real L_X, real T_X, real R_X, real U_D, real I_D, real P_D, real S_D, real E_D, real L_D, real T_D, real R_D, real U_M, real I_M, real P_M, real S_M, real E_M, real L_M, real T_M, real R_M) {
    # compute population size of MSM in group j
    return(U_N + I_N + P_N + S_N + E_N + L_N + T_N + R_N + U_X + I_X + P_X + S_X + E_X + L_X + T_X + R_X + U_D + I_D + P_D + S_D + E_D + L_D + T_D + R_D + U_M + I_M + P_M + S_M + E_M + L_M + T_M + R_M);
  }
  real get_pi(real c_target, real N_target, real c_remain, real N_remain) {
    # compute proportion of all partnerships in the population that involve a member of group j
    return((c_target * N_target) / (c_target * N_target + c_remain * N_remain));
  }
  real get_lambda(real t, real t_0, real c, real beta, real phi_beta, real epsilon, real C_target, real N_target, real pi_target, real C_remain, real N_remain, real pi_remain) {
    # compute force of infection in group j
    return(c * beta * (1 + phi_beta * (t - t_0)) * (epsilon * C_target / N_target + (1 - epsilon) * (pi_target * C_target / N_target + pi_remain * C_remain / N_remain)));
  }
  real get_eta(real t, real t_0, real eta_H_init, real phi_eta) {
    # compute rate of screening in the absence of symptoms in group H
    return(eta_H_init * (1 + phi_eta * (t - t_0)));
  }
  real get_xiN(real t, real t_0, real xi_N_init, real k_n) {
    # compute effective discontinuation rate due to transitioning to alternative prevention methods
    return(xi_N_init * (1 + k_n * (t - t_0)));
  }
  real get_delta(real t, real t_0, real delta_init, real delta_n) {
    # compute effective rate of adherence for transitioning from a doxy-inconsistent regimen to an adherent doxy-PEP regimen
    return(delta_init * (1 + k_delta * (t - t_0)));
  }
  real[] doxypep_syphilis_model(real t, real[] y, real[] theta, real[] x_r, int[] x_i) {
      # initial conditions
      real U_N_H = y[1];
      real R_N_H = y[2]; # R_N_H
      real U_X_H = y[3];
      real I_N_H = y[4];
      real I_X_H = y[5];
      real P_N_H = y[6];
      real P_X_H = y[7];
      real S_N_H = y[8];
      real S_X_H = y[9];
      real E_N_H = y[10];
      real E_X_H = y[11];
      real L_N_H = y[12];
      real L_X_H = y[13];
      real T_N_H = y[14];
      real T_X_H = y[15];
      real R_X_H = y[16]; # R_X_H
      real U_D_H = y[17];
      real I_D_H = y[18];
      real P_D_H = y[19];
      real S_D_H = y[20];
      real E_D_H = y[21];
      real L_D_H = y[22];
      real T_D_H = y[23];
      real R_D_H = y[24]; # R_D_H
      real U_M_H = y[25];
      real R_M_H = y[26]; # R_M_H
      real I_M_H = y[27];
      real P_M_H = y[28];
      real S_M_H = y[29];
      real E_M_H = y[30];
      real L_M_H = y[31];
      real T_M_H = y[32];
      real U_N_L = y[33];
      real R_N_L = y[34]; # R_N_L
      real U_X_L = y[35];
      real I_N_L = y[36];
      real I_X_L = y[37];
      real P_N_L = y[38];
      real P_X_L = y[39];
      real S_N_L = y[40];
      real S_X_L = y[41];
      real E_N_L = y[42];
      real E_X_L = y[43];
      real L_N_L = y[44];
      real L_X_L = y[45];
      real T_N_L = y[46];
      real T_X_L = y[47];
      real R_X_L = y[48]; # R_X_L
      real U_D_L = y[49];
      real I_D_L = y[50];
      real P_D_L = y[51];
      real S_D_L = y[52];
      real E_D_L = y[53];
      real L_D_L = y[54];
      real T_D_L = y[55];
      real R_D_L = y[56]; # R_D_L
      real U_M_L = y[57];
      real R_M_L = y[58]; # R_M_L
      real I_M_L = y[59];
      real P_M_L = y[60];
      real S_M_L = y[61];
      real E_M_L = y[62];
      real L_M_L = y[63];
      real T_M_L = y[64];
      
      # fixed real parameters
      real q_H = x_r[1]; # Proportion of the MSM population in group H
      real p_DbE = x_r[2]; # Probability of uptake of doxycycline before entry into the sexually-active population
      real c_H = x_r[3]; # Annual rate of partner change in group H
      real c_L = x_r[4]; # Annual rate of partner change in group L
      real t_0 = x_r[5]; # Initial time
      real p_DoS_H = x_r[6]; # Probability of uptake of doxycycline on screening with negative results in group H
      real p_DoD = x_r[7]; # Probability of uptake of doxycycline on diagnosis
      real e_d = x_r[8]; # Efficacy of doxycycline against infections
      real q_L = x_r[9]; # Proportion of the MSM population in group L
      real p_DoS_L = x_r[10]; # Probability of uptake of doxycycline on screening with negative results in group L
      
      # fixed integer parameters
      int alpha = x_i[1]; # Annual MSM population entrants (at age 15)
      int N_t0 = x_i[2]; # Initial population size of MSM
      int gamma = x_i[3]; # Years spent in the sexually-active population
      
      # model parameters
      real beta = theta[1]; # Probability of transmission per partnership
      real phi_beta = theta[2]; # Annual increase in transmission risk behaviour
      real epsilon = theta[3]; # Level of assortativity in sexual mixing
      real rho = theta[4]; # Rate of recovery after treatment
      real eta_H_init = theta[5]; # Initial rate of asymptomatic screening in group H
      real phi_eta = theta[6]; # Annual increase in screening rate
      real omega = theta[7]; # Ratio of screening rate in group L vs group H
      real iota = theta[8]; # Effective rate of doxycycline acquisition from non-STI medical clinics
      real k_n = theta[9]; # Transition speed for discontinuation of doxy-PEP
      real xi_N_init = theta[10];  # Initial discontinuation rate due to transitioning to alternative prevention methods
      real sigma = theta[11]; # Rate of leaving incubation period
      real mu = theta[12]; # Rate of seeking treatment due to symptoms 
      real psi_S = theta[13]; # Rate of leaving primary stage
      real psi_E = theta[14]; # Rate of leaving secondary stage
      real psi_L = theta[15]; # Rate of leaving early latent stage
      real psi_T = theta[16]; # Rate of leaving late latent stage
      real nu = theta[17]; # Mortality rate at tertiary stage
      real k_delta = theta[18]; # Transition speed from a doxy-inconsistent regimen to an adherent doxy-PEP regimen
      real delta_init = theta[19]; # Initial rate of adherence for transitioning from a doxy-inconsistent regimen to an adherent doxy-PEP regimen
      real zeta = theta[20]; # Scaling factor accounting for doxycycline inefficacy due to inconsistent and irregular usage
      real xi_m = theta[21]; # Effective doxycycline intolerance rate
      real kappa_D = theta[22]; # Shape parameter of communicable disease surveillance data
      
      # time-dependent variables
      real C_H = get_C(I_N_H, P_N_H, S_N_H, E_N_H, I_X_H, P_X_H, S_X_H, E_X_H, I_D_H, P_D_H, S_D_H, E_D_H, I_M_H, P_M_H, S_M_H, E_M_H); # Number of infectious individuals in group H
      real C_L = get_C(I_N_L, P_N_L, S_N_L, E_N_L, I_X_L, P_X_L, S_X_L, E_X_L, I_D_L, P_D_L, S_D_L, E_D_L, I_M_L, P_M_L, S_M_L, E_M_L); # Number of infectious individuals in group L
      real N_H = get_N(U_N_H, I_N_H, P_N_H, S_N_H, E_N_H, L_N_H, T_N_H, R_N_H, U_X_H, I_X_H, P_X_H, S_X_H, E_X_H, L_X_H, T_X_H, R_X_H, U_D_H, I_D_H, P_D_H, S_D_H, E_D_H, L_D_H, T_D_H, R_D_H, U_M_H, I_M_H, P_M_H, S_M_H, E_M_H, L_M_H, T_M_H, R_M_H); # Total number of MSM population in group H
      real N_L = get_N(U_N_L, I_N_L, P_N_L, S_N_L, E_N_L, L_N_L, T_N_L, R_N_L, U_X_L, I_X_L, P_X_L, S_X_L, E_X_L, L_X_L, T_X_L, R_X_L, U_D_L, I_D_L, P_D_L, S_D_L, E_D_L, L_D_L, T_D_L, R_D_L, U_M_L, I_M_L, P_M_L, S_M_L, E_M_L, L_M_L, T_M_L, R_M_L); # Total number of MSM population in group L
      real pi_H = get_pi(c_H, N_H, c_L, N_L); # Proportion of all partnerships in the population that involve a member of group H
      real pi_L = get_pi(c_L, N_L, c_H, N_H); # Proportion of all partnerships in the population that involve a member of group L
      real lambda_H = get_lambda(t, t_0, c_H, beta, phi_beta, epsilon, C_H, N_H, pi_H, C_L, N_L, pi_L); # Force of infection in group H
      real eta_H = get_eta(t, t_0, eta_H_init, phi_eta); # Rate of screening in the absence of symptoms in group H
      real eta_L = omega * eta_H; # Rate of screening in the absence of symptoms in group L
      real xi_N = get_xiN(t, t_0, xi_N_init, k_n); # Effective discontinuation rate of doxy-PEP
      real delta = get_delta(t, t_0, delta_N_init, k_delta); # Effective rate of adherence for transitioning from a doxy-inconsistent regimen to an adherent doxy-PEP regimen
      real lambda_L = get_lambda(t, t_0, c_L, beta, phi_beta, epsilon, C_L, N_L, pi_L, C_H, N_H, pi_H); # Force of infection in group L
      
      # ODEs
      # high-risk group
      # non-doxy-pep (N)
      real dU_N_H = q_H * alpha * (1 - p_DbE) - (lambda_H + p_DoS_H * eta_H + 1/gamma + iota) * U_N_H + (1 - p_DoD) * rho * R_N_H + xi_N * U_X_H;
      real dI_N_H = lambda_H * U_N_H - (sigma + 1/gamma + iota) * I_N_H + xi_N * I_X_H;
      real dP_N_H = sigma * I_N_H - (mu + psi_S + 1/gamma + iota) * P_N_H + xi_N * P_X_H;
      real dS_N_H = psi_S * P_N_H - (mu + psi_E + 1/gamma + iota) * S_N_H + xi_N * S_X_H;
      real dE_N_H = psi_E * S_N_H - (eta_H + psi_L + 1/gamma + iota) * E_N_H + xi_N * E_X_H;
      real dL_N_H = psi_L * E_N_H - (eta_H + psi_T + 1/gamma + iota) * L_N_H + xi_N * L_X_H;
      real dT_N_H = psi_T * L_N_H - (mu + nu + 1/gamma + iota) * T_N_H + xi_N * T_X_H;
      real dR_N_H = mu * (P_N_H + S_N_H + T_N_H) + eta_H * (E_N_H + L_N_H) - (rho + 1/gamma + iota) * R_N_H + xi_N * R_X_H;
      
      # doxy-inconsistent (X)
      real dU_X_H = (1 - delta) * U_D_H + rho * R_X_H - ((1 - zeta * e_d) * lambda_H + 1/gamma + xi_N + delta) * U_X_H;
      real dI_X_H = (1 - delta) * I_D_H + (1 - zeta * e_d) * lambda_H * U_X_H - (sigma + 1/gamma + xi_N + delta) * I_X_H;
      real dP_X_H = (1 - delta) * P_D_H + sigma * I_X_H - (mu + psi_S + 1/gamma + xi_N + delta) * P_X_H;
      real dS_X_H = (1 - delta) * S_D_H + psi_S * P_X_H - (mu + psi_E + 1/gamma + xi_N + delta) * S_X_H;
      real dE_X_H = (1 - delta) * E_D_H + psi_E * S_X_H - (eta_H + psi_L + 1/gamma + xi_N + delta) * E_X_H;
      real dL_X_H = (1 - delta) * L_D_H + psi_L * E_X_H - (eta_H + psi_T + 1/gamma + xi_N + delta) * L_X_H;
      real dT_X_H = (1 - delta) * T_D_H + psi_T * L_X_H - (mu + nu + 1/gamma + xi_N + delta) * T_X_H;
      real dR_X_H = (1 - delta) * R_D_H + mu * (P_X_H + S_X_H + T_X_H) + eta_H * (E_X_H + L_X_H) - (rho + 1/gamma + xi_N + delta) * R_X_H;
      
      # doxy-pep (D)
      real dU_D_H = q_H * alpha * p_DbE + (p_DoS_H * eta_H + iota) * U_N_H + p_DoD * rho * R_N_H + delta * U_X_H + rho * R_D_H - ((1 - e_d) * lambda_H + 1/gamma + (1 - delta) + xi_m) * U_D_H;
      real dI_D_H = (1 - e_d) * lambda_H * U_D_H + delta * I_X_H + iota * I_N_H - (sigma + 1/gamma + (1 - delta) + xi_m) * I_D_H;
      real dP_D_H = delta * P_X_H + sigma * I_D_H + iota * P_N_H - (mu + psi_S + 1/gamma + xi_m + (1 - delta)) * P_D_H;
      real dS_D_H = delta * S_X_H + psi_S * P_D_H + iota * S_N_H - (mu + psi_E + 1/gamma + xi_m + (1 - delta)) * S_D_H;
      real dE_D_H = delta * E_X_H + psi_E * S_D_H + iota * E_N_H - (eta_H + psi_L + 1/gamma + xi_m + (1 - delta)) * E_D_H;
      real dL_D_H = delta * L_X_H + psi_L * E_D_H + iota * L_N_H - (eta_H + psi_T + 1/gamma + xi_m + (1 - delta)) * L_D_H;
      real dT_D_H = delta * T_X_H + psi_T * L_D_H + iota * T_N_H - (mu + nu + 1/gamma + xi_m + (1 - delta)) * T_D_H;
      real dR_D_H = delta * R_X_H + mu * (P_D_H + S_D_H + T_D_H) + eta_H * (E_D_H + L_D_H) + iota * R_N_H - (rho + 1/gamma + xi_N + (1 - delta)) * R_D_H;
      
      # doxy_intolerant (M)
      real dU_M_H = xi_m * U_D_H + rho * R_M_H - (lambda_H + 1/gamma) * U_M_H;
      real dI_M_H = xi_m * I_D_H + lambda_H * U_M_H - (sigma + 1/gamma) * I_M_H;
      real dP_M_H = xi_m * P_D_H + sigma * I_M_H - (mu + psi_S + 1/gamma) * P_M_H;
      real dS_M_H = xi_m * S_D_H + psi_S * P_M_H - (mu + psi_E + 1/gamma) * S_M_H;
      real dE_M_H = xi_m * E_D_H + psi_E * S_M_H - (eta_H + psi_L + 1/gamma) * E_M_H;
      real dL_M_H = xi_m * L_D_H + psi_L * E_M_H - (eta_H + psi_T + 1/gamma) * L_M_H;
      real dT_M_H = xi_m * T_D_H + psi_T * L_M_H - (mu + nu + 1/gamma) * T_M_H;
      real dR_M_H = xi_m * R_D_H + mu * (P_M_H + S_M_H + T_M_H) + eta_H * (E_M_H + L_M_H) - (rhp + 1/gamma) * R_M_H;

      # low-risk group
      # non-doxy-pep (N)
      real dU_N_L = q_L * alpha * (1 - p_DbE) - (lambda_L + p_DoS_L * eta_L + 1/gamma + iota) * U_N_L + (1 - p_DoD) * rho * R_N_L + xi_N * U_X_L;
      real dI_N_L = lambda_L * U_N_L - (sigma + 1/gamma + iota) * I_N_L + xi_N * I_X_L;
      real dP_N_L = sigma * I_N_L - (mu + psi_S + 1/gamma + iota) * P_N_L + xi_N * P_X_L;
      real dS_N_L = psi_S * P_N_L - (mu + psi_E + 1/gamma + iota) * S_N_L + xi_N * S_X_L;
      real dE_N_L = psi_E * S_N_L - (eta_L + psi_L + 1/gamma + iota) * E_N_L + xi_N * E_X_L;
      real dL_N_L = psi_L * E_N_L - (eta_L + psi_T + 1/gamma + iota) * L_N_L + xi_N * L_X_L;
      real dT_N_L = psi_T * L_N_L - (mu + nu + 1/gamma + iota) * T_N_L + xi_N * T_X_L;
      real dR_N_L = mu * (P_N_L + S_N_L + T_N_L) + eta_L * (E_N_L + L_N_L) - (rho + 1/gamma + iota) * R_N_L + xi_N * R_X_L;
      
      # doxy-inconsistent (X)
      real dU_X_L = (1 - delta) * U_D_L + rho * R_X_L - ((1 - zeta * e_d) * lambda_L + 1/gamma + xi_N + delta) * U_X_L;
      real dI_X_L = (1 - delta) * I_D_L + (1 - zeta * e_d) * lambda_L * U_X_L - (sigma + 1/gamma + xi_N + delta) * I_X_L;
      real dP_X_L = (1 - delta) * P_D_L + sigma * I_X_L - (mu + psi_S + 1/gamma + xi_N + delta) * P_X_L;
      real dS_X_L = (1 - delta) * S_D_L + psi_S * P_X_L - (mu + psi_E + 1/gamma + xi_N + delta) * S_X_L;
      real dE_X_L = (1 - delta) * E_D_L + psi_E * S_X_L - (eta_L + psi_L + 1/gamma + xi_N + delta) * E_X_L;
      real dL_X_L = (1 - delta) * L_D_L + psi_L * E_X_L - (eta_L + psi_T + 1/gamma + xi_N + delta) * L_X_L;
      real dT_X_L = (1 - delta) * T_D_L + psi_T * L_X_L - (mu + nu + 1/gamma + xi_N + delta) * T_X_L;
      real dR_X_L = (1 - delta) * R_D_L + mu * (P_X_L + S_X_L + T_X_L) + eta_L * (E_X_L + L_X_L) - (rho + 1/gamma + xi_N + delta) * R_X_L;
      
      # doxy-pep (D)
      real dU_D_L = q_L * alpha * p_DbE + (p_DoS_L * eta_L + iota) * U_N_L + p_DoD * rho * R_N_L + delta * U_X_L + rho * R_D_L - ((1 - e_d) * lambda_L + 1/gamma + (1 - delta) + xi_m) * U_D_L;
      real dI_D_L = (1 - e_d) * lambda_L * U_D_L + delta * I_X_L + iota * I_N_L - (sigma + 1/gamma + (1 - delta) + xi_m) * I_D_L;
      real dP_D_L = delta * P_X_L + sigma * I_D_L + iota * P_N_L - (mu + psi_S + 1/gamma + xi_m + (1 - delta)) * P_D_L;
      real dS_D_L = delta * S_X_L + psi_S * P_D_L + iota * S_N_L - (mu + psi_E + 1/gamma + xi_m + (1 - delta)) * S_D_L;
      real dE_D_L = delta * E_X_L + psi_E * S_D_L + iota * E_N_L - (eta_L + psi_L + 1/gamma + xi_m + (1 - delta)) * E_D_L;
      real dL_D_L = delta * L_X_L + psi_L * E_D_L + iota * L_N_L - (eta_L + psi_T + 1/gamma + xi_m + (1 - delta)) * L_D_L;
      real dT_D_L = delta * T_X_L + psi_T * L_D_L + iota * T_N_L - (mu + nu + 1/gamma + xi_m + (1 - delta)) * T_D_L;
      real dR_D_L = delta * R_X_L + mu * (P_D_L + S_D_L + T_D_L) + eta_L * (E_D_L + L_D_L) + iota * R_N_L - (rho + 1/gamma + xi_N + (1 - delta)) * R_D_L;
      
      # doxy_intolerant (M)
      real dU_M_L = xi_m * U_D_L + rho * R_M_L - (lambda_L + 1/gamma) * U_M_L;
      real dI_M_L = xi_m * I_D_L + lambda_L * U_M_L - (sigma + 1/gamma) * I_M_L;
      real dP_M_L = xi_m * P_D_L + sigma * I_M_L - (mu + psi_S + 1/gamma) * P_M_L;
      real dS_M_L = xi_m * S_D_L + psi_S * P_M_L - (mu + psi_E + 1/gamma) * S_M_L;
      real dE_M_L = xi_m * E_D_L + psi_E * S_M_L - (eta_L + psi_L + 1/gamma) * E_M_L;
      real dL_M_L = xi_m * L_D_L + psi_L * E_M_L - (eta_L + psi_T + 1/gamma) * L_M_L;
      real dT_M_L = xi_m * T_D_L + psi_T * L_M_L - (mu + nu + 1/gamma) * T_M_L;
      real dR_M_L = xi_m * R_D_L + mu * (P_M_L + S_M_L + T_M_L) + eta_L * (E_M_L + L_M_L) - (rhp + 1/gamma) * R_M_L;
      
      return {dU_N_H, dI_N_H, dP_N_H, dS_N_H, dE_N_H, dL_N_H, dT_N_H, dR_N_H, dU_X_H, dI_X_H, dP_X_H, dS_X_H, dE_X_H, dL_X_H, dT_X_H, dR_X_H, dU_D_H, dI_D_H, dP_D_H, dS_D_H, dE_D_H, dL_D_H, dT_D_H, dR_D_H, dU_M_H, dI_M_H, dP_M_H, dS_M_H, dE_M_H, dL_M_H, dT_M_H, dR_M_H
      dU_N_L, dI_N_L, dP_N_L, dS_N_L, dE_N_L, dL_N_L, dT_N_L, dR_N_L, dU_X_L, dI_X_L, dP_X_L, dS_X_L, dE_X_L, dL_X_L, dT_X_L, dR_X_L, dU_D_L, dI_D_L, dP_D_L, dS_D_L, dE_D_L, dL_D_L, dT_D_L, dR_D_L, dU_M_L, dI_M_L, dP_M_L, dS_M_L, dE_M_L, dL_M_L, dT_M_L, dR_M_L};
  }
}
data {
  int<lower=1> n_years; # Number of years
  real y0[64]; # Initial conditions for all compartments
  real ts[n_years]; # Sequences of time steps
  real t_0;
  real q_H;
  real p_DbE;
  real c_H;
  real c_L;
  real p_DoS_H;
  real p_DoD;
  real e_d;
  real q_L;
  real p_DoS_L;
  int cases[n_years]; # Annual syphilis cases
  int alpha;
  int N_t0;
  int gamma;
}
transformed data {
  real x_r[10];
  int x_i[3];
  
  # assign values to x_r
  x_r[1] = q_H;
  x_r[2] = p_DbE;
  x_r[3] = c_H;
  x_r[4] = c_L;
  x_r[5] = t_0;
  x_r[6] = p_DoS_H;
  x_r[7] = p_DoD;
  x_r[8] = e_d;
  x_r[9] = q_L;
  x_r[10] = p_DoS_L;
  
  # assign values to x_i
  x_i[1] = alpha;
  x_i[2] = N_t0;
  x_i[3] = gamma;
}
parameters {
  real<lower=0, upper=1> beta;
  real<lower=0, upper=1> phi_beta;
  real<lower=0, upper=1> epsilon;
  real<lower=0> rho;
  real<lower=0, upper=4> eta_H_init;
  real<lower=0, upper=1> phi_eta;
  real<lower=0> omega;
  real<lower=0> iota;
  real<lower=0> k_n;
  real<lower=0> xi_N_init;
  real<lower=0> sigma;
  real<lower=0> mu;
  real<lower=0> psi_S;
  real<lower=0> psi_E;
  real<lower=0> psi_L;
  real<lower=0> psi_T;
  real<lower=0> nu;
  real<lower=0> k_delta;
  real<lower=0> delta_init;
  real<lower=0, upper=1> zeta;
  real<lower=0> xi_m;
  real<lower=0, upper=1> kappa_D;
}
transformed parameters{
  real y[n_years, 64];
  real incidence[n_years - 1];
  real theta[22];
  theta[1] = beta;
  theta[2] = phi_beta;
  theta[3] = epsilon;
  theta[4] = rho;
  theta[5] = eta_H_init;
  theta[6] = phi_eta;
  theta[7] = omega;
  theta[8] = iota;
  theta[9] = k_n;
  theta[10] = xi_N_init;
  theta[11] = sigma;
  theta[12] = mu;
  theta[13] = psi_S;
  theta[14] = psi_E;
  theta[15] = psi_L;
  theta[16] = psi_T;
  theta[17] = nu;
  theta[18] = k_delta;
  theta[19] = delta_init;
  theta[20] = zeta;
  theta[21] = xi_m;
  theta[22] = kappa_D;

  y = integrate_ode_rk45(doxypep_syphilis_model, y0, t_0, ts, theta, x_r, x_i);
  for (t in 1:(n_years - 1)) {
    // Trapezoidal rule: (f(a) + f(b)) / 2 * (b - a)
    incidence[t] = 0.5 * rho * (y[t, 2] + y[t + 1, 2] + y[t, 16] + y[t + 1, 16] + y[t, 24] + y[t + 1, 24] + y[t, 26] + y[t + 1, 26] + y[t, 34] + y[t + 1, 34] + y[t, 48] + y[t + 1, 48] + y[t, 56] + y[t + 1, 56] + y[t, 58] + y[t + 1, 58]);
  }
}