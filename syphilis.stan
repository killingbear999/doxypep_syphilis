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
  real get_zetaN(real t, real t_0, int zeta_N_max, real zeta_N_init, real k_n) {
    # compute discontinuation rate due to transitioning to alternative prevention methods
    return(zeta_N_max / (1 + ((zeta_N_max - zeta_N_init) / zeta_N_init) * e^(-k_n * t)));
  }
  real[] syphilis(real t, real[] y, real[] theta, real[] x_r, int[] x_i) {
      # initial conditions
      real U_N_H = y[1];
      real R_N_H = y[2];
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
      real R_X_H = y[16];
      
      # fixed real parameters
      real q_H = x_r[1]; # Proportion of the MSM population in group H
      real p_DbE = x_r[2]; # Probability of uptake of doxycycline before entry into the sexually-active population
      real c_H = x_r[3]; # Annual rate of partner change in group H
      real c_L = x_r[4]; # Annual rate of partner change in group L
      real t_0 = x_r[5]; # Initial time
      real p_DoS_H = x_r[6]; # Probability of uptake of doxycycline on screening with negative results in group H
      real p_DoD = x_r[7]; # Probability of uptake of doxycycline on diagnosis
      
      # fixed integer parameters
      int alpha = x_i[1]; # Annual MSM population entrants (at age 15)
      int N_t0 = x_i[2]; # Initial population size of MSM
      int gamma = x_i[3]; # Years spent in the sexually-active population
      int zeta_N_max = x_i[4]; # Maximum proportion of MSM discontinued doxy-PEP
      
      # model parameters
      real beta = theta[1]; # Probability of transmission per partnership
      real phi_beta = theta[2]; # Annual increase in transmission risk behaviour
      real epsilon = theta[3]; # Level of assortativity in sexual mixing
      real rho = theta[4]; # Rate of recovery after treatment
      real eta_H_init = theta[5]; # Initial rate of asymptomatic screening in group H
      real phi_eta = theta[6]; # Annual increase in screening rate
      real omega = theta[7]; # Ratio of screening rate in group L vs group H
      real iota = theta[8]; # Rate of doxycycline acquisition from non-STI medical clinics
      real k_n = theta[9]; # Transition speed for discontinuation of doxy-PEP
      real zeta_N_init = theta[10];  # Initial discontinuation rate due to transitioning to alternative prevention methods
      real sigma = theta[11]; # Rate of leaving incubation period
      real mu = theta[12]; # Rate of seeking treatment due to symptoms 
      real psi_S = theta[13]; # Rate of leaving primary stage
      real psi_E = theta[14]; # Rate of leaving secondary stage
      real psi_L = theta[15]; # Rate of leaving early latent stage
      real psi_T = theta[16]; # Rate of leaving late latent stage
      real nu = theta[17]; # Mortality rate at tertiary stage
      
      # time-dependent variables
      real C_H = get_C(I_N_H, P_N_H, S_N_H, E_N_H, I_X_H, P_X_H, S_X_H, E_X_H, I_D_H, P_D_H, S_D_H, E_D_H, I_M_H, P_M_H, S_M_H, E_M_H);
      real C_L = get_C(I_N_L, P_N_L, S_N_L, E_N_L, I_X_L, P_X_L, S_X_L, E_X_L, I_D_L, P_D_L, S_D_L, E_D_L, I_M_L, P_M_L, S_M_L, E_M_L);
      real N_H = get_N(U_N_H, I_N_H, P_N_H, S_N_H, E_N_H, L_N_H, T_N_H, R_N_H, U_X_H, I_X_H, P_X_H, S_X_H, E_X_H, L_X_H, T_X_H, R_X_H, U_D_H, I_D_H, P_D_H, S_D_H, E_D_H, L_D_H, T_D_H, R_D_H, U_M_H, I_M_H, P_M_H, S_M_H, E_M_H, L_M_H, T_M_H, R_M_H);
      real N_L = get_N(U_N_L, I_N_L, P_N_L, S_N_L, E_N_L, L_N_L, T_N_L, R_N_L, U_X_L, I_X_L, P_X_L, S_X_L, E_X_L, L_X_L, T_X_L, R_X_L, U_D_L, I_D_L, P_D_L, S_D_L, E_D_L, L_D_L, T_D_L, R_D_L, U_M_L, I_M_L, P_M_L, S_M_L, E_M_L, L_M_L, T_M_L, R_M_L);
      real pi_H = get_pi(c_H, N_H, c_L, N_L);
      real pi_L = get_pi(c_L, N_L, c_H, N_H);
      real lambda_H = get_lambda(t, t_0, c_H, beta, phi_beta, epsilon, C_H, N_H, pi_H, C_L, N_L, pi_L);
      real eta_H = get_eta(t, t_0, eta_H_init, phi_eta);
      real eta_L = omega * eta_H;
      real zeta_N = get_zetaN(t, t_0, zeta_N_max, zeta_N_init, k_n);
      
      # ODEs
      # high-risk group
      # non-doxy-pep (N)
      real dU_N_H = q_H * alpha * (1 - p_DbE) - (lambda_H + p_DoS_H * eta_H + 1/gamma + iota) * U_N_H + (1 - p_DoD) * rho * R_N_H + zeta_N * U_X_H;
      real dI_N_H = lambda_H * U_N_H - (sigma + 1/gamma + iota) * I_N_H + zeta_N * I_X_H;
      real dP_N_H = sigma * I_N_H - (mu + psi_S + 1/gamma + iota) * P_N_H + zeta_N * P_X_H;
      real dS_N_H = psi_S * P_N_H - (mu + psi_E + 1/gamma + iota) * S_N_H + zeta_N * S_X_H;
      real dE_N_H = psi_E * S_N_H - (eta_H + psi_L + 1/gamma + iota) * E_N_H + zeta_N * E_X_H;
      real dL_N_H = psi_L * E_N_H - (eta_H + psi_T + 1/gamma + iota) * L_N_H + zeta_N * L_X_H;
      real dT_N_H = psi_T * L_N_H - (mu + nu + 1/gamma + iota) * T_N_H + zeta_N * T_X_H;
      real dR_N_H = mu * (P_N_H + S_N_H + T_N_H) + eta_H * (E_N_H + L_N_H) - (rho + 1/gamma + iota) * R_N_H + zeta_N * R_X_H;
      
      return {dU_N_H, dI_N_H, dP_N_H, dS_N_H, dE_N_H, dL_N_H, dT_N_H, dR_N_H};
  }
}