library(deSolve)

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
    
    # Population sizes
    C_H <- get_C(I_N_H, P_N_H, S_N_H, E_N_H)
    C_L <- get_C(I_N_L, P_N_L, S_N_L, E_N_L)
    N_H <- get_N(U_N_H, I_N_H, P_N_H, S_N_H, E_N_H, L_N_H, T_N_H, R_N_H)
    N_L <- get_N(U_N_L, I_N_L, P_N_L, S_N_L, E_N_L, L_N_L, T_N_L, R_N_L)
    
    # Mixing
    pi_H <- get_pi(c_H, N_H, c_L, N_L)
    pi_L <- get_pi(c_L, N_L, c_H, N_H)
    lambda_H <- get_lambda(t, t_0, c_H, beta, phi_beta, epsilon, C_H, N_H, pi_H, C_L, N_L, pi_L)
    lambda_L <- get_lambda(t, t_0, c_L, beta, phi_beta, epsilon, C_L, N_L, pi_L, C_H, N_H, pi_H)
    
    # Screening
    eta_H <- get_eta(t, t_0, eta_H_init, phi_eta)
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

# Initial population size of MSM in 2018
N_t0 <- 135000

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
n_years <- 21
t <- seq(0, n_years, by = 1)
t_0 = 0 
t <- t[-1]

# initial conditions
U_N_H = 27838.87
I_N_H = 59.43
P_N_H = 7.59
S_N_H = 0.67
E_N_H = 0.18
L_N_H = 0.22
T_N_H = 0.00
R_N_H = 48.18
U_N_L = 107086.05
I_N_L = 3.56
P_N_L = 0.56
S_N_L = 0.06
E_N_L = 0.03
L_N_L = 0.74
T_N_L = 0.00
R_N_L = 2.86
y0 = c(U_N_H=U_N_H, I_N_H=I_N_H, P_N_H=P_N_H, S_N_H=S_N_H, E_N_H=E_N_H, L_N_H=L_N_H, T_N_H=T_N_H, R_N_H=R_N_H,
       U_N_L=U_N_L, I_N_L=I_N_L, P_N_L=P_N_L, S_N_L=S_N_L, E_N_L=E_N_L, L_N_L=L_N_L, T_N_L=T_N_L, R_N_L=R_N_L)

# Parameters
params <- list(
  q_H = q_H, c_H = c_H, c_L = c_L, q_L = q_L,
  t_0 = 0, alpha = alpha, gamma = gamma,
  sigma = sigma, psi_S = psi_S, psi_E = psi_E, psi_L = psi_L, psi_T = psi_T,
  nu = nu, beta = 0.9625903, phi_beta = 0.003486564, epsilon=0.8932742, rho=19.83683, eta_H_init=2.070594, 
  phi_eta=0.4828739, omega=0.4151082, mu=118.45, beta_nu=beta_nu
)

# Solve the system
out <- ode(y = y0, times = c(t_0, t), func = syphilis_model, parms = params)

# View output
out <- as.data.frame(out)
print(out)

incidence <- numeric(n_years - 1)

for (t in 1:(n_years - 1)) {
  # Trapezoidal rule: (f(a) + f(b)) / 2 * (b - a)
  incidence[t] = 0.5 * params$rho * (out[t, 9] + out[t + 1, 9] + out[t, 17] + out[t + 1, 17]);
}
print(incidence)

kappa_D <- 0.91899
cases <- rnbinom(n = length(incidence), size = kappa_D, mu = incidence)
print(cases)