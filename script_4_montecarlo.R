Script 4: Simulación Monte Carlo
Corresponde a las secciones 4.4 y 6.4. Implementa la simulación de pérdida agregada con 100.000 iteraciones bajo el modelo de riesgo colectivo (N ~ BN, X ~ Lognormal), estima la prima pura y los percentiles de pérdida, y produce los gráficos A19–A21
# =============================================================================
# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Paso 4: Simulación Monte Carlo — Distribución de pérdida agregada
# Prerequisito: correr procesamiento_encuesta_MTB.R + pasos 2 y 3
# =============================================================================
library(tidyverse)
library(ggplot2)
set.seed(42)
# =============================================================================
# A. PARÁMETROS DE ENTRADA
# =============================================================================
cat("=== A. PARÁMETROS DE ENTRADA ===\n")
lambda_anual <- mean(datos_modelo$n_acc_24m, na.rm=TRUE) / 2
theta_bn     <- mod_bn2$theta
mu_sev       <- mu_ln
sigma_sev    <- sigma_ln
N_SIM        <- 100000
TC           <- 1400
LIMITE_ARS   <- 3000000  # límite máximo por siniestro
cat("Frecuencia — Binomial Negativa:\n")
cat("  Lambda anual:", round(lambda_anual, 4), "\n")
cat("  Theta:       ", round(theta_bn, 4), "\n")
cat("\nSeveridad — Lognormal:\n")
cat("  mu:    ", round(mu_sev, 4), "\n")
cat("  sigma: ", round(sigma_sev, 4), "\n")
cat("\nSimulaciones:", N_SIM, "\n")
cat("Límite por siniestro: ARS", format(LIMITE_ARS, big.mark=","), "\n")
# =============================================================================
# B. FUNCIONES DE SIMULACIÓN
# =============================================================================
simular_perdida <- function(n_sim, lambda, theta, mu, sigma, limite=Inf) {

perdidas <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    n_acc <- rnbinom(1, mu = lambda, size = theta)
    if (n_acc == 0) {
      perdidas[i] <- 0
    } else {
      costos <- rlnorm(n_acc, meanlog = mu, sdlog = sigma)

costos <- pmin(costos, limite)
      perdidas[i] <- sum(costos)
    }
  }
  perdidas
}
# =============================================================================
# C. SIMULACIÓN SIN LÍMITE (cobertura ilimitada)
# =============================================================================
cat("\n=== C. SIMULACIÓN SIN LÍMITE ===\n")
set.seed(42)
perdida_sl <- simular_perdida(N_SIM, lambda_anual, theta_bn, mu_sev, sigma_sev)
prima_pura_sl <- mean(perdida_sl)
cat("Sin siniestro:", round(mean(perdida_sl==0)*100,1), "%\n")
cat("Media S:  ARS", format(round(mean(perdida_sl)), big.mark=","), "\n")
cat("P95:      ARS", format(round(quantile(perdida_sl,0.95)), big.mark=","), "\n")
cat("P99:      ARS", format(round(quantile(perdida_sl,0.99)), big.mark=","), "\n")
cat("\nPrima pura sin límite:\n")
cat("  ARS/año:   ", format(round(prima_pura_sl), big.mark=","), "\n")
cat("  USD/mes:   ", round(prima_pura_sl/TC/12, 2), "\n")
# =============================================================================
# D. SIMULACIÓN CON LÍMITE ARS 3.000.000
# =============================================================================
cat("\n=== D. SIMULACIÓN CON LÍMITE ARS 3.000.000 ===\n")
set.seed(42)
perdida_cl <- simular_perdida(N_SIM, lambda_anual, theta_bn, mu_sev, sigma_sev, LIMITE_ARS)
prima_pura_cl <- mean(perdida_cl)
cat("Sin siniestro:", round(mean(perdida_cl==0)*100,1), "%\n")
cat("Media S:  ARS", format(round(mean(perdida_cl)), big.mark=","), "\n")
cat("P95:      ARS", format(round(quantile(perdida_cl,0.95)), big.mark=","), "\n")
cat("P99:      ARS", format(round(quantile(perdida_cl,0.99)), big.mark=","), "\n")
cat("\nPrima pura con límite ARS 3.000.000:\n")
cat("  ARS/año:   ", format(round(prima_pura_cl), big.mark=","), "\n")
cat("  USD/mes:   ", round(prima_pura_cl/TC/12, 2), "\n")
# =============================================================================
# E. COMPARACIÓN Y PRIMA COMERCIAL
# =============================================================================
cat("\n=== E. COMPARACIÓN Y PRIMA COMERCIAL ===\n")
RECARGO_SEGURIDAD <- 0.15
RECARGO_GASTOS    <- 0.12
RECARGO_UTILIDAD  <- 0.08
FACTOR_COMERCIAL  <- (1 + RECARGO_SEGURIDAD) * (1 + RECARGO_GASTOS) * (1 + RECARGO_UTILIDAD)
prima_com_sl <- round(prima_pura_sl/TC/12 * FACTOR_COMERCIAL, 2)
prima_com_cl <- round(prima_pura_cl/TC/12 * FACTOR_COMERCIAL, 2)
comp <- data.frame(
  Plan              = c("Sin límite", "Con límite ARS 3M"),
  Prima_pura_usd    = round(c(prima_pura_sl/TC/12, prima_pura_cl/TC/12), 2),
  Prima_com_usd     = c(prima_com_sl, prima_com_cl),

P95_ARS           = format(round(c(quantile(perdida_sl,0.95),

quantile(perdida_cl,0.95))), big.mark=","),

P99_ARS           = format(round(c(quantile(perdida_sl,0.99),

quantile(perdida_cl,0.99))), big.mark=",")
)
print(comp)
cat("\nFactor comercial:", round(FACTOR_COMERCIAL, 3), "\n")
cat("WTP promedio referencia: USD 22.67/mes\n")
# =============================================================================
# F. SENSIBILIDAD FACTOR PRIVADO — ambos planes
# =============================================================================
cat("\n=== F. SENSIBILIDAD FACTOR PRIVADO ===\n")
factores_priv <- c(1.0, 1.5, 2.0, 2.5)
sens <- data.frame(factor_privado = factores_priv,
                   prima_sl_usd   = NA,
                   prima_cl_usd   = NA)
for (i in seq_along(factores_priv)) {

f        <- factores_priv[i]
  media_orig <- exp(mu_sev + sigma_sev^2/2)
  media_aj   <- media_orig * (0.63 * f + 0.37)
  mu_aj      <- log(media_aj) - sigma_sev^2/2
  set.seed(42)
  s_sl <- simular_perdida(50000, lambda_anual, theta_bn, mu_aj, sigma_sev)
  s_cl <- simular_perdida(50000, lambda_anual, theta_bn, mu_aj, sigma_sev, LIMITE_ARS)

sens$prima_sl_usd[i] <- round(mean(s_sl)/TC/12, 2)
  sens$prima_cl_usd[i] <- round(mean(s_cl)/TC/12, 2)
}
cat("\nPrima pura según factor privado (USD/mes):\n")
print(sens)
# =============================================================================
# G. GRÁFICOS
# =============================================================================
cat("\n=== G. GENERANDO GRÁFICOS ===\n")
# Distribución pérdida sin límite
sev_sl <- perdida_sl[perdida_sl > 0]
p_sl <- ggplot(data.frame(s=sev_sl), aes(x=s)) +
  geom_histogram(aes(y=after_stat(density)), bins=40,
                 fill="#378ADD", alpha=0.6, color="white") +
  geom_vline(xintercept=mean(perdida_sl), color="#e24b4a", linewidth=1.2) +
  geom_vline(xintercept=quantile(perdida_sl,0.95), color="#EF9F27",
             linewidth=1.2, linetype="dashed") +
  scale_x_continuous(labels=scales::comma) +

labs(title="Pérdida agregada anual — Sin límite (S > 0)",
       subtitle="Rojo: media | Naranja: P95",
       x="Pérdida ARS", y="Densidad") +

theme_minimal(base_size=12)
ggsave("graf_18a_montecarlo_sinlimite.png", p_sl, width=9, height=4, dpi=150)
# Distribución pérdida con límite
sev_cl <- perdida_cl[perdida_cl > 0]
p_cl <- ggplot(data.frame(s=sev_cl), aes(x=s)) +
  geom_histogram(aes(y=after_stat(density)), bins=40,
                 fill="#1D9E75", alpha=0.6, color="white") +
  geom_vline(xintercept=mean(perdida_cl), color="#e24b4a", linewidth=1.2) +
  geom_vline(xintercept=quantile(perdida_cl,0.95), color="#EF9F27",
             linewidth=1.2, linetype="dashed") +
  scale_x_continuous(labels=scales::comma) +

labs(title="Pérdida agregada anual — Con límite ARS 3.000.000 (S > 0)",
       subtitle="Rojo: media | Naranja: P95",
       x="Pérdida ARS", y="Densidad") +

theme_minimal(base_size=12)
ggsave("graf_18b_montecarlo_conlimite.png", p_cl, width=9, height=4, dpi=150)
cat("Gráficos guardados.\n")
# =============================================================================
# H. RESUMEN EJECUTIVO PASO 4
# =============================================================================
cat("\n=== H. RESUMEN EJECUTIVO PASO 4 ===\n")
cat("
PARÁMETROS:
  - Lambda anual:  ", round(lambda_anual, 4), "

- Theta BN:      ", round(theta_bn, 4), "
  - Mu Lognormal:  ", round(mu_sev, 4), "

- Sigma:         ", round(sigma_sev, 4), "
PLAN SIN LÍMITE:

- Sin siniestro: ", round(mean(perdida_sl==0)*100,1), "%
  - Media S:       ARS", format(round(prima_pura_sl), big.mark=","), "
  - P95:           ARS", format(round(quantile(perdida_sl,0.95)), big.mark=","), "
  - P99:           ARS", format(round(quantile(perdida_sl,0.99)), big.mark=","), "

- Prima pura:    USD", round(prima_pura_sl/TC/12, 2), "/mes
  - Prima com:     USD", prima_com_sl, "/mes
PLAN CON LÍMITE ARS 3.000.000:
  - Sin siniestro: ", round(mean(perdida_cl==0)*100,1), "%

- Media S:       ARS", format(round(prima_pura_cl), big.mark=","), "
  - P95:           ARS", format(round(quantile(perdida_cl,0.95)), big.mark=","), "
  - P99:           ARS", format(round(quantile(perdida_cl,0.99)), big.mark=","), "

- Prima pura:    USD", round(prima_pura_cl/TC/12, 2), "/mes
  - Prima com:     USD", prima_com_cl, "/mes
FACTOR COMERCIAL:", round(FACTOR_COMERCIAL, 3), "
")
