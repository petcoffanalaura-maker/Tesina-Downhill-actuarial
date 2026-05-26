Script 5: Bootstrap
Corresponde a las secciones 4.5 y 6.5. Implementa el bootstrap no paramétrico con B=2.000 iteraciones sobre los datos de frecuencia (BN) y severidad (Lognormal), construye los intervalos de confianza al 95% para λ, μ y σ y la prima pura mensual, y produce los gráficos A22–A24.
# =============================================================================
# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Paso 5: Bootstrap — estabilidad de estimaciones
# Prerequisito: correr procesamiento_encuesta_MTB.R + pasos 2, 3 y 4
# =============================================================================
library(tidyverse)
library(fitdistrplus)
library(ggplot2)
set.seed(42)
N_BOOT <- 2000
# =============================================================================
# A. BOOTSTRAP FRECUENCIA
# =============================================================================
cat("=== A. BOOTSTRAP FRECUENCIA ===\n")
lambda_boot <- numeric(N_BOOT)
for (b in 1:N_BOOT) {

muestra_b      <- datos_modelo[sample(nrow(datos_modelo), replace=TRUE), ]

lambda_boot[b] <- mean(muestra_b$n_acc_24m, na.rm=TRUE) / 2
}
cat("Estimación puntual:", round(lambda_anual, 4), "\n")
cat("Media bootstrap:   ", round(mean(lambda_boot), 4), "\n")
cat("IC 95%: [", round(quantile(lambda_boot, 0.025), 4),
    ",", round(quantile(lambda_boot, 0.975), 4), "]\n")
# =============================================================================
# B. BOOTSTRAP SEVERIDAD — Lognormal
# =============================================================================
cat("\n=== B. BOOTSTRAP SEVERIDAD (Lognormal) ===\n")
mu_boot    <- numeric(N_BOOT)
sigma_boot <- numeric(N_BOOT)
for (b in 1:N_BOOT) {
  muestra_b     <- sev$costo_total[sample(nrow(sev), replace=TRUE)]
  fit_b         <- fitdist(muestra_b, "lnorm", method = "mle")
  mu_boot[b]    <- fit_b$estimate["meanlog"]
  sigma_boot[b] <- fit_b$estimate["sdlog"]
}
cat("mu - Estimación puntual:", round(mu_ln, 4), "\n")
cat("mu - IC 95%: [", round(quantile(mu_boot, 0.025), 4),
    ",", round(quantile(mu_boot, 0.975), 4), "]\n")
cat("\nsigma - Estimación puntual:", round(sigma_ln, 4), "\n")
cat("sigma - IC 95%: [", round(quantile(sigma_boot, 0.025), 4),
    ",", round(quantile(sigma_boot, 0.975), 4), "]\n")
# =============================================================================
# C. BOOTSTRAP PRIMA PURA — ambos planes
# =============================================================================
cat("\n=== C. BOOTSTRAP PRIMA PURA ===\n")
cat("Combinando incertidumbre de frecuencia y severidad...\n")
prima_boot_sl <- numeric(N_BOOT)  # sin límite
prima_boot_cl <- numeric(N_BOOT)  # con límite ARS 3.000.000
for (b in 1:N_BOOT) {

muestra_freq <- datos_modelo[sample(nrow(datos_modelo), replace=TRUE), ]

lam_b <- mean(muestra_freq$n_acc_24m, na.rm=TRUE) / 2
  muestra_sev <- sev$costo_total[sample(nrow(sev), replace=TRUE)]
  fit_sev     <- fitdist(muestra_sev, "lnorm", method = "mle")
  mu_b        <- fit_sev$estimate["meanlog"]
  sigma_b     <- fit_sev$estimate["sdlog"]
  n_sim_b <- 10000
  s_sl_b  <- numeric(n_sim_b)
  s_cl_b  <- numeric(n_sim_b)
  for (i in 1:n_sim_b) {
    n_acc <- rnbinom(1, mu = lam_b, size = theta_bn)
    if (n_acc == 0) {
      s_sl_b[i] <- 0
      s_cl_b[i] <- 0
    } else {
      costos    <- rlnorm(n_acc, meanlog = mu_b, sdlog = sigma_b)
      s_sl_b[i] <- sum(costos)
      s_cl_b[i] <- sum(pmin(costos, LIMITE_ARS))
    }
  }
  prima_boot_sl[b] <- mean(s_sl_b)
  prima_boot_cl[b] <- mean(s_cl_b)
}
prima_boot_sl_usd <- prima_boot_sl / TC / 12
prima_boot_cl_usd <- prima_boot_cl / TC / 12
cat("\n--- Plan sin límite ---\n")
cat("Estimación puntual: USD", round(prima_pura_sl/TC/12, 2), "\n")
cat("Media bootstrap:    USD", round(mean(prima_boot_sl_usd), 2), "\n")
cat("IC 95%: [USD", round(quantile(prima_boot_sl_usd, 0.025), 2),
    ", USD", round(quantile(prima_boot_sl_usd, 0.975), 2), "]\n")
cat("Amplitud IC: USD", round(diff(quantile(prima_boot_sl_usd, c(0.025, 0.975))), 2), "\n")
cat("\n--- Plan con límite ARS 3.000.000 ---\n")
cat("Estimación puntual: USD", round(prima_pura_cl/TC/12, 2), "\n")
cat("Media bootstrap:    USD", round(mean(prima_boot_cl_usd), 2), "\n")
cat("IC 95%: [USD", round(quantile(prima_boot_cl_usd, 0.025), 2),
    ", USD", round(quantile(prima_boot_cl_usd, 0.975), 2), "]\n")
cat("Amplitud IC: USD", round(diff(quantile(prima_boot_cl_usd, c(0.025, 0.975))), 2), "\n")
# =============================================================================
# D. BOOTSTRAP COEFICIENTES GLM
# =============================================================================
cat("\n=== D. BOOTSTRAP COEFICIENTES GLM ===\n")
coef_boot <- matrix(NA, nrow=N_BOOT, ncol=length(coef(mod_bn2)))
colnames(coef_boot) <- names(coef(mod_bn2))
for (b in 1:N_BOOT) {

muestra_b <- datos_modelo[sample(nrow(datos_modelo), replace=TRUE), ]

tryCatch({
    mod_b <- glm(
      n_acc_24m ~ nivel_f2 + disc_f + region_f + tendencia_f +
        anios_practica + compite_f + offset(log_expo),
      family = poisson(link="log"),
      data   = muestra_b
    )
    coef_boot[b, ] <- coef(mod_b)
  }, error = function(e) NULL)
}
cat("\n--- IC 95% bootstrap coeficientes GLM ---\n")
ic_coef <- apply(coef_boot, 2, function(x) {
  x <- x[!is.na(x)]
  c(media   = round(mean(x), 4),
    ic_low  = round(quantile(x, 0.025), 4),
    ic_high = round(quantile(x, 0.975), 4),
    se      = round(sd(x), 4))
})
print(t(ic_coef))
# =============================================================================
# E. GRÁFICOS
# =============================================================================
cat("\n=== E. GENERANDO GRÁFICOS ===\n")
p_lam <- ggplot(data.frame(lambda=lambda_boot), aes(x=lambda)) +
  geom_histogram(fill="#378ADD", color="white", bins=40, alpha=0.7) +
  geom_vline(xintercept=lambda_anual, color="#e24b4a", linewidth=1.2) +
  geom_vline(xintercept=quantile(lambda_boot, c(0.025, 0.975)),
             color="#EF9F27", linewidth=1, linetype="dashed") +
  labs(title="Bootstrap: lambda (tasa anual)",

x="Lambda", y="Frecuencia") +
  theme_minimal(base_size=12)
ggsave("graf_21_boot_lambda.png", p_lam, width=7, height=4, dpi=150)
p_sl <- ggplot(data.frame(prima=prima_boot_sl_usd), aes(x=prima)) +
  geom_histogram(fill="#378ADD", color="white", bins=40, alpha=0.7) +
  geom_vline(xintercept=prima_pura_sl/TC/12, color="#e24b4a", linewidth=1.2) +
  geom_vline(xintercept=quantile(prima_boot_sl_usd, c(0.025, 0.975)),
             color="#EF9F27", linewidth=1, linetype="dashed") +

labs(title="Bootstrap: prima pura mensual sin límite (USD)",
       x="Prima pura (USD/mes)", y="Frecuencia") +

theme_minimal(base_size=12)
ggsave("graf_22_boot_prima_sl.png", p_sl, width=7, height=4, dpi=150)
p_cl <- ggplot(data.frame(prima=prima_boot_cl_usd), aes(x=prima)) +
  geom_histogram(fill="#1D9E75", color="white", bins=40, alpha=0.7) +

geom_vline(xintercept=prima_pura_cl/TC/12, color="#e24b4a", linewidth=1.2) +

geom_vline(xintercept=quantile(prima_boot_cl_usd, c(0.025, 0.975)),
             color="#EF9F27", linewidth=1, linetype="dashed") +

labs(title="Bootstrap: prima pura mensual con límite ARS 3M (USD)",
       x="Prima pura (USD/mes)", y="Frecuencia") +

theme_minimal(base_size=12)
ggsave("graf_23_boot_prima_cl.png", p_cl, width=7, height=4, dpi=150)
cat("Gráficos guardados.\n")
# =============================================================================
# F. RESUMEN EJECUTIVO PASO 5
# =============================================================================
cat("\n=== F. RESUMEN EJECUTIVO PASO 5 ===\n")
cat("
BOOTSTRAP (", N_BOOT, "iteraciones):
FRECUENCIA (lambda anual):
  - Estimación puntual:", round(lambda_anual, 4), "

- IC 95%: [", round(quantile(lambda_boot, 0.025), 4),
    ",", round(quantile(lambda_boot, 0.975), 4), "]
SEVERIDAD (mu Lognormal):

- Estimación puntual:", round(mu_ln, 4), "

- IC 95%: [", round(quantile(mu_boot, 0.025), 4),

",", round(quantile(mu_boot, 0.975), 4), "]
PRIMA PURA MENSUAL — SIN LÍMITE (USD):
  - Estimación puntual: USD", round(prima_pura_sl/TC/12, 2), "

- IC 95%: [USD", round(quantile(prima_boot_sl_usd, 0.025), 2),
    ", USD", round(quantile(prima_boot_sl_usd, 0.975), 2), "]
  - Amplitud IC: USD", round(diff(quantile(prima_boot_sl_usd, c(0.025, 0.975))), 2), "
PRIMA PURA MENSUAL — CON LÍMITE ARS 3M (USD):
  - Estimación puntual: USD", round(prima_pura_cl/TC/12, 2), "

- IC 95%: [USD", round(quantile(prima_boot_cl_usd, 0.025), 2),
    ", USD", round(quantile(prima_boot_cl_usd, 0.975), 2), "]
  - Amplitud IC: USD", round(diff(quantile(prima_boot_cl_usd, c(0.025, 0.975))), 2), "
")
