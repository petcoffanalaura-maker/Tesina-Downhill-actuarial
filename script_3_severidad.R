Script 3: Modelo de severidad
Corresponde a las secciones 4.3, 4.3.1, 4.3.2 y 6.3. Ajusta distribuciones Lognormal y Gamma a los 186 costos positivos por MLE, compara por AIC/BIC y tests KS y Anderson-Darling, y genera los gráficos A14–A18: ajuste de distribuciones, Q-Q plots y CDF empírica vs ajustada.
# =============================================================================
# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Paso 3: Modelo de severidad (Lognormal y Gamma)
# Prerequisito: correr primero procesamiento_encuesta_MTB.R
# =============================================================================
library(tidyverse)
library(fitdistrplus)
library(MASS)
library(moments)
library(goftest)
library(ggplot2)
# =============================================================================
# A. PREPARACIÓN DE DATOS DE SEVERIDAD
# =============================================================================
cat("=== A. DATOS DE SEVERIDAD ===\n")
sev <- acc_largo %>%
  filter(!is.na(costo_total) & costo_total > 0) %>%
  dplyr::select(costo_total, ISC, lesion, parte, disc, nivel, lugar)
cat("N accidentes con costo > 0:", nrow(sev), "\n")
cat("\nEstadísticas descriptivas del costo (ARS):\n")
print(summary(sev$costo_total))
cat("\nMomentos:\n")
cat("  Media:     ARS", format(round(mean(sev$costo_total)), big.mark=","), "\n")
cat("  Desvío:    ARS", format(round(sd(sev$costo_total)), big.mark=","), "\n")
cat("  CV:       ", round(sd(sev$costo_total)/mean(sev$costo_total), 4), "\n")
cat("  Skewness: ", round(moments::skewness(sev$costo_total), 4), "\n")
cat("  Kurtosis: ", round(moments::kurtosis(sev$costo_total), 4), "\n")
# =============================================================================
# B. AJUSTE LOGNORMAL
# =============================================================================
cat("\n=== B. AJUSTE LOGNORMAL ===\n")
fit_ln <- fitdist(sev$costo_total, "lnorm", method = "mle")
cat("\nParámetros Lognormal (MLE):\n")
print(summary(fit_ln))
mu_ln    <- fit_ln$estimate["meanlog"]
sigma_ln <- fit_ln$estimate["sdlog"]
cat("\nAIC Lognormal:", round(fit_ln$aic, 2), "\n")
cat("BIC Lognormal:", round(fit_ln$bic, 2), "\n")
cat("Media teórica:   ARS", format(round(exp(mu_ln + sigma_ln^2/2)), big.mark=","), "\n")
cat("Mediana teórica: ARS", format(round(exp(mu_ln)), big.mark=","), "\n")
# =============================================================================
# C. AJUSTE GAMMA (via optimización directa Nelder-Mead)
# =============================================================================
cat("\n=== C. AJUSTE GAMMA ===\n")
nloglik_gamma <- function(shape, scale) {
  if (shape <= 0 | scale <= 0) return(Inf)
  -sum(dgamma(sev$costo_total, shape=shape, scale=scale, log=TRUE))
}
media_orig <- mean(sev$costo_total)
var_orig   <- var(sev$costo_total)
scale_init <- var_orig / media_orig
shape_init <- media_orig / scale_init
opt <- optim(
  par     = c(shape=shape_init, scale=scale_init),
  fn      = function(p) nloglik_gamma(p[1], p[2]),
  method  = "Nelder-Mead",
  control = list(maxit=10000, reltol=1e-10)
)
shape_gm  <- opt$par[1]
scale_gm  <- opt$par[2]
rate_gm   <- 1 / scale_gm
loglik_gm <- -opt$value
aic_gm    <- -2 * loglik_gm + 2 * 2
bic_gm    <- -2 * loglik_gm + log(nrow(sev)) * 2
cat("\nParámetros Gamma (MLE):\n")
cat("  shape:", round(shape_gm, 6), "\n")
cat("  scale:", format(round(scale_gm), big.mark=","), "ARS\n")
cat("  Media teórica: ARS", format(round(shape_gm * scale_gm), big.mark=","), "\n")
cat("\nAIC Gamma:", round(aic_gm, 2), "\n")
cat("BIC Gamma:", round(bic_gm, 2), "\n")
# =============================================================================
# D. COMPARACIÓN DE MODELOS
# =============================================================================
cat("\n=== D. COMPARACIÓN LOGNORMAL vs GAMMA ===\n")
comp_sev <- data.frame(
  Modelo = c("Lognormal", "Gamma"),
  AIC    = round(c(fit_ln$aic, aic_gm), 2),
  BIC    = round(c(fit_ln$bic, bic_gm), 2),
  LogLik = round(c(fit_ln$loglik, loglik_gm), 2)
)
print(comp_sev)
mejor_sev <- ifelse(aic_gm < fit_ln$aic, "GAMMA", "LOGNORMAL")
cat("\nModelo seleccionado por AIC:", mejor_sev, "\n")
cat("
Δ
AIC:", round(abs(fit_ln$aic - aic_gm), 2), "\n")
# =============================================================================
# E. PRUEBAS DE BONDAD DE AJUSTE
# =============================================================================
cat("\n=== E. BONDAD DE AJUSTE ===\n")
ks_ln <- ks.test(sev$costo_total, "plnorm", mu_ln, sigma_ln)
ks_gm <- ks.test(sev$costo_total, "pgamma", shape_gm, rate_gm)
cat("\n--- Kolmogorov-Smirnov ---\n")
cat("Lognormal: D =", round(ks_ln$statistic, 4), "  p =", round(ks_ln$p.value, 4), "\n")
cat("Gamma:     D =", round(ks_gm$statistic, 4), "  p =", round(ks_gm$p.value, 4), "\n")
ad_ln <- ad.test(sev$costo_total, "plnorm", mu_ln, sigma_ln)
ad_gm <- ad.test(sev$costo_total, "pgamma", shape_gm, rate_gm)
cat("\n--- Anderson-Darling ---\n")
cat("Lognormal: A² =", round(ad_ln$statistic, 4), "  p =", round(ad_ln$p.value, 4), "\n")
cat("Gamma:     A² =", round(ad_gm$statistic, 4), "  p =", round(ad_gm$p.value, 4), "\n")
# =============================================================================
# F. PERCENTILES
# =============================================================================
cat("\n=== F. PERCENTILES ===\n")
probs    <- c(0.50, 0.75, 0.90, 0.95, 0.99)
perc_ln  <- qlnorm(probs, mu_ln, sigma_ln)
perc_gm  <- qgamma(probs, shape_gm, rate_gm)
perc_obs <- quantile(sev$costo_total, probs)
perc_tabla <- data.frame(
  Percentil  = paste0("P", probs*100),
  Observado  = format(round(perc_obs), big.mark=","),
  Lognormal  = format(round(perc_ln),  big.mark=","),
  Gamma      = format(round(perc_gm),  big.mark=",")
)
print(perc_tabla)
# =============================================================================
# G. GRÁFICOS
# =============================================================================
cat("\n=== G. GENERANDO GRÁFICOS ===\n")
x_seq   <- seq(min(sev$costo_total), quantile(sev$costo_total, 0.99), length.out=500)
dens_ln <- dlnorm(x_seq, mu_ln, sigma_ln)
dens_gm <- dgamma(x_seq, shape_gm, rate_gm)
p_dens <- ggplot(sev, aes(x=costo_total)) +
  geom_histogram(aes(y=after_stat(density)), bins=20,
                 fill="#378ADD", alpha=0.5, color="white") +
  geom_line(data=data.frame(x=x_seq, y=dens_ln),

aes(x=x, y=y), color="#e24b4a", linewidth=1.2) +

geom_line(data=data.frame(x=x_seq, y=dens_gm),
            aes(x=x, y=y), color="#1D9E75", linewidth=1.2, linetype="dashed") +
  scale_x_continuous(labels=scales::comma) +

labs(title="Ajuste de distribuciones al costo por accidente",
       subtitle="Rojo: Lognormal | Verde: Gamma",
       x="Costo ARS", y="Densidad") +

theme_minimal(base_size=12)
ggsave("graf_13_ajuste_severidad.png", p_dens, width=8, height=4, dpi=150)
png("graf_14_qq_lognormal.png", width=700, height=500, res=150)
qlnorm_teorico <- qlnorm(ppoints(nrow(sev)), mu_ln, sigma_ln)
plot(sort(qlnorm_teorico), sort(sev$costo_total),
     main="Q-Q plot — Lognormal",

xlab="Cuantiles teóricos", ylab="Cuantiles observados",

col="#378ADD", pch=16, cex=0.7)
abline(0, 1, col="#e24b4a", lwd=2)
dev.off()
png("graf_15_qq_gamma.png", width=700, height=500, res=150)
qgamma_teorico <- qgamma(ppoints(nrow(sev)), shape_gm, rate_gm)
plot(sort(qgamma_teorico), sort(sev$costo_total),
     main="Q-Q plot — Gamma",

xlab="Cuantiles teóricos", ylab="Cuantiles observados",

col="#1D9E75", pch=16, cex=0.7)
abline(0, 1, col="#e24b4a", lwd=2)
dev.off()
p_cdf <- ggplot(sev, aes(x=costo_total)) +
  stat_ecdf(color="#378ADD", linewidth=1.2) +
  stat_function(fun=plnorm, args=list(meanlog=mu_ln, sdlog=sigma_ln),
                color="#e24b4a", linewidth=1) +
  stat_function(fun=pgamma, args=list(shape=shape_gm, rate=rate_gm),
                color="#1D9E75", linewidth=1, linetype="dashed") +
  scale_x_continuous(labels=scales::comma) +

labs(title="CDF empírica vs ajustadas",
       subtitle="Azul: Empírica | Rojo: Lognormal | Verde: Gamma",
       x="Costo ARS", y="F(x)") +

theme_minimal(base_size=12)
ggsave("graf_17_cdf_comparativa.png", p_cdf, width=8, height=4, dpi=150)
cat("Gráficos guardados.\n")
# =============================================================================
# H. RESUMEN EJECUTIVO PASO 3
# =============================================================================
cat("\n=== H. RESUMEN EJECUTIVO PASO 3 ===\n")
cat("
DATOS DE SEVERIDAD:
  - N accidentes con costo > 0:", nrow(sev), "

- Media observada:   ARS", format(round(mean(sev$costo_total)), big.mark=","), "
  - Mediana observada: ARS", format(round(median(sev$costo_total)), big.mark=","), "
  - Skewness:         ", round(moments::skewness(sev$costo_total), 4), "
COMPARACIÓN DE MODELOS:
  - Lognormal AIC:", round(fit_ln$aic, 2), "
  - Gamma AIC:    ", round(aic_gm, 2), "
  -
Δ
AIC:         ", round(abs(fit_ln$aic - aic_gm), 2), "

- Modelo seleccionado:", mejor_sev, "
PARÁMETROS LOGNORMAL:

- mu (meanlog):  ", round(mu_ln, 6), "
  - sigma (sdlog): ", round(sigma_ln, 6), "
PARÁMETROS GAMMA:
  - shape:", round(shape_gm, 6), "
  - scale:", format(round(scale_gm), big.mark=","), "ARS
BONDAD DE AJUSTE:
  - KS  Lognormal: D =", round(ks_ln$statistic, 4), "  p =", round(ks_ln$p.value, 4), "
  - KS  Gamma:     D =", round(ks_gm$statistic, 4), "  p =", round(ks_gm$p.value, 4), "
  - AD  Lognormal: A² =", round(ad_ln$statistic, 4), "  p =", round(ad_ln$p.value, 4), "
  - AD  Gamma:     A² =", round(ad_gm$statistic, 4), "  p =", round(ad_gm$p.value, 4), "
PERCENTILES LOGNORMAL:
  - P50:", format(round(qlnorm(0.50, mu_ln, sigma_ln)), big.mark=","), "ARS
  - P75:", format(round(qlnorm(0.75, mu_ln, sigma_ln)), big.mark=","), "ARS
  - P90:", format(round(qlnorm(0.90, mu_ln, sigma_ln)), big.mark=","), "ARS
  - P95:", format(round(qlnorm(0.95, mu_ln, sigma_ln)), big.mark=","), "ARS
  - P99:", format(round(qlnorm(0.99, mu_ln, sigma_ln)), big.mark=","), "ARS
")
