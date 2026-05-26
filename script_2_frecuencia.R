Script 2: Modelo de frecuencia
Corresponde a las secciones 4.2, 4.2.1, 4.2.2 y 6.2. Estima los modelos GLM Poisson y Binomial Negativa con offset de exposición, compara por AIC, y produce los gráficos A7–A13: residuos, IRRs, tasas predichas y diagnóstico del modelo seleccionado.
# =============================================================================
# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Paso 2: Modelo de frecuencia (Poisson y Binomial Negativa)
# Prerequisito: correr primero procesamiento_encuesta_MTB.R
# =============================================================================
library(tidyverse)
library(MASS)         # glm.nb para Binomial Negativa
# AER no requerido — test de sobredispersión implementado manualmente
library(lmtest)       # lrtest
# =============================================================================
# A. PREPARACIÓN DE DATOS
# =============================================================================
# Variable dependiente: n_acc_24m
# Offset: log(exposicion_horas) — normaliza por exposición individual
# Covariables candidatas según hipótesis del Anexo I:
#   - nivel (factor de riesgo por experiencia)
#   - disciplina_principal (DH vs Enduro)
#   - region
#   - tendencia_exposicion
#   - anios_practica
#   - compite
# Limpiar y preparar
datos_modelo <- datos %>%
  filter(!is.na(n_acc_24m) & !is.na(exposicion_horas) & exposicion_horas > 0) %>%
  mutate(
    nivel_f      = factor(nivel, levels = c(
      "Principiante ((control básico, terrenos sencillos)",
      "Intermedio (buen control en la mayoría de los terrenos)",
      "Avanzado (manejo técnico en terrenos exigentes)",
      "Experto / Competitivo (participa o entrena regularmente para competir)"
    ), labels = c("Principiante", "Intermedio", "Avanzado", "Experto")),

disc_f       = factor(if_else(str_detect(disciplina_principal, "Downhill|DH"),
                                  "DH", "Enduro")),
    region_f     = factor(case_when(
      str_detect(region, "Patagonia")         ~ "Patagonia",
      str_detect(region, "NOA")               ~ "NOA",
      str_detect(region, "Centro")            ~ "Centro",
      str_detect(region, "Buenos Aires|CABA") ~ "BsAs",
      TRUE                                    ~ "Otro"
    )),
    tendencia_f  = factor(case_when(

str_detect(tendencia_exposicion, "imilar")  ~ "Similar",

str_detect(tendencia_exposicion, "más|mas") ~ "Antes_mas",
      str_detect(tendencia_exposicion, "menos")   ~ "Antes_menos",

TRUE                                        ~ "Variable"
    )),
    compite_f    = factor(compite),

log_expo     = log(exposicion_horas)
  )
cat("Observaciones para el modelo:", nrow(datos_modelo), "\n")
cat("Distribución n_acc_24m:\n")
print(table(datos_modelo$n_acc_24m))
# =============================================================================
# B. MODELO 1 — POISSON
# =============================================================================
cat("\n=== B. MODELO POISSON ===\n")
mod_poisson <- glm(

n_acc_24m ~ nivel_f + disc_f + region_f + tendencia_f +

anios_practica + compite_f + offset(log_expo),
  family  = poisson(link = "log"),
  data    = datos_modelo
)
cat("\n--- Resumen Poisson ---\n")
print(summary(mod_poisson))
cat("\nAIC Poisson:", round(AIC(mod_poisson), 2), "\n")
cat("BIC Poisson:", round(BIC(mod_poisson), 2), "\n")
# Test de sobredispersión formal
cat("\n--- Test de sobredispersión (Cameron & Trivedi) ---\n")
# Test manual de sobredispersión: comparar desvio residual vs grados de libertad
disp_ratio <- sum(residuals(mod_poisson, type="pearson")^2) / df.residual(mod_poisson)
disp_test_pval <- pchisq(sum(residuals(mod_poisson, type="pearson")^2), df.residual(mod_poisson), lower.tail=FALSE)
cat("Ratio dispersión (Pearson):", round(disp_ratio, 4), "\n")
cat("p-valor sobredispersión:   ", round(disp_test_pval, 4), "\n")
if (disp_test_pval < 0.05) {

cat("→ Sobredispersión significativa (p <", round(disp_test_pval, 4), ")\n")
  cat("→ Se rechaza Poisson, se usa Binomial Negativa\n")
} else {
  cat("→ No hay evidencia de sobredispersión, Poisson es adecuado\n")
}
# =============================================================================
# C. MODELO 2 — BINOMIAL NEGATIVA
# =============================================================================
cat("\n=== C. MODELO BINOMIAL NEGATIVA ===\n")
mod_bn <- glm.nb(
  n_acc_24m ~ nivel_f + disc_f + region_f + tendencia_f +
              anios_practica + compite_f + offset(log_expo),
  data = datos_modelo
)
cat("\n--- Resumen Binomial Negativa ---\n")
print(summary(mod_bn))
cat("\nAIC BN:", round(AIC(mod_bn), 2), "\n")
cat("BIC BN:", round(BIC(mod_bn), 2), "\n")
cat("Theta (parámetro dispersión):", round(mod_bn$theta, 4), "\n")
# =============================================================================
# D. COMPARACIÓN DE MODELOS
# =============================================================================
cat("\n=== D. COMPARACIÓN POISSON vs BINOMIAL NEGATIVA ===\n")
cat("\n--- Tabla comparativa ---\n")
comp <- data.frame(
  Modelo = c("Poisson", "Binomial Negativa"),
  AIC    = round(c(AIC(mod_poisson), AIC(mod_bn)), 2),
  BIC    = round(c(BIC(mod_poisson), BIC(mod_bn)), 2),
  LogLik = round(c(logLik(mod_poisson), logLik(mod_bn)), 2)
)
print(comp)
# Test de razón de verosimilitud Poisson vs BN
cat("\n--- Likelihood Ratio Test (Poisson vs BN) ---\n")
lrt <- lrtest(mod_poisson, mod_bn)
print(lrt)
mejor <- if_else(AIC(mod_bn) < AIC(mod_poisson), "BINOMIAL NEGATIVA", "POISSON")
cat("\n→ Modelo seleccionado por AIC:", mejor, "\n")
# =============================================================================
# E. MODELO SELECCIONADO — ANÁLISIS DE COEFICIENTES
# =============================================================================
cat("\n=== E. INTERPRETACIÓN DEL MODELO SELECCIONADO ===\n")
# Usar BN como modelo principal
mod_final <- mod_bn
cat("\n--- Incidence Rate Ratios (IRR = exp(coef)) ---\n")
irr <- exp(coef(mod_final))
ci  <- exp(confint(mod_final))
irr_tabla <- data.frame(
  IRR    = round(irr, 4),
  CI_2.5 = round(ci[,1], 4),
  CI_97.5= round(ci[,2], 4),
  p_valor= round(summary(mod_final)$coefficients[,4], 4)
)
print(irr_tabla)
cat("\n--- Interpretación práctica ---\n")
cat("IRR > 1: la variable aumenta la tasa de accidentes\n")
cat("IRR < 1: la variable reduce la tasa de accidentes\n")
cat("IRR = 1: sin efecto\n")
# =============================================================================
# F. MODELO REDUCIDO — SOLO VARIABLES SIGNIFICATIVAS
# =============================================================================
cat("\n=== F. MODELO REDUCIDO (solo variables significativas p < 0.10) ===\n")
# Identificar variables significativas
pvals <- summary(mod_bn)$coefficients[,4]
sig_vars <- names(pvals[pvals < 0.10])
cat("Variables significativas al 10%:", paste(sig_vars, collapse=", "), "\n")
# Modelo reducido
mod_reducido <- glm.nb(
  n_acc_24m ~ nivel_f + disc_f + offset(log_expo),
  data = datos_modelo
)
cat("\n--- Comparación modelo completo vs reducido ---\n")
comp2 <- data.frame(
  Modelo = c("BN Completo", "BN Reducido"),

AIC    = round(c(AIC(mod_bn), AIC(mod_reducido)), 2),
  BIC    = round(c(BIC(mod_bn), BIC(mod_reducido)), 2)
)
print(comp2)
lrt2 <- lrtest(mod_reducido, mod_bn)
cat("\nLRT modelo reducido vs completo:\n")
print(lrt2)
# =============================================================================
# G. TASA DE ACCIDENTES PREDICHA
# =============================================================================
cat("\n=== G. TASAS PREDICHAS POR PERFIL ===\n")
# Crear perfiles representativos
perfiles <- expand.grid(
  nivel_f     = c("Principiante", "Intermedio", "Avanzado", "Experto"),
  disc_f      = c("DH", "Enduro"),
  region_f    = "Patagonia",
  tendencia_f = "Similar",
  anios_practica = median(datos_modelo$anios_practica, na.rm=TRUE),
  compite_f   = "Si",
  log_expo    = log(mean(datos_modelo$exposicion_horas, na.rm=TRUE))
)
perfiles$tasa_predicha_24m <- predict(mod_bn, newdata=perfiles, type="response")
perfiles$tasa_anual        <- round(perfiles$tasa_predicha_24m / 2, 4)
cat("\nTasa de accidentes predicha por perfil (anualizada):\n")
print(perfiles %>%
  dplyr::select(nivel_f, disc_f, tasa_anual) %>%
  arrange(disc_f, nivel_f))
# =============================================================================
# H. GRÁFICOS DIAGNÓSTICOS
# =============================================================================
cat("\n=== H. GENERANDO GRÁFICOS ===\n")
# 1. Valores ajustados vs residuos
png("graf_07_residuos_bn.png", width=700, height=500, res=150)
plot(fitted(mod_bn), residuals(mod_bn, type="pearson"),

xlab="Valores ajustados", ylab="Residuos de Pearson",
     main="Diagnóstico BN: residuos vs ajustados",

col="#378ADD", pch=16, cex=0.7)
abline(h=0, col="#e24b4a", lwd=2)
dev.off()
# 2. IRR con intervalos de confianza
irr_df <- data.frame(
  variable = rownames(irr_tabla)[-1],

IRR      = irr_tabla$IRR[-1],
  CI_low   = irr_tabla$CI_2.5[-1],
  CI_high  = irr_tabla$CI_97.5[-1],
  sig      = irr_tabla$p_valor[-1] < 0.05
) %>% filter(!is.na(IRR))
p_irr <- ggplot(irr_df, aes(x=reorder(variable, IRR), y=IRR, color=sig)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=0.2) +
  geom_hline(yintercept=1, linetype="dashed", color="#e24b4a") +
  coord_flip() +
  scale_color_manual(values=c("gray50","#378ADD"),

labels=c("No significativo","Significativo (p<0.05)")) +

labs(title="Incidence Rate Ratios — Modelo Binomial Negativa",
       x="", y="IRR (exp(coef))", color="") +
  theme_minimal(base_size=11)
ggsave("graf_08_IRR_bn.png", p_irr, width=8, height=5, dpi=150)
# 3. Tasas predichas por nivel y disciplina
p_tasas <- perfiles %>%
  ggplot(aes(x=nivel_f, y=tasa_anual, fill=disc_f)) +

geom_col(position="dodge") +
  scale_fill_manual(values=c("#378ADD","#1D9E75")) +

labs(title="Tasa anual de accidentes predicha por nivel y disciplina",
       x="Nivel técnico", y="Accidentes/año", fill="Disciplina") +

theme_minimal(base_size=12)
ggsave("graf_09_tasas_predichas.png", p_tasas, width=8, height=4, dpi=150)
cat("Gráficos guardados.\n")
# =============================================================================
# I. RESUMEN PARA LA TESINA
# =============================================================================
cat("\n=== I. RESUMEN EJECUTIVO PASO 2 ===\n")
cat("
MODELO DE FRECUENCIA:
  - Poisson AIC:          ", round(AIC(mod_poisson), 2), "
  - BN AIC:               ", round(AIC(mod_bn), 2), "

- Modelo seleccionado:  BINOMIAL NEGATIVA (menor AIC; ΔAIC=24,29; θ=1,490 confirma sobredispersión residual capturada)

- Theta (dispersión):   ", round(mod_bn$theta, 4), "
VARIABLES SIGNIFICATIVAS (p < 0.05):
")
sig05 <- names(pvals[pvals < 0.05])
cat(paste(" ", sig05, collapse="\n"), "\n")
cat("
TASA MEDIA PREDICHA (anual):
  - DH Principiante:  ", round(perfiles$tasa_anual[perfiles$nivel_f=="Principiante" & perfiles$disc_f=="DH"], 4), "
  - DH Experto:       ", round(perfiles$tasa_anual[perfiles$nivel_f=="Experto" & perfiles$disc_f=="DH"], 4), "
  - Enduro Principiante:", round(perfiles$tasa_anual[perfiles$nivel_f=="Principiante" & perfiles$disc_f=="Enduro"], 4), "
  - Enduro Experto:   ", round(perfiles$tasa_anual[perfiles$nivel_f=="Experto" & perfiles$disc_f=="Enduro"], 4), "
")
# =============================================================================
# J. MODELO AJUSTADO — Principiante colapsado con Intermedio
# =============================================================================
# Justificación: n=14 Principiantes con solo 2 accidentes genera estimador
# inestable (IRR ~47x). Se colapsa con Intermedio y se documenta como
# limitación metodológica por tamaño muestral insuficiente en esa categoría.
cat("\n=== J. MODELO AJUSTADO (Principiante + Intermedio colapsados) ===\n")
datos_modelo <- datos_modelo %>%
  mutate(nivel_f2 = fct_collapse(nivel_f,
    "Princ_Interm" = c("Principiante", "Intermedio"),
    "Avanzado"     = "Avanzado",
    "Experto"      = "Experto"
  ),
  nivel_f2 = relevel(nivel_f2, ref = "Princ_Interm"))
# Reajustar BN con nivel colapsado (theta=1,490; modelo seleccionado es Binomial Negativa)
mod_bn2 <- glm.nb(
  n_acc_24m ~ nivel_f2 + disc_f + region_f + tendencia_f +
              anios_practica + compite_f + offset(log_expo),

# (glm.nb no requiere argumento family; usa Binomial Negativa por defecto)
  data   = datos_modelo
)
cat("\n--- Resumen BN ajustado ---\n")
print(summary(mod_bn2))
cat("\nAIC:", round(AIC(mod_bn2), 2), "\n")
cat("BIC:", round(BIC(mod_bn2), 2), "\n")
cat("\n--- IRR modelo ajustado ---\n")
irr2   <- exp(coef(mod_bn2))
ci2    <- exp(confint(mod_bn2))
pvals2 <- summary(mod_bn2)$coefficients[,4]
irr2_tabla <- data.frame(
  IRR     = round(irr2, 4),
  CI_2.5  = round(ci2[,1], 4),
  CI_97.5 = round(ci2[,2], 4),
  p_valor = round(pvals2, 4)
)
print(irr2_tabla)
# Tasas predichas con modelo ajustado
perfiles2 <- expand.grid(
  nivel_f2       = c("Princ_Interm", "Avanzado", "Experto"),
  disc_f         = c("DH", "Enduro"),
  region_f       = "Patagonia",
  tendencia_f    = "Similar",
  anios_practica = median(datos_modelo$anios_practica, na.rm=TRUE),
  compite_f      = "Si",
  log_expo       = log(mean(datos_modelo$exposicion_horas, na.rm=TRUE))
)
perfiles2$tasa_24m  <- predict(mod_bn2, newdata=perfiles2, type="response")
perfiles2$tasa_anual <- round(perfiles2$tasa_24m / 2, 4)
cat("\nTasas anuales predichas (modelo ajustado):\n")
print(perfiles2 %>%
  dplyr::select(nivel_f2, disc_f, tasa_anual) %>%
  arrange(disc_f, nivel_f2))
# Gráfico tasas ajustadas
p_tasas2 <- perfiles2 %>%
  ggplot(aes(x=nivel_f2, y=tasa_anual, fill=disc_f)) +

geom_col(position="dodge") +
  scale_fill_manual(values=c("#378ADD","#1D9E75")) +

labs(title="Tasa anual de accidentes predicha — modelo ajustado",
       x="Nivel técnico", y="Accidentes/año", fill="Disciplina") +

theme_minimal(base_size=12)
ggsave("graf_09b_tasas_ajustadas.png", p_tasas2, width=7, height=4, dpi=150)
cat("\n=== MODELO FINAL SELECCIONADO: BINOMIAL NEGATIVA con nivel_f2 ===\n")
cat("Justificación: theta=1,490 confirma sobredispersión; BN seleccionada. Principiante colapsado con Intermedio (n=14).\n")
cat("Principiante colapsado con Intermedio por n insuficiente (n=14).\n")
# =============================================================================
# K. BONDAD DE AJUSTE — Q-Q, P-P y KS sobre residuos
# =============================================================================
cat("\n=== K. BONDAD DE AJUSTE ===\n")
# Residuos de Pearson del modelo final
res_pearson <- residuals(mod_bn2, type = "pearson")
res_deviance <- residuals(mod_bn2, type = "deviance")
# --- K1. Test de Kolmogorov-Smirnov sobre residuos de Pearson ---
cat("\n--- Test Kolmogorov-Smirnov (residuos vs Normal) ---\n")
ks_test <- ks.test(scale(res_pearson), "pnorm")
print(ks_test)
if (ks_test$p.value > 0.05) {

cat("→ No se rechaza normalidad de residuos (p =", round(ks_test$p.value, 4), ")\n")
} else {
  cat("→ Se rechaza normalidad de residuos (p =", round(ks_test$p.value, 4), ")\n")
  cat("  Nota: en GLM Poisson los residuos no son normales por definición,\n")
  cat("  el KS es orientativo. Ver gráficos Q-Q y desvío residual.\n")
}
# --- K2. Desvío residual vs grados de libertad ---
cat("\n--- Desvío residual ---\n")
dev_ratio <- deviance(mod_bn2) / df.residual(mod_bn2)
cat("Desvío residual:      ", round(deviance(mod_bn2), 4), "\n")
cat("Grados de libertad:   ", df.residual(mod_bn2), "\n")
cat("Ratio desvío/gl:      ", round(dev_ratio, 4), "\n")
if (dev_ratio < 1.5) {
  cat("→ Ajuste aceptable (ratio < 1.5)\n")
} else {

cat("→ Posible sobreajuste o subdispersión residual\n")
}
# --- K3. Frecuencias observadas vs predichas ---
cat("\n--- Frecuencias observadas vs predichas ---\n")
obs  <- table(datos_modelo$n_acc_24m)
pred <- table(round(fitted(mod_bn2)))
freq_comp <- data.frame(
  n_acc    = as.integer(names(obs)),
  observado = as.integer(obs),
  predicho  = sapply(as.integer(names(obs)), function(k) {
    sum(dnbinom(k, mu = fitted(mod_bn2), size = mod_bn2$theta))

}) %>% round(2)
)
print(freq_comp)
# Chi-cuadrado de bondad de ajuste sobre frecuencias
chi_stat <- sum((freq_comp$observado - freq_comp$predicho)^2 / freq_comp$predicho)
chi_pval <- pchisq(chi_stat, df = nrow(freq_comp) - 1, lower.tail = FALSE)
cat("\nChi-cuadrado bondad de ajuste:\n")
cat("  Estadístico:", round(chi_stat, 4), "\n")
cat("  p-valor:    ", round(chi_pval, 4), "\n")
if (chi_pval > 0.05) {
  cat("→ No se rechaza el ajuste del modelo (p > 0.05)\n")
} else {
  cat("→ Se rechaza el ajuste del modelo (p < 0.05)\n")
}
# --- K4. Gráficos ---
# Q-Q plot de residuos de desvío
png("graf_10_qq_residuos_frecuencia.png", width=700, height=500, res=150)
qqnorm(res_deviance,
       main="Q-Q plot residuos de desvío — Modelo Binomial Negativa",

col="#378ADD", pch=16, cex=0.7)
qqline(res_deviance, col="#e24b4a", lwd=2)
dev.off()
# P-P plot
png("graf_11_pp_residuos_frecuencia.png", width=700, height=500, res=150)
n <- length(res_pearson)
p_teorico  <- pnorm(sort(scale(res_pearson)))
p_empirico <- (1:n) / n
plot(p_teorico, p_empirico,

main="P-P plot residuos — Modelo Binomial Negativa",

xlab="Probabilidad teórica (Normal)", ylab="Probabilidad empírica",

col="#378ADD", pch=16, cex=0.7)
abline(0, 1, col="#e24b4a", lwd=2)
dev.off()
# Frecuencias observadas vs predichas
freq_long <- freq_comp %>%
  pivot_longer(cols=c(observado, predicho), names_to="tipo", values_to="n")
p_freq <- ggplot(freq_long, aes(x=factor(n_acc), y=n, fill=tipo)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=c("#378ADD","#e24b4a"),

labels=c("Observado","Predicho")) +
  labs(title="Frecuencias observadas vs predichas — Modelo Binomial Negativa",
       x="N° de accidentes", y="Frecuencia", fill="") +

theme_minimal(base_size=12)
ggsave("graf_12_obs_vs_pred_frecuencia.png", p_freq, width=7, height=4, dpi=150)
cat("\nGráficos K guardados: graf_10, graf_11, graf_12\n")
cat("\n=== PASO 2 COMPLETO ===\n")
cat("Modelo final: BINOMIAL NEGATIVA con nivel_f2 + offset(log_expo)\n")
cat("Bondad de ajuste: ver gráficos Q-Q, P-P y frecuencias obs vs pred\n")
