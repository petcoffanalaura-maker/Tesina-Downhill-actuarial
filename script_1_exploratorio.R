Script 1: Análisis exploratorio
Corresponde a la sección 6.1 (Análisis exploratorio) y 6.1.1 (Validación del ISC). Produce los gráficos A1–A6 del Anexo I: distribución de accidentes por persona, distribución de costos, exposición vs siniestros, distribución del ISC, Q-Q plot de log(Costo) y validación ISC vs costo.
# =============================================================================
# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Paso 1: Análisis exploratorio
# Prerequisito: correr primero procesamiento_encuesta_MTB.R
# =============================================================================
library(tidyverse)
library(moments)    # skewness, kurtosis
library(MASS)       # fitdistr
# =============================================================================
# A. ESTADÍSTICAS DESCRIPTIVAS DE LA MUESTRA
# =============================================================================
cat("=== A. PERFIL DE LA MUESTRA ===\n")
cat("Respondentes:", nrow(datos), "\n")
cat("Con lesiones:", sum(datos$tuvo_lesiones == "Si", na.rm=TRUE), "\n")
cat("Tasa de siniestralidad bruta:", round(mean(datos$tuvo_lesiones == "Si", na.rm=TRUE)*100, 1), "%\n\n")
cat("--- Disciplina principal ---\n")
print(table(datos$disciplina_principal))
cat("\n--- Nivel técnico ---\n")
print(table(datos$nivel))
cat("\n--- Región ---\n")
print(table(datos$region))
cat("\n--- Género ---\n")
print(table(datos$genero))
cat("\n--- Edad ---\n")
print(summary(datos$edad))
cat("\n--- Años de práctica ---\n")
print(summary(datos$anios_practica))
cat("\n--- Exposición 24m (horas) ---\n")
print(summary(datos$exposicion_horas))
# =============================================================================
# B. ANÁLISIS DE FRECUENCIA
# =============================================================================
cat("\n=== B. ANÁLISIS DE FRECUENCIA ===\n")
cat("\n--- Distribución de n_acc_24m ---\n")
print(table(datos$n_acc_24m, useNA = "ifany"))
cat("\n--- Estadísticas de n_acc_24m ---\n")
cat("Media:    ", round(mean(datos$n_acc_24m, na.rm=TRUE), 4), "\n")
cat("Varianza: ", round(var(datos$n_acc_24m, na.rm=TRUE), 4), "\n")
cat("Desvío:   ", round(sd(datos$n_acc_24m, na.rm=TRUE), 4), "\n")
cat("Max:      ", max(datos$n_acc_24m, na.rm=TRUE), "\n")
# Test de sobredispersión: varianza/media
media_n  <- mean(datos$n_acc_24m, na.rm=TRUE)
var_n    <- var(datos$n_acc_24m, na.rm=TRUE)
indice_d <- var_n / media_n
cat("\n--- Test de sobredispersión ---\n")
cat("Índice de dispersión (Var/Media):", round(indice_d, 4), "\n")
if (indice_d > 1.2) {
  cat("→ Sobredispersión presente: usar Binomial Negativa\n")
} else if (indice_d < 0.8) {
  cat("→ Subdispersión presente: evaluar Binomial\n")
} else {
  cat("→ Equidispersión aproximada: Poisson es adecuado\n")
}
# Tasa de accidentes por hora de exposición (normalizada a 1000 horas)
datos_con <- datos %>% filter(tuvo_lesiones == "Si")
tasa_por_1000h <- (mean(datos$n_acc_24m, na.rm=TRUE) / mean(datos$exposicion_horas, na.rm=TRUE)) * 1000
cat("\nTasa de accidentes cada 1000 horas de exposición:", round(tasa_por_1000h, 4), "\n")
# Tasa anualizada (dividir por 2 porque la ventana es 24 meses)
cat("Tasa anualizada (accidentes/persona/año):", round(media_n/2, 4), "\n")
# =============================================================================
# C. ANÁLISIS DE SEVERIDAD (ISC y Costo)
# =============================================================================
cat("\n=== C. ANÁLISIS DE SEVERIDAD ===\n")
# Filtrar solo accidentes reales
acc_sev <- acc_largo %>% filter(!is.na(ISC) & ISC > 0)
cat("Accidentes con ISC > 0:", nrow(acc_sev), "\n")
cat("\n--- ISC (Índice de Severidad Clínica) ---\n")
print(summary(acc_sev$ISC))
cat("Skewness: ", round(skewness(acc_sev$ISC), 4), "\n")
cat("Kurtosis: ", round(kurtosis(acc_sev$ISC), 4), "\n")
# Costo solo para accidentes con costo > 0
acc_costo <- acc_largo %>% filter(!is.na(costo_total) & costo_total > 0)
cat("\nAccidentes con costo > 0:", nrow(acc_costo), "\n")
cat("\n--- Costo por accidente (ARS) ---\n")
print(summary(acc_costo$costo_total))
cat("Skewness: ", round(skewness(acc_costo$costo_total), 4), "\n")
cat("Kurtosis: ", round(kurtosis(acc_costo$costo_total), 4), "\n")
# Percentiles clave
cat("\n--- Percentiles del costo ---\n")
perc <- quantile(acc_costo$costo_total, probs = c(0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
print(round(perc))
# Proporción de costo > distintos umbrales
cat("\n--- Concentración de la cola ---\n")
cat("% accidentes con costo > $500k:  ", round(mean(acc_costo$costo_total > 500000)*100, 1), "%\n")
cat("% accidentes con costo > $1M:    ", round(mean(acc_costo$costo_total > 1000000)*100, 1), "%\n")
cat("% accidentes con costo > $3M:    ", round(mean(acc_costo$costo_total > 3000000)*100, 1), "%\n")
cat("% costo acumulado top 10%:       ", round(sum(sort(acc_costo$costo_total, decreasing=TRUE)[1:ceiling(nrow(acc_costo)*0.1)]) / sum(acc_costo$costo_total)*100, 1), "%\n")
# =============================================================================
# D. ANÁLISIS POR SUBGRUPOS (para hipótesis del modelo)
# =============================================================================
cat("\n=== D. FRECUENCIA POR SUBGRUPOS ===\n")
cat("\n--- Tasa de accidentes por nivel técnico ---\n")
datos %>%
  group_by(nivel) %>%
  summarise(
    n          = n(),
    acc_total  = sum(n_acc_24m, na.rm=TRUE),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    expo_media = round(mean(exposicion_horas, na.rm=TRUE), 1),
    tasa_1000h = round(acc_total / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)

) %>%
  print()
cat("\n--- Tasa de accidentes por disciplina principal ---\n")
datos %>%
  filter(str_detect(disciplina_principal, "Downhill|DH|Enduro")) %>%

group_by(disciplina_principal) %>%
  summarise(
    n          = n(),
    acc_total  = sum(n_acc_24m, na.rm=TRUE),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    tasa_1000h = round(acc_total / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)

) %>%
  print()
cat("\n--- Tasa de accidentes por región ---\n")
datos %>%
  group_by(region) %>%
  summarise(
    n          = n(),
    acc_total  = sum(n_acc_24m, na.rm=TRUE),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    tasa_1000h = round(acc_total / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)
  ) %>%
  arrange(desc(tasa_1000h)) %>%

print()
cat("\n--- Tasa de accidentes: compite vs no compite ---\n")
datos %>%
  group_by(compite) %>%
  summarise(
    n          = n(),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    tasa_1000h = round(sum(n_acc_24m, na.rm=TRUE) / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)

) %>%
  print()
# =============================================================================
# E. CORRELACIONES ENTRE VARIABLES
# =============================================================================
cat("\n=== E. CORRELACIONES CON n_acc_24m ===\n")
vars_num <- datos %>%
  dplyr::select(n_acc_24m, edad, anios_practica, exposicion_horas,
         meses_anio, dias_semana, horas_dia) %>%

drop_na()
cor_mat <- cor(vars_num, method = "spearman")
cat("\n--- Correlación de Spearman con n_acc_24m ---\n")
print(round(cor_mat["n_acc_24m", ], 3))
# =============================================================================
# F. GRÁFICOS (guardar como PNG)
# =============================================================================
cat("\n=== F. GENERANDO GRÁFICOS ===\n")
# 1. Histograma de n_acc_24m
p1 <- ggplot(datos, aes(x = n_acc_24m)) +

geom_bar(fill = "#378ADD", color = "white", width = 0.6) +

labs(title = "Distribución de accidentes por persona (24 meses)",
       x = "N° de accidentes", y = "Frecuencia") +

theme_minimal(base_size = 13)
ggsave("graf_01_freq_nacc.png", p1, width = 7, height = 4, dpi = 150)
# 2. Histograma de costo con log-escala
p2 <- ggplot(acc_costo, aes(x = costo_total)) +

geom_histogram(fill = "#e24b4a", color = "white", bins = 25) +
  scale_x_log10(labels = scales::comma) +

labs(title = "Distribución del costo por accidente (escala log)",
       x = "Costo ARS (log)", y = "Frecuencia") +

theme_minimal(base_size = 13)
ggsave("graf_02_sev_costo_log.png", p2, width = 7, height = 4, dpi = 150)
# 3. Costo por nivel técnico
p3 <- datos %>%
  filter(costo_total_24m > 0) %>%
  ggplot(aes(x = reorder(nivel, costo_total_24m, median), y = costo_total_24m)) +

geom_boxplot(fill = "#534AB7", color = "white", outlier.color = "#e24b4a") +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +

labs(title = "Costo total 24m por nivel técnico",
       x = "", y = "Costo ARS (log)") +

theme_minimal(base_size = 11)
ggsave("graf_03_costo_por_nivel.png", p3, width = 8, height = 4, dpi = 150)
# 4. Exposición vs n_acc
p4 <- ggplot(datos, aes(x = exposicion_horas, y = n_acc_24m)) +

geom_point(alpha = 0.4, color = "#1D9E75", size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),

se = TRUE, color = "#e24b4a") +
  labs(title = "Exposición vs N° de accidentes",
       x = "Horas de exposición (24m)", y = "N° de accidentes") +

theme_minimal(base_size = 13)
ggsave("graf_04_expo_vs_nacc.png", p4, width = 7, height = 4, dpi = 150)
# 5. ISC distribución
p5 <- ggplot(acc_sev, aes(x = ISC)) +
  geom_histogram(fill = "#EF9F27", color = "white", bins = 15) +

labs(title = "Distribución del ISC (accidentes graves)",
       x = "Índice de Severidad Clínica", y = "Frecuencia") +

theme_minimal(base_size = 13)
ggsave("graf_05_ISC.png", p5, width = 7, height = 4, dpi = 150)
# 6. Q-Q plot del log(costo) — verificar normalidad en log-escala (Lognormal)
log_costo <- log(acc_costo$costo_total)
png("graf_06_qq_logcosto.png", width=700, height=500, res=150)
qqnorm(log_costo, main="Q-Q plot de log(Costo) — verificación Lognormal",
       col="#378ADD", pch=16, cex=0.7)
qqline(log_costo, col="#e24b4a", lwd=2)
dev.off()
cat("Gráficos guardados en el directorio de trabajo.\n")
# =============================================================================
# G. RESUMEN PARA LA TESINA
# =============================================================================
cat("\n=== G. RESUMEN EJECUTIVO PARA LA TESINA ===\n")
cat("
FRECUENCIA:
  - Media n_acc_24m:      ", round(media_n, 4), "
  - Varianza n_acc_24m:   ", round(var_n, 4), "
  - Índice dispersión:    ", round(indice_d, 4), "
  - Tasa c/1000h expo:    ", round(tasa_por_1000h, 4), "
SEVERIDAD (accidentes con costo > 0):
  - N accidentes:         ", nrow(acc_costo), "
  - Media costo:          ARS", format(round(mean(acc_costo$costo_total)), big.mark="."), "
  - Mediana costo:        ARS", format(round(median(acc_costo$costo_total)), big.mark="."), "

- P95 costo:            ARS", format(round(quantile(acc_costo$costo_total, 0.95)), big.mark="."), "
  - Skewness costo:       ", round(skewness(acc_costo$costo_total), 4), "
CONCLUSIÓN EXPLORATORIA:
  - Frecuencia: ", ifelse(indice_d > 1.2, "BINOMIAL NEGATIVA (sobredispersión)", "POISSON (equidispersión)"), "
  - Severidad:  GAMMA seleccionada (mejor AIC y Anderson-Darling; Lognormal como alternativa en sensibilidad)
")# =============================================================================
# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Paso 1: Análisis exploratorio
# Prerequisito: correr primero procesamiento_encuesta_MTB.R
# =============================================================================
library(tidyverse)
library(moments)    # skewness, kurtosis
library(MASS)       # fitdistr
# =============================================================================
# A. ESTADÍSTICAS DESCRIPTIVAS DE LA MUESTRA
# =============================================================================
cat("=== A. PERFIL DE LA MUESTRA ===\n")
cat("Respondentes:", nrow(datos), "\n")
cat("Con lesiones:", sum(datos$tuvo_lesiones == "Si", na.rm=TRUE), "\n")
cat("Tasa de siniestralidad bruta:", round(mean(datos$tuvo_lesiones == "Si", na.rm=TRUE)*100, 1), "%\n\n")
cat("--- Disciplina principal ---\n")
print(table(datos$disciplina_principal))
cat("\n--- Nivel técnico ---\n")
print(table(datos$nivel))
cat("\n--- Región ---\n")
print(table(datos$region))
cat("\n--- Género ---\n")
print(table(datos$genero))
cat("\n--- Edad ---\n")
print(summary(datos$edad))
cat("\n--- Años de práctica ---\n")
print(summary(datos$anios_practica))
cat("\n--- Exposición 24m (horas) ---\n")
print(summary(datos$exposicion_horas))
# =============================================================================
# B. ANÁLISIS DE FRECUENCIA
# =============================================================================
cat("\n=== B. ANÁLISIS DE FRECUENCIA ===\n")
cat("\n--- Distribución de n_acc_24m ---\n")
print(table(datos$n_acc_24m, useNA = "ifany"))
cat("\n--- Estadísticas de n_acc_24m ---\n")
cat("Media:    ", round(mean(datos$n_acc_24m, na.rm=TRUE), 4), "\n")
cat("Varianza: ", round(var(datos$n_acc_24m, na.rm=TRUE), 4), "\n")
cat("Desvío:   ", round(sd(datos$n_acc_24m, na.rm=TRUE), 4), "\n")
cat("Max:      ", max(datos$n_acc_24m, na.rm=TRUE), "\n")
# Test de sobredispersión: varianza/media
media_n  <- mean(datos$n_acc_24m, na.rm=TRUE)
var_n    <- var(datos$n_acc_24m, na.rm=TRUE)
indice_d <- var_n / media_n
cat("\n--- Test de sobredispersión ---\n")
cat("Índice de dispersión (Var/Media):", round(indice_d, 4), "\n")
if (indice_d > 1.2) {
  cat("→ Sobredispersión presente: usar Binomial Negativa\n")
} else if (indice_d < 0.8) {
  cat("→ Subdispersión presente: evaluar Binomial\n")
} else {
  cat("→ Equidispersión aproximada: Poisson es adecuado\n")
}
# Tasa de accidentes por hora de exposición (normalizada a 1000 horas)
datos_con <- datos %>% filter(tuvo_lesiones == "Si")
tasa_por_1000h <- (mean(datos$n_acc_24m, na.rm=TRUE) / mean(datos$exposicion_horas, na.rm=TRUE)) * 1000
cat("\nTasa de accidentes cada 1000 horas de exposición:", round(tasa_por_1000h, 4), "\n")
# Tasa anualizada (dividir por 2 porque la ventana es 24 meses)
cat("Tasa anualizada (accidentes/persona/año):", round(media_n/2, 4), "\n")
# =============================================================================
# C. ANÁLISIS DE SEVERIDAD (ISC y Costo)
# =============================================================================
cat("\n=== C. ANÁLISIS DE SEVERIDAD ===\n")
# Filtrar solo accidentes reales
acc_sev <- acc_largo %>% filter(!is.na(ISC) & ISC > 0)
cat("Accidentes con ISC > 0:", nrow(acc_sev), "\n")
cat("\n--- ISC (Índice de Severidad Clínica) ---\n")
print(summary(acc_sev$ISC))
cat("Skewness: ", round(skewness(acc_sev$ISC), 4), "\n")
cat("Kurtosis: ", round(kurtosis(acc_sev$ISC), 4), "\n")
# Costo solo para accidentes con costo > 0
acc_costo <- acc_largo %>% filter(!is.na(costo_total) & costo_total > 0)
cat("\nAccidentes con costo > 0:", nrow(acc_costo), "\n")
cat("\n--- Costo por accidente (ARS) ---\n")
print(summary(acc_costo$costo_total))
cat("Skewness: ", round(skewness(acc_costo$costo_total), 4), "\n")
cat("Kurtosis: ", round(kurtosis(acc_costo$costo_total), 4), "\n")
# Percentiles clave
cat("\n--- Percentiles del costo ---\n")
perc <- quantile(acc_costo$costo_total, probs = c(0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
print(round(perc))
# Proporción de costo > distintos umbrales
cat("\n--- Concentración de la cola ---\n")
cat("% accidentes con costo > $500k:  ", round(mean(acc_costo$costo_total > 500000)*100, 1), "%\n")
cat("% accidentes con costo > $1M:    ", round(mean(acc_costo$costo_total > 1000000)*100, 1), "%\n")
cat("% accidentes con costo > $3M:    ", round(mean(acc_costo$costo_total > 3000000)*100, 1), "%\n")
cat("% costo acumulado top 10%:       ", round(sum(sort(acc_costo$costo_total, decreasing=TRUE)[1:ceiling(nrow(acc_costo)*0.1)]) / sum(acc_costo$costo_total)*100, 1), "%\n")
# =============================================================================
# D. ANÁLISIS POR SUBGRUPOS (para hipótesis del modelo)
# =============================================================================
cat("\n=== D. FRECUENCIA POR SUBGRUPOS ===\n")
cat("\n--- Tasa de accidentes por nivel técnico ---\n")
datos %>%
  group_by(nivel) %>%
  summarise(
    n          = n(),
    acc_total  = sum(n_acc_24m, na.rm=TRUE),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    expo_media = round(mean(exposicion_horas, na.rm=TRUE), 1),
    tasa_1000h = round(acc_total / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)

) %>%
  print()
cat("\n--- Tasa de accidentes por disciplina principal ---\n")
datos %>%
  filter(str_detect(disciplina_principal, "Downhill|DH|Enduro")) %>%

group_by(disciplina_principal) %>%
  summarise(
    n          = n(),
    acc_total  = sum(n_acc_24m, na.rm=TRUE),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    tasa_1000h = round(acc_total / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)

) %>%
  print()
cat("\n--- Tasa de accidentes por región ---\n")
datos %>%
  group_by(region) %>%
  summarise(
    n          = n(),
    acc_total  = sum(n_acc_24m, na.rm=TRUE),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    tasa_1000h = round(acc_total / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)
  ) %>%
  arrange(desc(tasa_1000h)) %>%

print()
cat("\n--- Tasa de accidentes: compite vs no compite ---\n")
datos %>%
  group_by(compite) %>%
  summarise(
    n          = n(),
    media_acc  = round(mean(n_acc_24m, na.rm=TRUE), 3),
    tasa_1000h = round(sum(n_acc_24m, na.rm=TRUE) / sum(exposicion_horas, na.rm=TRUE) * 1000, 3)

) %>%
  print()
# =============================================================================
# E. CORRELACIONES ENTRE VARIABLES
# =============================================================================
cat("\n=== E. CORRELACIONES CON n_acc_24m ===\n")
vars_num <- datos %>%
  dplyr::select(n_acc_24m, edad, anios_practica, exposicion_horas,
                meses_anio, dias_semana, horas_dia) %>%

drop_na()
cor_mat <- cor(vars_num, method = "spearman")
cat("\n--- Correlación de Spearman con n_acc_24m ---\n")
print(round(cor_mat["n_acc_24m", ], 3))
# =============================================================================
# F. GRÁFICOS (guardar como PNG)
# =============================================================================
cat("\n=== F. GENERANDO GRÁFICOS ===\n")
# 1. Histograma de n_acc_24m
p1 <- ggplot(datos, aes(x = n_acc_24m)) +

geom_bar(fill = "#378ADD", color = "white", width = 0.6) +

labs(title = "Distribución de accidentes por persona (24 meses)",
       x = "N° de accidentes", y = "Frecuencia") +

theme_minimal(base_size = 13)
ggsave("graf_01_freq_nacc.png", p1, width = 7, height = 4, dpi = 150)
# 2. Histograma de costo con log-escala
p2 <- ggplot(acc_costo, aes(x = costo_total)) +

geom_histogram(fill = "#e24b4a", color = "white", bins = 25) +
  scale_x_log10(labels = scales::comma) +

labs(title = "Distribución del costo por accidente (escala log)",
       x = "Costo ARS (log)", y = "Frecuencia") +

theme_minimal(base_size = 13)
ggsave("graf_02_sev_costo_log.png", p2, width = 7, height = 4, dpi = 150)
# 3. Costo por nivel técnico
p3 <- datos %>%
  filter(costo_total_24m > 0) %>%
  ggplot(aes(x = reorder(nivel, costo_total_24m, median), y = costo_total_24m)) +

geom_boxplot(fill = "#534AB7", color = "white", outlier.color = "#e24b4a") +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +

labs(title = "Costo total 24m por nivel técnico",
       x = "", y = "Costo ARS (log)") +

theme_minimal(base_size = 11)
ggsave("graf_03_costo_por_nivel.png", p3, width = 8, height = 4, dpi = 150)
# 4. Exposición vs n_acc
p4 <- ggplot(datos, aes(x = exposicion_horas, y = n_acc_24m)) +

geom_point(alpha = 0.4, color = "#1D9E75", size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),

se = TRUE, color = "#e24b4a") +
  labs(title = "Exposición vs N° de accidentes",
       x = "Horas de exposición (24m)", y = "N° de accidentes") +

theme_minimal(base_size = 13)
ggsave("graf_04_expo_vs_nacc.png", p4, width = 7, height = 4, dpi = 150)
# 5. ISC distribución
p5 <- ggplot(acc_sev, aes(x = ISC)) +
  geom_histogram(fill = "#EF9F27", color = "white", bins = 15) +

labs(title = "Distribución del ISC (accidentes graves)",
       x = "Índice de Severidad Clínica", y = "Frecuencia") +

theme_minimal(base_size = 13)
ggsave("graf_05_ISC.png", p5, width = 7, height = 4, dpi = 150)
# 6. Q-Q plot del log(costo) — verificar normalidad en log-escala (Lognormal)
log_costo <- log(acc_costo$costo_total)
png("graf_06_qq_logcosto.png", width=700, height=500, res=150)
qqnorm(log_costo, main="Q-Q plot de log(Costo) — verificación Lognormal",
       col="#378ADD", pch=16, cex=0.7)
qqline(log_costo, col="#e24b4a", lwd=2)
dev.off()
cat("Gráficos guardados en el directorio de trabajo.\n")
# =============================================================================
# G. RESUMEN PARA LA TESINA
# =============================================================================
cat("\n=== G. RESUMEN EJECUTIVO PARA LA TESINA ===\n")
cat("
FRECUENCIA:
  - Media n_acc_24m:      ", round(media_n, 4), "
  - Varianza n_acc_24m:   ", round(var_n, 4), "
  - Índice dispersión:    ", round(indice_d, 4), "
  - Tasa c/1000h expo:    ", round(tasa_por_1000h, 4), "
SEVERIDAD (accidentes con costo > 0):
  - N accidentes:         ", nrow(acc_costo), "
  - Media costo:          ARS", format(round(mean(acc_costo$costo_total)), big.mark="."), "
  - Mediana costo:        ARS", format(round(median(acc_costo$costo_total)), big.mark="."), "

- P95 costo:            ARS", format(round(quantile(acc_costo$costo_total, 0.95)), big.mark="."), "
  - Skewness costo:       ", round(skewness(acc_costo$costo_total), 4), "
CONCLUSIÓN EXPLORATORIA:
  - Frecuencia: ", ifelse(indice_d > 1.2, "BINOMIAL NEGATIVA (sobredispersión)", "POISSON (equidispersión)"), "
  - Severidad:  GAMMA seleccionada (mejor AIC y Anderson-Darling; Lognormal como alternativa en sensibilidad)
")
# =============================================================================
# H. VALIDACIÓN DEL ISC
# =============================================================================
cat("\n=== H. VALIDACIÓN DEL ISC ===\n")
# Dataset: accidentes con ISC > 0 y costo > 0
isc_val <- acc_largo %>%
  filter(!is.na(ISC) & ISC > 0 & !is.na(costo_total) & costo_total > 0)
cat("N accidentes para validación:", nrow(isc_val), "\n")
# Estadísticas del ISC
cat("\n--- Estadísticas del ISC ---\n")
cat("Media:   ", round(mean(isc_val$ISC), 2), "\n")
cat("Mediana: ", round(median(isc_val$ISC), 2), "\n")
cat("Desvío:  ", round(sd(isc_val$ISC), 2), "\n")
cat("Min:     ", min(isc_val$ISC), "\n")
cat("Max:     ", max(isc_val$ISC), "\n")
# Correlación de Spearman ISC vs costo
cat("\n--- Correlación ISC vs Costo ---\n")
cor_sp <- cor.test(isc_val$ISC, isc_val$costo_total, method = "spearman")
cat("Spearman rho:", round(cor_sp$estimate, 4), "\n")
cat("p-valor:     ", format(cor_sp$p.value, scientific = TRUE), "\n")
# Correlación de Pearson en escala logarítmica
cor_pe <- cor.test(log(isc_val$ISC), log(isc_val$costo_total), method = "pearson")
cat("\nPearson r (log-log):", round(cor_pe$estimate, 4), "\n")
cat("p-valor:            ", format(cor_pe$p.value, scientific = TRUE), "\n")
# Terciles de ISC
cat("\n--- Análisis por terciles de ISC ---\n")
isc_val <- isc_val %>%
  mutate(tercil_ISC = case_when(

ISC <= 4  ~ "Bajo (ISC <= 4)",
    ISC <= 9  ~ "Medio (ISC 5-9)",
    ISC >= 10 ~ "Alto (ISC >= 10)"
  ))
isc_val %>%

group_by(tercil_ISC) %>%
  summarise(
    n          = n(),
    ISC_medio  = round(mean(ISC), 2),

costo_medio = round(mean(costo_total)),
    costo_med  = round(median(costo_total))
  ) %>%
  arrange(ISC_medio) %>%
  print()
# Factor entre tercil alto y bajo
costo_bajo <- mean(isc_val$costo_total[isc_val$tercil_ISC == "Bajo (ISC <= 4)"])
costo_alto <- mean(isc_val$costo_total[isc_val$tercil_ISC == "Alto (ISC >= 10)"])
cat("\nFactor Alto/Bajo:", round(costo_alto / costo_bajo, 1), "x\n")
# ISC medio por tipo de lesión
cat("\n--- ISC medio por tipo de lesión ---\n")
isc_val %>%
  filter(!is.na(lesion)) %>%
  group_by(lesion) %>%
  summarise(
    n         = n(),
    ISC_medio = round(mean(ISC), 2),

costo_medio = round(mean(costo_total))
  ) %>%
  arrange(desc(ISC_medio)) %>%
  print()
# ISC medio por parte del cuerpo
cat("\n--- ISC medio por parte del cuerpo ---\n")
isc_val %>%
  filter(!is.na(parte)) %>%
  group_by(parte) %>%
  summarise(
    n         = n(),
    ISC_medio = round(mean(ISC), 2),

costo_medio = round(mean(costo_total))
  ) %>%
  arrange(desc(ISC_medio)) %>%
  print()
