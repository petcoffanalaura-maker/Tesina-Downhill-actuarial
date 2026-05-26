Script 6: Prima comercial y propuesta de seguro
Corresponde a las secciones 4.6, 7.1, 7.3 y 7.4. Aplica los recargos comerciales sobre la prima pura, segmenta por perfil recreativo/competitivo, analiza los determinantes de la WTP mediante pruebas no paramétricas (Kruskal-Wallis, Mann-Whitney), y realiza los análisis de sensibilidad al factor de costo privado y a los recargos. Produce los gráficos A25–A27.
# =============================================================================
# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Paso 6: Prima comercial y propuesta de seguro
# Prerequisito: correr procesamiento_encuesta_MTB.R + pasos 2, 3, 4 y 5
# =============================================================================
library(tidyverse)
library(ggplot2)
# =============================================================================
# A. ESTRUCTURA DE LA PRIMA COMERCIAL — AMBOS PLANES
# =============================================================================
cat("=== A. ESTRUCTURA DE LA PRIMA COMERCIAL ===\n")
prima_pura_base_sl <- round(prima_pura_sl / TC / 12, 2)
prima_pura_base_cl <- round(prima_pura_cl / TC / 12, 2)
RECARGO_SEGURIDAD <- 0.15
RECARGO_GASTOS    <- 0.12
RECARGO_UTILIDAD  <- 0.08
FACTOR_COMERCIAL  <- (1 + RECARGO_SEGURIDAD) * (1 + RECARGO_GASTOS) * (1 + RECARGO_UTILIDAD)
prima_com_sl <- round(prima_pura_base_sl * FACTOR_COMERCIAL, 2)
prima_com_cl <- round(prima_pura_base_cl * FACTOR_COMERCIAL, 2)
prima_ic_sl_low  <- round(quantile(prima_boot_sl_usd, 0.025) * FACTOR_COMERCIAL, 2)
prima_ic_sl_high <- round(quantile(prima_boot_sl_usd, 0.975) * FACTOR_COMERCIAL, 2)
prima_ic_cl_low  <- round(quantile(prima_boot_cl_usd, 0.025) * FACTOR_COMERCIAL, 2)
prima_ic_cl_high <- round(quantile(prima_boot_cl_usd, 0.975) * FACTOR_COMERCIAL, 2)
cat("\nRecargos:\n")
cat("  Seguridad:", RECARGO_SEGURIDAD*100, "%\n")
cat("  Gastos:   ", RECARGO_GASTOS*100, "%\n")
cat("  Utilidad: ", RECARGO_UTILIDAD*100, "%\n")
cat("  Factor total:", round(FACTOR_COMERCIAL, 4), "\n")
cat("\n--- Plan sin límite ---\n")
cat("  Prima pura:    USD", prima_pura_base_sl, "/mes\n")
cat("  Prima com:     USD", prima_com_sl, "/mes\n")
cat("  IC 95%: [USD", prima_ic_sl_low, ", USD", prima_ic_sl_high, "]\n")
cat("\n--- Plan con límite ARS 3.000.000 ---\n")
cat("  Prima pura:    USD", prima_pura_base_cl, "/mes\n")
cat("  Prima com:     USD", prima_com_cl, "/mes\n")
cat("  IC 95%: [USD", prima_ic_cl_low, ", USD", prima_ic_cl_high, "]\n")
# =============================================================================
# B. PRIMA COMERCIAL vs WTP
# =============================================================================
cat("\n=== B. PRIMA COMERCIAL vs DISPOSICIÓN A PAGAR ===\n")
wtp_dist <- datos %>%
  filter(!is.na(wtp_monto)) %>%
  mutate(wtp_monto = case_when(
    wtp_monto == "20-Nov" | wtp_monto == "11-20" ~ "11 a 20",
    wtp_monto == "21-30"                         ~ "21 a 30",
    wtp_monto == "31-50"                         ~ "31 a 50",
    wtp_monto == "6-10"                          ~ "6 a 10",
    wtp_monto == "0-5"                           ~ "0 a 5",

wtp_monto == "51+"                           ~ "51 o más",
    TRUE                                         ~ wtp_monto

)) %>%
  count(wtp_monto) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    wtp_medio = case_when(
      wtp_monto == "0 a 5"    ~  2.5,
      wtp_monto == "6 a 10"   ~  8.0,
      wtp_monto == "11 a 20"  ~ 15.5,
      wtp_monto == "21 a 30"  ~ 25.5,
      wtp_monto == "31 a 50"  ~ 40.5,

wtp_monto == "51 o más" ~ 60.0,
      TRUE                    ~ NA_real_

)
  ) %>%
  arrange(wtp_medio)
wtp_prom <- sum(wtp_dist$wtp_medio * wtp_dist$n, na.rm=TRUE) / sum(wtp_dist$n)
mercado_sl <- wtp_dist %>%
  filter(wtp_medio >= prima_com_sl) %>%

summarise(n=sum(n), pct=sum(pct))
mercado_cl <- wtp_dist %>%
  filter(wtp_medio >= prima_com_cl) %>%

summarise(n=sum(n), pct=sum(pct))
cat("\nDistribución WTP:\n")
print(wtp_dist)
cat("\nWTP promedio ponderado: USD", round(wtp_prom, 2), "/mes\n")
cat("\n--- Plan sin límite ---\n")
cat("  Prima com: USD", prima_com_sl, "| Brecha: USD", round(prima_com_sl - wtp_prom, 2), "\n")
cat("  Mercado potencial:", mercado_sl$n, "personas (", mercado_sl$pct, "% muestra)\n")
cat("\n--- Plan con límite ARS 3.000.000 ---\n")
cat("  Prima com: USD", prima_com_cl, "| Brecha: USD", round(prima_com_cl - wtp_prom, 2), "\n")
cat("  Mercado potencial:", mercado_cl$n, "personas (", mercado_cl$pct, "% muestra)\n")
# =============================================================================
# C. SENSIBILIDAD A RECARGOS — plan con límite
# =============================================================================
cat("\n=== C. SENSIBILIDAD A RECARGOS (plan con límite) ===\n")
escenarios <- expand.grid(
  recargo_seg    = c(0.10, 0.15, 0.20, 0.25),
  recargo_gastos = c(0.10, 0.12, 0.15, 0.20)
) %>%
  mutate(
    factor    = (1 + recargo_seg) * (1 + recargo_gastos) * (1 + RECARGO_UTILIDAD),
    prima_com = round(prima_pura_base_cl * factor, 2)
  )
cat("\nPrima comercial plan con límite según recargos (USD/mes):\n")
print(escenarios %>%
  dplyr::select(recargo_seg, recargo_gastos, prima_com) %>%

pivot_wider(names_from=recargo_gastos, values_from=prima_com,

names_prefix="gastos_"))
# =============================================================================
# D. PROPUESTA DE COBERTURA
# =============================================================================
cat("\n=== D. PROPUESTA DE COBERTURA ===\n")
cobertura <- data.frame(
  Item = c(
    "Atención en guardia de emergencia",
    "Estudios de imagen (Rx, TAC, RMN)",
    "Cirugía traumatológica",
    "Internación (hasta 7 días)",
    "Rehabilitación (hasta 30 sesiones)",
    "Traslado y rescate terrestre",
    "Rescate aéreo (hasta USD 500)",
    "Daños al equipo"
  ),
  Plan_sin_limite  = c("Si","Si","Si","Si","Si","Si","Si","No"),
  Plan_con_limite  = c("Si","Si","Si","Si","Si","Si","Si","No")
)
cat("\nCoberturas (iguales en ambos planes, difieren en límite por siniestro):\n")
print(cobertura)
cat("\nPreferencia de cobertura (encuesta):\n")
print(table(datos$tipo_cobertura))
# =============================================================================
# E. VIABILIDAD — AMBOS PLANES
# =============================================================================
cat("\n=== E. ANÁLISIS DE VIABILIDAD ===\n")
N_POTENCIAL <- 15000
# Plan sin límite
penetracion_sl <- mercado_sl$pct / 100 * 0.5
n_aseg_sl      <- round(N_POTENCIAL * penetracion_sl)
ingreso_sl     <- round(n_aseg_sl * prima_com_sl * 12)
# Plan con límite
penetracion_cl <- mercado_cl$pct / 100 * 0.5
n_aseg_cl      <- round(N_POTENCIAL * penetracion_cl)
ingreso_cl     <- round(n_aseg_cl * prima_com_cl * 12)
cat("\nMercado potencial estimado:", N_POTENCIAL, "ciclistas DH+Enduro en Argentina\n")
cat("\n--- Plan sin límite ---\n")
cat("  Penetración esperada:", round(penetracion_sl*100,1), "%\n")
cat("  Asegurados estimados:", n_aseg_sl, "\n")
cat("  Ingreso anual: USD", format(ingreso_sl, big.mark=","), "\n")
cat("\n--- Plan con límite ARS 3.000.000 ---\n")
cat("  Penetración esperada:", round(penetracion_cl*100,1), "%\n")
cat("  Asegurados estimados:", n_aseg_cl, "\n")
cat("  Ingreso anual: USD", format(ingreso_cl, big.mark=","), "\n")
# =============================================================================
# F. GRÁFICOS
# =============================================================================
cat("\n=== F. GENERANDO GRÁFICOS ===\n")
wtp_plot <- wtp_dist %>% filter(!is.na(wtp_medio))
# WTP vs ambas primas
p_wtp <- ggplot(wtp_plot, aes(x=reorder(wtp_monto, wtp_medio), y=n)) +
  geom_col(fill="#d0d0d0", width=0.7) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=which(wtp_plot$wtp_medio >= prima_com_sl)[1] - 0.5,
             color="#378ADD", linewidth=1, linetype="dashed") +
  geom_vline(xintercept=which(wtp_plot$wtp_medio >= prima_com_cl)[1] - 0.5,
             color="#1D9E75", linewidth=1, linetype="dashed") +
  annotate("text", x=5.2, y=max(wtp_plot$n)*0.9,

label=paste0("Sin límite\nUSD ", prima_com_sl),

color="#378ADD", size=3.2) +
  annotate("text", x=3.8, y=max(wtp_plot$n)*0.9,

label=paste0("Con límite\nUSD ", prima_com_cl),
           color="#1D9E75", size=3.2) +
  labs(title="Disposición a pagar vs prima comercial — ambos planes",
       x="WTP (USD/mes)", y="N° encuestados") +

theme_minimal(base_size=12)
ggsave("graf_24_wtp_vs_prima.png", p_wtp, width=8, height=4, dpi=150)
# Estructura prima plan con límite
estructura <- data.frame(
  componente = c("Prima pura", "Recargo seguridad", "Gastos admin.", "Utilidad"),
  valor = c(
    prima_pura_base_cl,
    prima_pura_base_cl * RECARGO_SEGURIDAD,
    prima_pura_base_cl * (1 + RECARGO_SEGURIDAD) * RECARGO_GASTOS,
    prima_pura_base_cl * (1 + RECARGO_SEGURIDAD) * (1 + RECARGO_GASTOS) * RECARGO_UTILIDAD
  )
)
estructura$pct <- round(estructura$valor / sum(estructura$valor) * 100, 1)
p_estr <- ggplot(estructura, aes(x=reorder(componente, valor), y=valor, fill=componente)) +

geom_col(width=0.6) +
  scale_fill_manual(values=c("#378ADD","#e24b4a","#EF9F27","#1D9E75")) +
  geom_text(aes(label=paste0("USD ", round(valor,1), " (", pct, "%)")),
            hjust=-0.1, size=3.5) +
  coord_flip() +
  expand_limits(y=prima_com_cl * 1.3) +

labs(title=paste0("Estructura prima comercial con límite — USD ", prima_com_cl, "/mes"),
       x="", y="USD/mes", fill="") +

theme_minimal(base_size=12) +
  theme(legend.position="none")
ggsave("graf_25_estructura_prima.png", p_estr, width=8, height=4, dpi=150)
# Sensibilidad recargos
p_sens <- escenarios %>%
  mutate(gastos_label = paste0("Gastos ", recargo_gastos*100, "%")) %>%
  ggplot(aes(x=factor(recargo_seg*100), y=prima_com,
             group=gastos_label, color=gastos_label)) +

geom_line(linewidth=1.2) +
  geom_point(size=3) +
  geom_hline(yintercept=wtp_prom, linetype="dashed", color="#888780") +
  annotate("text", x=0.6, y=wtp_prom+0.5,
           label=paste0("WTP promedio USD ", round(wtp_prom,1)),

color="#888780", size=3.2) +
  labs(title="Sensibilidad prima comercial a recargos — plan con límite (USD/mes)",
       x="Recargo seguridad (%)", y="Prima USD/mes", color="") +

theme_minimal(base_size=12)
ggsave("graf_26_sensibilidad_recargos.png", p_sens, width=8, height=4, dpi=150)
cat("Gráficos guardados: graf_24, graf_25, graf_26\n")
# =============================================================================
# G. RESUMEN EJECUTIVO PASO 6
# =============================================================================
cat("\n=== G. RESUMEN EJECUTIVO PASO 6 ===\n")
cat("
FACTOR COMERCIAL:", round(FACTOR_COMERCIAL, 3), "
PLAN SIN LÍMITE:
  - Prima pura:  USD", prima_pura_base_sl, "/mes
  - Prima com:   USD", prima_com_sl, "/mes

- IC 95%: [USD", prima_ic_sl_low, ", USD", prima_ic_sl_high, "]

- Mercado:     ", mercado_sl$pct, "% muestra |", n_aseg_sl, "asegurados
  - Ingreso:     USD", format(ingreso_sl, big.mark=","), "/año
PLAN CON LÍMITE ARS 3.000.000:
  - Prima pura:  USD", prima_pura_base_cl, "/mes
  - Prima com:   USD", prima_com_cl, "/mes

- IC 95%: [USD", prima_ic_cl_low, ", USD", prima_ic_cl_high, "]

- Mercado:     ", mercado_cl$pct, "% muestra |", n_aseg_cl, "asegurados
  - Ingreso:     USD", format(ingreso_cl, big.mark=","), "/año
WTP:
  - Promedio ponderado: USD", round(wtp_prom, 2), "/mes
  - Brecha plan sin límite:   USD", round(prima_com_sl - wtp_prom, 2), "/mes
  - Brecha plan con límite:   USD", round(prima_com_cl - wtp_prom, 2), "/mes
")
# =============================================================================
# ANÁLISIS WTP — tests estadísticos y segmentación
# =============================================================================
cat("=== WTP: ESTADÍSTICAS GENERALES ===\n")
wtp_analisis <- datos %>%

filter(!is.na(wtp_medio)) %>%

mutate(nivel_f2 = case_when(

str_detect(nivel, "Principiante|Intermedio") ~ "Princ/Interm",
    str_detect(nivel, "Avanzado")                ~ "Avanzado",
    str_detect(nivel, "Experto")                 ~ "Experto",

TRUE                                         ~ NA_character_
  ))
cat("N respondentes WTP:", nrow(wtp_analisis), "\n")
cat("WTP promedio ponderado: USD", round(wtp_prom, 2), "/mes\n")
cat("WTP mediana:", median(wtp_analisis$wtp_medio, na.rm=TRUE), "\n")
cat("\n--- Distribución WTP ---\n")
print(wtp_dist)
cat("\n=== WTP POR NIVEL TÉCNICO ===\n")
wtp_nivel <- wtp_analisis %>%
  filter(!is.na(nivel_f2)) %>%
  group_by(nivel_f2) %>%
  summarise(
    n          = n(),
    wtp_media  = round(mean(wtp_medio, na.rm=TRUE), 2),

wtp_mediana= median(wtp_medio, na.rm=TRUE)
  )
print(wtp_nivel)
cat("\n--- Kruskal-Wallis por nivel técnico ---\n")
kw <- kruskal.test(wtp_medio ~ nivel_f2, data = wtp_analisis)
print(kw)
cat("\n--- Mann-Whitney: Experto vs resto ---\n")
experto     <- wtp_analisis$wtp_medio[wtp_analisis$nivel_f2 == "Experto"]
no_experto  <- wtp_analisis$wtp_medio[wtp_analisis$nivel_f2 !=
"Experto"]
mw_experto  <- wilcox.test(experto, no_experto, alternative="greater")
print(mw_experto)
cat("\n--- Distribución WTP por nivel (tabla %) ---\n")
wtp_tabla <- wtp_analisis %>%
  filter(!is.na(nivel_f2)) %>%
  group_by(nivel_f2, wtp_monto) %>%
  summarise(n=n(), .groups="drop") %>%
  group_by(nivel_f2) %>%
  mutate(pct = round(n/sum(n)*100, 1)) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from=wtp_monto, values_from=pct, values_fill=0)
print(wtp_tabla)
cat("\n=== WTP Y OTRAS VARIABLES ===\n")
cat("\n--- Spearman WTP vs edad ---\n")
cor_edad <- cor.test(wtp_analisis$wtp_medio, wtp_analisis$edad,
                     method="spearman")
cat("rho:", round(cor_edad$estimate, 4), "p:", round(cor_edad$p.value, 4), "\n")
cat("\n--- Mann-Whitney WTP: tuvo accidente vs no ---\n")
mw_acc <- wilcox.test(wtp_medio ~ tuvo_lesiones, data=wtp_analisis)
cat("p-valor:", round(mw_acc$p.value, 4), "\n")
cat("\n--- Mann-Whitney WTP: compite vs no compite ---\n")
mw_comp <- wilcox.test(wtp_medio ~ compite, data=wtp_analisis)
cat("p-valor:", round(mw_comp$p.value, 4), "\n")
cat("\n=== WTP POR REGIÓN ===\n")
wtp_analisis %>%
  group_by(region) %>%
  summarise(
    n        = n(),
    wtp_media= round(mean(wtp_medio, na.rm=TRUE), 2)

) %>%
  arrange(desc(wtp_media)) %>%
  print()
cat("\n=== MERCADO POTENCIAL POR PRIMA ===\n")
cat("Prima sin límite USD", prima_com_sl, ":\n")
cat("  Mercado:", mercado_sl$pct, "% (", mercado_sl$n, "personas)\n")
cat("Prima con límite USD", prima_com_cl, ":\n")
cat("  Mercado:", mercado_cl$pct, "% (", mercado_cl$n, "personas)\n")
