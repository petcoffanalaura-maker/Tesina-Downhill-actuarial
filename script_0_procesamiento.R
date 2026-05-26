# TESINA: Evaluación actuarial del riesgo en ciclismo de montaña
# Carrera de Actuario - FCE-UBA | Ana Laura Petcoff
# Script: Limpieza, ISC y costos por accidente
# Fuente aranceles: Nomenclador MS-GCABA - Vigente 15 Abril 2026
# =============================================================================
library(tidyverse)
library(janitor)
# =============================================================================
# 0. ARANCELES DE REFERENCIA (Nomenclador MS-GCABA, Abril 2026)
# =============================================================================
ARANCEL_GUARDIA        <- 132226   # GUAR.02: observación en guardia
ARANCEL_DIA_INTERNAC   <- 444959   # INC.01: día clínico
ARANCEL_CIRUGIA_OST    <- 1473726  # OYT.04.10: osteosíntesis miembro sup/inf
ARANCEL_CIRUGIA_ART    <- 654955   # OYT.09.01: artroscopia (luxaciones)
ARANCEL_CIRUGIA_DISC   <- 2120812  # OYT.05.04: hernia discal / cadera
ARANCEL_RX             <- 16637    # IMA.20: radiografía simple
ARANCEL_ECOGRAFIA      <- 22183    # IMA.12: ecografía simple
ARANCEL_TAC            <- 159121   # IMA.22: tomografía computada
ARANCEL_RMN            <- 190602   # IMA.21: resonancia magnética
ARANCEL_SESION_REHAB   <- 21610    # REHA.06: kinesioterapia por sesión
FACTOR_PUBLICO         <- 1.0      # nomenclador GCBA
FACTOR_PRIVADO         <- 1.0      # mismo arancel base GCBA (ver análisis de sensibilidad en tesina)
N_ACC <- 7
# =============================================================================
# 1. CARGA
# =============================================================================
datos_raw <- read_csv("respuestas_xlsx.csv", show_col_types = FALSE)
datos <- clean_names(datos_raw)
# Nota: clean_names() resuelve columnas duplicadas agregando el número de columna.
# lesion_principal queda: _18 (acc1), _31 (acc2), _2 (acc3), _3..._6 (acc4-7)
# =============================================================================
# 2. RENOMBRAR
# =============================================================================
datos <- datos %>% rename(
  edad                 = edad_indicar_numero,
  region               = region_donde_mas_practicas_la_disciplina,
  otro_pais            = si_selecciono_otro_pais_especifique_cual,
  disciplina_principal = cual_es_tu_disciplina_principal_de_ciclismo,
  otras_disciplinas    = practicas_otras_disciplinas_ademas_de_la_principal,
  nivel                = como_describirias_tu_nivel_en_la_disciplina_principal,
  anios_practica       = hace_cuantos_anos_practicas_la_disciplina_principal,
  meses_anio           = cuantos_meses_al_ano_practicas_la_disciplina_principal,
  dias_semana          = cuantos_dias_por_semana_practicas_la_disciplina_principal_en_esos_meses,
  horas_dia            = cuantas_horas_dedicas_a_la_disciplina_principal_en_un_dia_tipico_de_salida,
  tendencia_exposicion = comparada_con_tus_primeros_anos_de_practica_como_fue_tu_intensidad_frecuencia_horas_a_lo_largo_del_tiempo,
  compite              = participas_en_competencias,
  competencias_anio    = cuantas_competencias_en_promedio_por_ano_indicar_numero,
  tuvo_lesiones        = tuviste_lesiones_que_requerian_asistencia_medica_en_los_ultimos_24_meses,
  n_acc_24m            = cuantas_lesiones_que_requerian_asistencia_medica_tuviste_en_los_ultimos_24_meses_indicar_numero,
  cobertura            = tenes_cobertura_medica,
  cobertura_deportes   = tu_cobertura_incluye_accidentes_deportivos,
  contrataria_seguro   = contratarias_un_seguro_deportivo_especifico_para_ciclismo,
  wtp_monto            = cuanto_pagarias_usd_mes_por_este_seguro_deportivo,
  tipo_cobertura       = que_tipo_de_cobertura_preferirias
)
# Accidente 1
datos <- datos %>% rename(
  acc1_disc          = disciplina,
  acc1_lesion        = lesion_principal_18,
  acc1_parte         = parte_del_cuerpo,
  acc1_contexto      = contexto,
  acc1_guardia       = requirio_guardia_o_emergencia,
  acc1_internacion   = en_caso_de_requerir_internacion_por_cuantos_dias,
  acc1_cirugia       = requirio_cirugia,
  acc1_imagen        = estudios_de_imagen,
  acc1_rehab         = si_realizaste_rehabilitacion_profesional_kinesiologia_fisioterapia_u_otra_cuantas_sesiones_fueron,
  acc1_evacuacion    = evacuacion,
  acc1_dias_perdidos = dias_de_actividad_perdidos_trabajo_escuela_universidad_etc,
  acc1_descripcion   = queres_contar_algo_mas_sobre_este_accidente_opcional,
  acc1_lugar         = donde_recibiste_atencion_medica_por_este_accidente
)
# Accidente 2
datos <- datos %>% rename(
  acc2_tiene         = tenes_un_segundo_accidente_para_registrar,
  acc2_disc          = disciplina_2,
  acc2_lesion        = lesion_principal_31,
  acc2_parte         = parte_del_cuerpo_2,
  acc2_contexto      = contexto_2,
  acc2_guardia       = requirio_guardia_o_emergencia_2,
  acc2_internacion   = en_caso_de_requerir_internacion_por_cuantos_dias_2,
  acc2_cirugia       = requirio_cirugia_2,
  acc2_imagen        = estudios_de_imagen_2,
  acc2_rehab         = si_realizaste_rehabilitacion_profesional_kinesiologia_fisioterapia_u_otra_cuantas_sesiones_fueron_2,
  acc2_evacuacion    = evacuacion_2,
  acc2_dias_perdidos = dias_de_actividad_perdidos_trabajo_escuela_universidad_etc_2,
  acc2_descripcion   = queres_contar_algo_mas_sobre_este_accidente_opcional_2,
  acc2_lugar         = donde_recibiste_atencion_medica_por_este_accidente_2
)
# Accidente 3
datos <- datos %>% rename(
  acc3_tiene         = tenes_un_tercer_accidente_para_registrar,
  acc3_disc          = disciplina_3,
  acc3_lesion        = lesion_principal_2,
  acc3_parte         = parte_del_cuerpo_3,
  acc3_contexto      = contexto_3,
  acc3_guardia       = requirio_guardia_o_emergencia_3,
  acc3_internacion   = en_caso_de_requerir_internacion_por_cuantos_dias_3,
  acc3_cirugia       = requirio_cirugia_3,
  acc3_imagen        = estudios_de_imagen_3,
  acc3_rehab         = si_realizaste_rehabilitacion_profesional_kinesiologia_fisioterapia_u_otra_cuantas_sesiones_fueron_3,
  acc3_evacuacion    = evacuacion_3,
  acc3_dias_perdidos = dias_de_actividad_perdidos_trabajo_escuela_universidad_etc_3,
  acc3_descripcion   = queres_contar_algo_mas_sobre_este_accidente_opcional_3,
  acc3_lugar         = donde_recibiste_atencion_medica_por_este_accidente_3
)
# Accidente 4
datos <- datos %>% rename(
  acc4_tiene         = tenes_un_cuarto_accidente_para_registrar,
  acc4_disc          = disciplina_4,
  acc4_lesion        = lesion_principal_3,
  acc4_parte         = parte_del_cuerpo_4,
  acc4_contexto      = contexto_4,
  acc4_guardia       = requirio_guardia_o_emergencia_4,
  acc4_internacion   = en_caso_de_requerir_internacion_por_cuantos_dias_4,
  acc4_cirugia       = requirio_cirugia_4,
  acc4_imagen        = estudios_de_imagen_4,
  acc4_rehab         = si_realizaste_rehabilitacion_profesional_kinesiologia_fisioterapia_u_otra_cuantas_sesiones_fueron_4,
  acc4_evacuacion    = evacuacion_4,
  acc4_dias_perdidos = dias_de_actividad_perdidos_trabajo_escuela_universidad_etc_4,
  acc4_descripcion   = queres_contar_algo_mas_sobre_este_accidente_opcional_4,
  acc4_lugar         = donde_recibiste_atencion_medica_por_este_accidente_4
)
# Accidente 5
datos <- datos %>% rename(
  acc5_tiene         = tenes_un_quinto_accidente_para_registrar,
  acc5_disc          = disciplina_5,
  acc5_lesion        = lesion_principal_4,
  acc5_parte         = parte_del_cuerpo_5,
  acc5_contexto      = contexto_5,
  acc5_guardia       = requirio_guardia_o_emergencia_5,
  acc5_internacion   = en_caso_de_requerir_internacion_por_cuantos_dias_5,
  acc5_cirugia       = requirio_cirugia_5,
  acc5_imagen        = estudios_de_imagen_5,
  acc5_rehab         = si_realizaste_rehabilitacion_profesional_kinesiologia_fisioterapia_u_otra_cuantas_sesiones_fueron_5,
  acc5_evacuacion    = evacuacion_5,
  acc5_dias_perdidos = dias_de_actividad_perdidos_trabajo_escuela_universidad_etc_5,
  acc5_descripcion   = queres_contar_algo_mas_sobre_este_accidente_opcional_5,
  acc5_lugar         = donde_recibiste_atencion_medica_por_este_accidente_5
)
# Accidente 6
datos <- datos %>% rename(
  acc6_tiene         = tenes_un_sexto_accidente_para_registrar,
  acc6_disc          = disciplina_6,
  acc6_lesion        = lesion_principal_5,
  acc6_parte         = parte_del_cuerpo_6,
  acc6_contexto      = contexto_6,
  acc6_guardia       = requirio_guardia_o_emergencia_6,
  acc6_internacion   = en_caso_de_requerir_internacion_por_cuantos_dias_6,
  acc6_cirugia       = requirio_cirugia_6,
  acc6_imagen        = estudios_de_imagen_6,
  acc6_rehab         = si_realizaste_rehabilitacion_profesional_kinesiologia_fisioterapia_u_otra_cuantas_sesiones_fueron_6,
  acc6_evacuacion    = evacuacion_6,
  acc6_dias_perdidos = dias_de_actividad_perdidos_trabajo_escuela_universidad_etc_6,
  acc6_descripcion   = queres_contar_algo_mas_sobre_este_accidente_opcional_6,
  acc6_lugar         = donde_recibiste_atencion_medica_por_este_accidente_6
)
# Accidente 7
datos <- datos %>% rename(
  acc7_tiene         = tenes_un_septimo_accidente_para_registrar,
  acc7_disc          = disciplina_7,
  acc7_lesion        = lesion_principal_6,
  acc7_parte         = parte_del_cuerpo_7,
  acc7_contexto      = contexto_7,
  acc7_guardia       = requirio_guardia_o_emergencia_7,
  acc7_internacion   = en_caso_de_requerir_internacion_por_cuantos_dias_7,
  acc7_cirugia       = requirio_cirugia_7,
  acc7_imagen        = estudios_de_imagen_7,
  acc7_rehab         = si_realizaste_rehabilitacion_profesional_kinesiologia_fisioterapia_u_otra_cuantas_sesiones_fueron_7,
  acc7_evacuacion    = evacuacion_7,
  acc7_dias_perdidos = dias_de_actividad_perdidos_trabajo_escuela_universidad_etc_7,
  acc7_descripcion   = queres_contar_algo_mas_sobre_este_accidente_opcional_7,
  acc7_lugar         = donde_recibiste_atencion_medica_por_este_accidente_7
)
# Eliminar columnas fantasma
datos <- datos %>% dplyr::select(-matches("^parte_del_cuerpo_(8|9|10|11|12|13)$"))
# =============================================================================
# 3. LIMPIEZA
# =============================================================================
datos <- datos %>%
  mutate(fecha = Sys.Date())
rehab_cols    <- paste0("acc", 1:N_ACC, "_rehab")
dias_cols     <- paste0("acc", 1:N_ACC, "_dias_perdidos")
internac_cols <- paste0("acc", 1:N_ACC, "_internacion")
datos <- datos %>%

mutate(across(all_of(rehab_cols),    ~ replace_na(as.numeric(.x), 0))) %>%
  mutate(across(all_of(dias_cols),     ~ replace_na(as.numeric(.x), 0))) %>%
  mutate(across(all_of(internac_cols), ~ replace_na(as.numeric(.x), 0))) %>%

mutate(n_acc_24m = if_else(tuvo_lesiones == "No", 0, as.numeric(n_acc_24m)))
# =============================================================================
# 4. FILTROS DE MUESTRA
# =============================================================================
# 1. Solo Argentina
# 2. Solo DH o Enduro como disciplina principal O secundaria
# 3. n_acc_24m <= 7 (el formulario solo captura hasta 7 accidentes;
#    quien declara 9 probablemente incluyó accidentes sin atención médica
#    o errores de tipeo — se documenta como criterio de exclusión)
datos <- datos %>%
  filter(region != "Otro país") %>%
  filter(
    str_detect(disciplina_principal, "Downhill|DH|Enduro") |
      str_detect(otras_disciplinas,    "Downhill|DH|Enduro")

) %>%
  filter(is.na(n_acc_24m) | n_acc_24m <= 7)
cat("Respondentes tras filtros:", nrow(datos), "\n")
# =============================================================================
# 5. EXPOSICIÓN (ventana 24 meses)
# =============================================================================
datos <- datos %>%
  mutate(
    # Para menos de 1 año: meses calculados desde anios_practica directamente
    # Para el resto: usa meses_anio reportado
    meses_efectivos  = if_else(anios_practica < 1,
                               anios_practica * 12,
                               as.numeric(meses_anio)),
    # Ventana máxima: 2 años (24 meses)
    anos_exposicion  = pmin(anios_practica, 2),
    # tendencia_exposicion entra como covariable en el GLM
    exposicion_horas = anos_exposicion * (meses_efectivos / 12) * dias_semana * horas_dia * 52.18
  )
# Excluir outliers de exposición (>5000h en 24m = implausible)
# Criterio documentado en tesina como criterio de calidad de datos
n_antes <- nrow(datos)
datos <- datos %>% filter(exposicion_horas <= 5000)
cat("Outliers de exposicion excluidos:", n_antes - nrow(datos), "\n")
cat("Respondentes finales:", nrow(datos), "\n")
# =============================================================================
# 6. FUNCIONES: COSTO E ISC
# =============================================================================
factor_lugar <- function(lugar) {

case_when(
    is.na(lugar)                                 ~ FACTOR_PUBLICO,

str_detect(lugar, "blico|público")           ~ FACTOR_PUBLICO,
    str_detect(lugar, "privado|clínica|clinica") ~ FACTOR_PRIVADO,

TRUE                                         ~ FACTOR_PUBLICO
  )
}
costo_imagen <- function(imagen) {
  case_when(

is.na(imagen)                       ~ 0,
    str_detect(imagen, "Ninguno")       ~ 0,
    str_detect(imagen, "Radiograf")     ~ ARANCEL_RX,
    str_detect(imagen, "Ecograf")       ~ ARANCEL_ECOGRAFIA,
    str_detect(imagen, "TAC|Tomograf")  ~ ARANCEL_TAC,
    str_detect(imagen, "Resonan|RMN")   ~ ARANCEL_RMN,
    TRUE                                ~ ARANCEL_RX
  )
}
# Cirugía diferenciada por lesión y parte del cuerpo
# OYT.09.01 artroscopia / OYT.05.04 cadera-espalda / OYT.04.10 osteosíntesis
costo_cirugia <- function(cirugia, lesion, parte) {

case_when(
    is.na(cirugia) | cirugia == "No"   ~ 0,

str_detect(parte,  "Espalda")      ~ ARANCEL_CIRUGIA_DISC,
    str_detect(parte,  "Cadera|pelvis")~ ARANCEL_CIRUGIA_DISC,

str_detect(lesion, "Luxaci")       ~ ARANCEL_CIRUGIA_ART,
    TRUE                               ~ ARANCEL_CIRUGIA_OST

)
}
calcular_ISC <- function(guardia, internacion, cirugia, lesion, parte, imagen, rehab, evacuacion) {

pts_guardia  <- if_else(!is.na(guardia) & str_detect(guardia, "^S[ií]"), 1, 0)
  pts_cirugia  <- if_else(!is.na(cirugia) & str_detect(cirugia, "^S[ií]"), 5, 0)

pts_internac <- case_when(
    is.na(internacion) | internacion == 0 ~ 0,
    internacion <= 3                      ~ 2,
    internacion <= 7                      ~ 4,
    internacion > 7                       ~ 7,
    TRUE                                  ~ 0
  )

  # pts_imagen: suma aditiva por tipo de estudio (corregido: case_when solo captura el primero)

pts_imagen <- if_else(is.na(imagen) | str_detect(imagen, "Ninguno"), 0L,
                        as.integer(str_detect(imagen, "Radiograf")) * 1L +
                          as.integer(str_detect(imagen, "Ecograf"))  * 2L +
                          as.integer(str_detect(imagen, "TAC|Tomograf")) * 2L +
                          as.integer(str_detect(imagen, "Resonan|RMN")) * 3L
  )

  pts_rehab <- case_when(
    is.na(rehab) | rehab == 0 ~ 0,
    rehab < 4                 ~ 1,
    rehab <= 12               ~ 2,
    rehab > 12                ~ 4,
    TRUE                      ~ 0
  )

  pts_evacuacion <- case_when(
    is.na(evacuacion)                             ~ 0,
    str_detect(evacuacion, "Autoe|compa")         ~ 0,
    str_detect(evacuacion, "terrestre|Rescate t") ~ 2,
    str_detect(evacuacion, "éreo|aereo|aéreo")    ~ 5,

TRUE                                          ~ 0
  )

  pts_guardia + pts_internac + pts_cirugia + pts_imagen + pts_rehab + pts_evacuacion
}
# =============================================================================
# 7. CALCULAR ISC Y COSTO POR ACCIDENTE
# =============================================================================
for (i in 1:N_ACC) {
  g   <- paste0("acc", i, "_guardia")

int <- paste0("acc", i, "_internacion")
  cir <- paste0("acc", i, "_cirugia")
  les <- paste0("acc", i, "_lesion")
  par <- paste0("acc", i, "_parte")

img <- paste0("acc", i, "_imagen")
  reh <- paste0("acc", i, "_rehab")

eva <- paste0("acc", i, "_evacuacion")
  lug <- paste0("acc", i, "_lugar")

  fl <- factor_lugar(datos[[lug]])

  datos[[paste0("acc", i, "_ISC")]] <- calcular_ISC(
    datos[[g]], datos[[int]], datos[[cir]], datos[[les]],
    datos[[par]], datos[[img]], datos[[reh]], datos[[eva]]
  )

  datos[[paste0("acc", i, "_costo_guardia")]] <-

if_else(!is.na(datos[[g]]) & str_detect(datos[[g]], "^S[ií]"),

ARANCEL_GUARDIA * fl, 0)

  datos[[paste0("acc", i, "_costo_cirugia")]] <-
    costo_cirugia(datos[[cir]], datos[[les]], datos[[par]]) * fl

  datos[[paste0("acc", i, "_costo_internac")]] <-
    if_else(is.na(datos[[int]]) | datos[[int]] == 0, 0,
            datos[[int]] * ARANCEL_DIA_INTERNAC * fl)

  datos[[paste0("acc", i, "_costo_imagen")]] <- costo_imagen(datos[[img]]) * fl
  datos[[paste0("acc", i, "_costo_rehab")]]  <- datos[[reh]] * ARANCEL_SESION_REHAB * fl

  datos[[paste0("acc", i, "_costo_total")]] <-
    datos[[paste0("acc", i, "_costo_guardia")]]  +
    datos[[paste0("acc", i, "_costo_internac")]] +
    datos[[paste0("acc", i, "_costo_cirugia")]]  +
    datos[[paste0("acc", i, "_costo_imagen")]]   +
    datos[[paste0("acc", i, "_costo_rehab")]]
}
# Totales por persona
isc_cols   <- paste0("acc", 1:N_ACC, "_ISC")
costo_cols <- paste0("acc", 1:N_ACC, "_costo_total")
datos <- datos %>%
  mutate(
    ISC_max         = pmax(!!!syms(isc_cols),   na.rm = TRUE),
    costo_total_24m = rowSums(across(all_of(costo_cols)), na.rm = TRUE)

)
# =============================================================================
# 8. WTP — normalizar formato y calcular punto medio
# =============================================================================
datos <- datos %>%
  mutate(wtp_monto = case_when(
    wtp_monto == "20-Nov" | wtp_monto == "11-20" ~ "11 a 20",
    wtp_monto == "21-30"                         ~ "21 a 30",
    wtp_monto == "31-50"                         ~ "31 a 50",
    wtp_monto == "6-10"                          ~ "6 a 10",
    wtp_monto == "0-5"                           ~ "0 a 5",

wtp_monto == "51+"                           ~ "51 o más",
    TRUE                                         ~ wtp_monto

)) %>%
  mutate(wtp_medio = case_when(
    wtp_monto == "0 a 5"    ~  2.5,
    wtp_monto == "6 a 10"   ~  8.0,
    wtp_monto == "11 a 20"  ~ 15.5,
    wtp_monto == "21 a 30"  ~ 25.5,

wtp_monto == "31 a 50"  ~ 40.5,
    wtp_monto == "51 o más" ~ 60.0,
    TRUE                    ~ NA_real_
  ))
# =============================================================================
# 9. FORMATO LARGO: un accidente por fila (para GLM)
# =============================================================================
acc_largo <- datos %>%
  dplyr::select(timestamp, edad, genero, region, disciplina_principal, nivel,
                tendencia_exposicion, anios_practica, exposicion_horas, n_acc_24m,

matches("^acc[1-7]_")) %>%
  pivot_longer(
    cols          = matches("^acc[1-7]_"),
    names_to      = c("acc_num", ".value"),
    names_pattern = "acc([1-7])_(.*)"
  ) %>%
  filter(!is.na(disc)) %>%
  mutate(acc_num = as.integer(acc_num))
# =============================================================================
# 10. EXPORTAR
# =============================================================================
write_csv(datos,     "datos_wide.csv")
write_csv(acc_largo, "datos_largo.csv")
cat("\nProcesamiento completo\n")
cat("  Respondentes:          ", nrow(datos), "\n")
cat("  Accidentes registrados:", nrow(acc_largo), "\n")
# =============================================================================
# 11. RESUMEN
# =============================================================================
cat("\n--- Exposicion 24m (horas) ---\n")
print(summary(datos$exposicion_horas))
cat("\n--- n_acc_24m ---\n")
print(table(datos$n_acc_24m, useNA = "ifany"))
cat("\n--- ISC maximo por persona ---\n")
print(summary(datos$ISC_max))
cat("\n--- Costo total 24m (ARS) ---\n")
print(summary(datos$costo_total_24m))
cat("\n--- WTP (USD/mes) ---\n")
print(table(datos$wtp_monto, useNA = "ifany"))
cat("\n--- Verificacion outliers n_acc ---\n")
datos %>%
  filter(n_acc_24m >= 6) %>%
  dplyr::select(edad, nivel, disciplina_principal, anios_practica,
                exposicion_horas, n_acc_24m, compite) %>%
  print()
# =============================================================================
# 12. ANÁLISIS DE SENSIBILIDAD — Factor privado
# =============================================================================
# El nomenclador GCBA se usa como base única (FACTOR_PRIVADO = 1.0).
# Este bloque evalúa el impacto de distintos factores sobre los casos privados.
# Fuente limitación: no existe nomenclador público para el sector privado en AR.
cat("\n=== ANÁLISIS DE SENSIBILIDAD — Factor privado ===\n")
factores <- c(1.0, 1.5, 2.0, 2.5)
# Identificar casos con atención privada en accidente 1
privado_mask <- !is.na(datos$acc1_lugar) &
  str_detect(datos$acc1_lugar, "privado|clínica|clinica")
resultados_sens <- sapply(factores, function(f) {

costo_ajustado <- datos$costo_total_24m
  # Aplicar factor solo a los casos privados
  costo_ajustado[privado_mask] <- costo_ajustado[privado_mask] * f
  c(
    media   = round(mean(costo_ajustado, na.rm=TRUE)),
    mediana = round(median(costo_ajustado, na.rm=TRUE)),
    p95     = round(quantile(costo_ajustado, 0.95, na.rm=TRUE))
  )
})
colnames(resultados_sens) <- paste0("Factor_", factores)
cat("\nCosto total 24m (ARS) según factor privado aplicado:\n")
print(resultados_sens)
cat("\nNota: Factor 1.0 = nomenclador GCBA para todos (base)\n")
cat("      Factor 2.5 = estimación conservadora sector privado libre\n")
