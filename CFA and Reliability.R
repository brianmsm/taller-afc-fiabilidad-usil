
#################################################################
####   III CONGRESO INTERNACIONAL DE EVALUACIÓN PSICOLÓGICA  ####
###   ANÁLISIS FACTORIAL CONFIRMATORIO Y CONFIABILIDAD CON R  ###
######  PEÑA-CALERO, BRIAN N. Y TAFUR-MENDOZA, ARNOLD A.  #######
###  GRUPO DE ESTUDIOS AVANCES EN MEDICIÓN PSICOLÓGICA (AMP) ####
####################  12 DE OCTUBRE DE 2019  ####################
#################################################################

# Instalar herramienta para el manejo de paquetes
install.packages("pacman")

# "p_load" carga los paquetes especificados
# Si los paquetes no están instalados, "p_load" los instala
# [lavaan] paquete para análisis de variables latentes
# [tidyverse] conjunto de paquete con utilidades múltiples
# [readxl] paquete para importar bases de datos en Excel
# [MVN] paquete para análisis de normalidad multivariada
# [semPlot] paquete para Path Diagrams
# devtools::install_github("simsem/semTools/semTools")
pacman::p_load_gh("simsem/semTools/semTools")
pacman::p_load(lavaan, dplyr, readxl, MVN, semPlot)

# Cargamos la base de datos de inteligencia emocional
base <- read_excel("base_inteligencia.xlsx")

# Revisar si la base de datos fue cargada correctamente
glimpse(base)


######################################
## Análisis Factorial Confirmatorio ##
######################################

# Especificación del modelo factorial
# 5 variables latentes medidas por 4 ítems cada una
# 20 ítems: del I01 al I20
# Cada línea representa un factor con sus ítems
# =~ este símbolo puede ser leído como <medido por>
inteligencia <- " intrapersonal   =~ I03 + I07 + I10 + I16
                  interpersonal   =~ I01 + I05 + I13 + I19
                  manejo_emocion  =~ I02 + I08 + I12 + I18
                  adaptabilidad   =~ I06 + I09 + I11 + I14
                  estado_animo    =~ I04 + I15 + I17 + I20 "

# Identificación y Estimación del modelo factorial
# "model" indica el modelo a estimar
# "data" base de datos donde se encuentran las variables
# "missing" especifica el método para missing value
# Full Information Maximum Likelihood (FIML)
fit_ml <- cfa(model = inteligencia, data = base, missing = "fiml")

# Evaluación del modelo factorial
# "object" indica el modelo estimado
# "fit.measures" muestra los índices de ajuste
# "standardized" muestra las medidas estandarizadas
summary(object = fit_ml, fit.measures = TRUE, standardized = TRUE)

# Mostrar una mayor cantidad de índices de ajuste
fitMeasures(fit_ml)

# Reespecificación del modelo factorial
# Índices de modificación
# "object" indica el modelo estimado
# "sort." permite ordenar los índices de modificación (mayor a menor)
# "minimum.value" permite establecer un punto de corte
modificationIndices(object = fit_ml, sort. = T, minimum.value = 3, maximum.number = 10)

# Modelo reespecificado
inteligencia_re <- " intrapersonal   =~ I03 + I07 + I10 + I16
                     interpersonal   =~ I01 + I05 + I13 + I19
                     manejo_emocion  =~ I02 + I08 + I12 + I18
                     adaptabilidad   =~ I06 + I09 + I11 + I14
                     estado_animo    =~ I04 + I15 + I17 + I20 
                     I19 ~~ I18 "
fit_re <- cfa(inteligencia_re, base)
summary(fit_re, fit.measures = T, standardized = T)


# Visualizar todas las opciones para personalizar el análisis
?lavOptions

# Revisar las distribuciones de los ítems
# La función "mvn" permite realizar análisis de normalidad
# "data" variables para analizar
# "mvnTest" selecciona un test para normalidad multivariada
# "univariateTest" prueba de normalidad univariada
# "univariatePlot" gráfico de normalidad univariada
mvn(data = base[4:23], mvnTest = "mardia", univariateTest = "SW",
    univariatePlot = "histogram")


# Estimar modelo Maximum Likelihood Robust (MLR)
# "estimator" estimador a ser utilizado
fit_mlr <- cfa(inteligencia, base, estimator = "MLR")
summary(fit_mlr, fit.measures = T, standardized = T)


# Estimar modelo Weighted Least Square with Mean and Variance Adjusted (WLSMV)
# "ordered" establece la ordinalidad de los ítems
# "mimic" imita los resultados de otros software
fit_wlsmv <- cfa(inteligencia, base, estimator = "WLSMV",
                 ordered = names(base[, 4:23]), mimic = "Mplus")
summary(fit_wlsmv, fit.measures = T, standardized = T)

# Realizar un Path Diagram con la función "semPaths"
dev.off() # Elimina la configuración de gráficos anteriores

semPaths(object = fit_wlsmv, whatLabels = "std",layout = "tree",
         edge.label.cex = 1.3, rotation = 2, nCharNodes = 3, sizeLat = 7,
         sizeLat2 = 4, sizeMan = 5, sizeMan2 = 4, intercepts = F,
         thresholds = F, exoVar = F, mar = c(1,6,1,6), edge.color = "black")


# Especificación del Modelo ortogonal
# "orthogonal" establece la no covarianza entre factores
fit_wlsmv_or <- cfa(inteligencia, base, estimator = "WLSMV", orthogonal = T,
                 ordered = names(base[ , 4:23]), mimic = "Mplus")
summary(fit_wlsmv_or, fit.measures = T, standardized = T)
semPaths(object = fit_wlsmv_or, whatLabels = "std",layout = "tree",
         edge.label.cex = 1.3, rotation = 2, nCharNodes = 3, sizeLat = 7,
         sizeLat2 = 4, sizeMan = 5, sizeMan2 = 4, intercepts = F,
         thresholds = F, exoVar = F, mar = c(1,6,1,6), edge.color = "black")


# Comparación de modelos
# Modelo correlacionado y no correlacionado
# Primera forma "anova"
anova(fit_wlsmv, fit_wlsmv_or)
# Segunda forma "compareFit" del paquete [semTools]
compareFit(fit_wlsmv, fit_wlsmv_or)

# Especificación del Modelo unifactorial
inteligencia_one <- " inteligencia =~ I03 + I07 + I10 + I16 +
                                      I01 + I05 + I13 + I19 +
                                      I02 + I08 + I12 + I18 +
                                      I06 + I09 + I11 + I14 +
                                      I04 + I15 + I17 + I20 "

fit_wlsmv_one <- cfa(inteligencia_one, base, estimator = "WLSMV",
                    ordered = names(base[ , 4:23]), mimic = "Mplus")
summary(fit_wlsmv_one, fit.measures = T, standardized = T)
semPaths(object = fit_wlsmv_one, whatLabels = "std",layout = "tree",
         edge.label.cex = 1.3, rotation = 2, nCharNodes = 3, sizeLat = 7,
         sizeLat2 = 4, sizeMan = 5, sizeMan2 = 4, intercepts = F,
         thresholds = F, exoVar = F, mar = c(1,6,1,6), edge.color = "black")


# Comparación de modelos
# Modelo correlacionado y unifactorial
compareFit(fit_wlsmv, fit_wlsmv_one)


# Especificación del Modelo de segundo orden
jerarquico <- " intrapersonal   =~ I03 + I07 + I10 + I16
                interpersonal   =~ I01 + I05 + I13 + I19
                manejo_emocion  =~ I02 + I08 + I12 + I18
                adaptabilidad   =~ I06 + I09 + I11 + I14
                estado_animo    =~ I04 + I15 + I17 + I20
                inteligencia    =~ intrapersonal + interpersonal +
                                   manejo_emocion + adaptabilidad +
                                   estado_animo"

fit_jerarquico <- cfa(jerarquico, base, estimator = "WLSMV",
                 ordered = names(base[ , 4:23]), mimic = "Mplus")
summary(fit_jerarquico, fit.measures = T, standardized = T, rsquare = T)
semPaths(object = fit_jerarquico, whatLabels = "std",layout = "tree",
         edge.label.cex = 1.3, rotation = 2, nCharNodes = 3, sizeLat = 7,
         sizeLat2 = 4, sizeMan = 5, sizeMan2 = 4, intercepts = F,
         thresholds = F, exoVar = F, mar = c(1,6,1,6), edge.color = "black")

# Comparación de modelos
# Modelo correlacionado y jerárquico
compareFit(fit_wlsmv, fit_jerarquico)


# Especificación del Modelo Bifactor
bifactor <- " inteligencia =~ I03 + I07 + I10 + I16 +
                              I01 + I05 + I13 + I19 +
                              I02 + I08 + I12 + I18 +
                              I06 + I09 + I11 + I14 +
                              I04 + I15 + I17 + I20 
              intrapersonal   =~ I03 + I07 + I10 + I16
              interpersonal   =~ I01 + I05 + I13 + I19
              manejo_emocion  =~ I02 + I08 + I12 + I18
              adaptabilidad   =~ I06 + I09 + I11 + I14
              estado_animo    =~ I04 + I15 + I17 + I20 "

fit_bifactor <- cfa(bifactor, base, estimator = "WLSMV", orthogonal = T,
                      ordered = names(base[ , 4:23]), mimic = "Mplus")
summary(fit_bifactor, fit.measures = T, standardized = T, rsquare = T)
semPaths(object = fit_bifactor, bifactor = "inteligencia", whatLabels = "std",
         layout = "tree2", edge.label.cex = 1.3, rotation = 2, nCharNodes = 3,
         sizeLat = 7, sizeLat2 = 4, sizeMan = 5, sizeMan2 = 4, intercepts = F,
         thresholds = F, exoVar = F, mar = c(1,2,1,2), edge.color = "black")


# Comparación de modelos
# Modelo correlacionado y bifactor
compareFit(fit_wlsmv, fit_bifactor)



###################################################
###### Análisis de la Invarianza de Medición ######
###################################################

# Método: Análisis Factorial Confirmatorio Multigrupo
# Si no se considera la ordinalidad de los ítems
inv <- measurementInvariance(model = inteligencia, data = base, group = "SEXO")
compareFit(inv)

# Considerando la ordinalidad de los ítems
# Primera forma (recomendada)
configural <- cfa(inteligencia, base, estimator = "WLSMV",
                 ordered = names(base[, 4:23]), group = "SEXO")
metrica <- cfa(inteligencia, base, estimator = "WLSMV", group = "SEXO",
                  ordered = names(base[, 4:23]), group.equal = c("loadings"))
escalar <- cfa(inteligencia, base, estimator = "WLSMV", group = "SEXO",
               ordered = names(base[, 4:23]), group.equal = c("loadings",
                                                              "thresholds"))
estricta <- cfa(inteligencia, base, estimator = "WLSMV", group = "SEXO", 
               ordered = names(base[, 4:23]), group.equal =
                 c("loadings", "intercepts", "residuals"))
compareFit(configural, metrica, escalar, estricta)

# Segunda forma
inv <- measurementInvarianceCat(model = inteligencia,
                                data = base, group = "SEXO",
                                estimator = "WLSMV",
                                ordered = names(base[, 4:23]))
compareFit(inv)



#######################################
############ Confiabilidad ############
#######################################

# Modelo de medidas esencialmente tau-equivalentes (ítems ordinales)
tau <- " intrapersonal   =~ A*I03 + A*I07 + A*I10 + A*I16
         interpersonal   =~ B*I01 + B*I05 + B*I13 + B*I19
         manejo_emocion  =~ C*I02 + C*I08 + C*I12 + C*I18
         adaptabilidad   =~ D*I06 + D*I09 + D*I11 + D*I14
         estado_animo    =~ E*I04 + E*I15 + E*I17 + E*I20 "
fit_tau <- cfa(tau, base, estimator = "WLSMV",
                 ordered = names(base[, 4:23]), mimic = "Mplus")
summary(fit_tau, fit.measures = T, standardized = T)


# Modelo de medidas congenéricas (ítems ordinales)
cong <- " intrapersonal   =~ I03 + I07 + I10 + I16
          interpersonal   =~ I01 + I05 + I13 + I19
          manejo_emocion  =~ I02 + I08 + I12 + I18
          adaptabilidad   =~ I06 + I09 + I11 + I14
          estado_animo    =~ I04 + I15 + I17 + I20 "
fit_cong <- cfa(cong, base, estimator = "WLSMV",
               ordered = names(base[, 4:23]), mimic = "Mplus")
summary(fit_cong, fit.measures = T, standardized = T)

# Comparación de modelos
# Esencialmente tau-equivalentes y Congenéricas
compareFit(fit_tau, fit_cong)


#Estimación puntual de los coeficientes alfa y omega
fiabilidad <- reliability(fit_cong)
# Redondear a dos decimales
round(fiabilidad, 2)





###############################
############ BONUS ############
###############################

# Análisis Factorial Confirmatorio con 1 ítem
# El error del ítem está compuesto por:
# [1 - confiabilidad(item)] * varianza(item)

# Supongamos que el I01 es el único ítem del EQ-i-M20
# Considerando que tiene una fiabilidad de 0.74
# Varianza del ítem 01 (0.4912601)
var(base$I01)

# Modelo del Ítem único
unico <- " inteligencia =~ I01
           I01 ~~ (0.26*0.4912601)*I01"
fit_unico <- cfa(unico, base, estimator = "WLSMV", mimic = "Mplus")
summary(fit_unico, fit.measures = T, standardized = T)
