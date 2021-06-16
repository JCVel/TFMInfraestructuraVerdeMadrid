################################################################################
# Archivo: 9_TFM_analisisDatos.R
# Autor: Juan Carlos VelAzquez Melero 
# Fecha: marzo - junio 2021
# DescripciOn: 
# En el marco del TFM del MTIG de la UAH, este script realiza 2 tipos de anAlisis:
#   (1) AnAlisis entre indicadores de IV, el Indice de valoraciOn de la IV, 
#       coordenadas x e y, y distancia al Sistema Central
#   (2) AnAlisis entre la valoraciOn de la IV e indicadores socioeconOmicos
# 
# Datos de entrada:
#   - Indicador de Servicios EcosistEmicos (SE)
#   - Indicador Indice Combinado de Biodiversidad (ICB)
#   - Indicador de Accesibilidad (Acc)
#   - Indice de valoraciOn de la Infraestructura Verde (IV)
#   - Shapefile del Eje del Sistema Central (SC)
#   - Shapefile de municipios y datos socioeconOmicos (generado en "5_TFM_socioeconom.R")
#   - Shapefile con los datos de IV por municipio (generado con el ModelBuilder)
#   - Shapefile con los datos de IV por municipio y APs (generado con el ModelBuilder)
# 
# Datos de salida:  
#   - Se crean automAticamente dos directorios:
#       - "exportacionesAnalisisDatosIndic" y "exportacionesAnalisisDatosSocioEcon"
#   - Ambos contienen:
#     - Graficos de correlaciones (jpg) para cada combinaciOn de variables
#     - Matriz de correlaciones con * en funciOn de la significaciOn estadIstica
# 
################################################################################

rm(list = ls(all = TRUE)) # Eliminar objetos previos, si los hubiera
# InstalaciOn de paquetes tomada de Antoine Soetewey (https://statsandr.com)
packages <- c("sf", "ggplot2", "dplyr", "ggpubr", "PerformanceAnalytics", "tidyverse",
              "xtable", "Hmisc", "raster")
# IntalaciOn de paquetes
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Carga de paquetes
invisible(lapply(packages, library, character.only = TRUE))

################################################################################
# Modificado de la funciOn char.Correlation del paquete PerformanceAnalytics
# para que los valores de r tengan un tamaNo estandar
chart.CorrelationMod <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), ...){
  x = checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  cormeth <- method
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = cormeth, cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = 1.2)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor)
}

#____ Generar una matriz de correlaciOn con asteriscos 
# Se copia la funciOn corstars de Dominik Vogel
corstars <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
################################################################################

################################################################################
# ANaLISIS DATOS INDICADORES
################################################################################
#____ Carpeta de salida
# Directorio en el que guardar el producto de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/99_analisisDatos"
# Nombre de la carpeta con las exportaciones
newFolderName <- "exportacionesAnalisisDatosIndic"
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete
dir.create(exportPathComplete)

#____Rutas de los archivos a leer
# Ruta del indicador de Servicios EcosistEmicos (SE) sin Infra Gris
pathContSE <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/8_restricciones/export_IndicConRestric/ind1_restr.tif"
# Ruta del indicador Indice Combinado de Biodiversidad (ICB) sin Infra Gris
pathICB <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/8_restricciones/export_IndicConRestric/ind3_restr.tif"
# Ruta del indicador de Accesibilidad (Acc) sin Infra Gris
pathAcc <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/8_restricciones/export_IndicConRestric/ind2_restr.tif"
# Ruta del Indice de valoraciOn de la Infraestructura Verde (IV) sin Infra Gris
pathIV <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/9_integracionIndicadores/exportacionesIntegraIndic/IV_05acc_restr0.tif"
# Ruta del eje del Sistema Central (SC)
pathSC <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/999_capasBase/4_ejes/ejeSC2.shp"

#____Lectura de los archivos
ContSE <- raster(pathContSE)
ICB <- raster(pathICB)
Acc <- raster(pathAcc)
IV <- raster(pathIV)
SC <- st_read(pathSC)

#____ObtenciOn de datos
# ObtenciOn de las coordenadas xy de todas las celdas
rasterCoords <- as.data.frame(coordinates(IV))

# SelecciOn de un subconjunto de observaciones (del total aprox. 29E6)
set.seed(30)
observaciones <- 10000000
rasterCoords <- rasterCoords[sample(nrow(rasterCoords), observaciones), ]

# Se extraen los datos de cada raster en cada punto en un df
elementos <- c(IV, ContSE, ICB, Acc)
elemNombre <- c("IV", "ContSE", "ICB", "Acc")
datos <- as.data.frame(rep(NA, observaciones))
colnames(datos)[1] <- "vacio"
contador <- 0

for (elem in elementos){
  contador <- contador + 1
  elem_datos <- cbind(raster::extract(elem, rasterCoords, df = T), rasterCoords)
  colnames(elem_datos)[2] <- elemNombre[contador]
  datos <- cbind(datos, elem_datos)
}

# Se seleccionan los campos de interEs y eliminaciOn de registros con NAs
datos <- datos[, c("ID", "x", "y", "IV", "ContSE", "ICB", "Acc" )]
datos <- na.omit(datos)

# Se aNade un campo que contiene las distancias de cada punto al Sistema Central
puntos <- st_as_sf(datos, coords = c("x", "y"), 
                   agr = "constant", crs = 25830)
datosD <- cbind(st_distance(puntos, SC), datos)
colnames(datosD)[1] <- "distSC"
datosD$distSC <- as.numeric(datosD$distSC)/1000 # correcciOn campo distancias y cambio a km
datosD$x <- as.numeric(datosD$x)/1000 # cambio de metros a km
datosD$y <- as.numeric(datosD$y)/1000 # cambio de metros a km


#____ ElecciOn y ordenaciOn de las variables
datosD <- datosD[ , c("IV", "ContSE", "ICB", "Acc", "distSC", "x", "y")]
datosD1000 <- datosD[sample(nrow(datosD), 1000), ] # CreaciOn de un subset con solo 1000 observaciones

#____ Generar un plot con el resumen de datos
chart.CorrelationMod(datosD1000, histogram = TRUE, pch = 19)

#____ Generar una matriz de correlaciOn con asteriscos 
datosDMillon <- datosD[sample(nrow(datosD), 1000000), ]
matrizCorrelac <- corstars(datosDMillon)
matrizCorrelac

exportPathCompleteCorrInd <- file.path(exportPathComplete, "matrizCorrelacionIndMod2.csv")
write.table(matrizCorrelac, exportPathCompleteCorrInd, sep = ";",
            col.names = NA)

#____ Generar grAficas independientes de correlaciones
# Vector sobre el que iterar la generaciOn de grAficas
variables <- c("ContSE", "ICB", "Acc", "distSC", "x", "y")
nombresVars <- c("Contribución a los Servicios Ecosistémicos",
                 "Índice combinado de biodiversidad", "Accesibilidad por la red de caminos",
                 "Distancia al Sistema Central", "Coordenada x", "Coordenada y")
varDependiente <- "IV"
varDependienteNombre <- "Valoración Infraestructura Verde"
setwd(exportPathComplete)

for (i in 1:length(variables)){

  plot <- ggscatter(datosD1000, x = variables[i], y = varDependiente,
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson",
              xlab = nombresVars[i], ylab = varDependienteNombre,
              ylim = c(0, 1), size = 0.5) +
      font("xlab", size = 10) +
      font("ylab", size = 10) +
      font("xy.text", size = 10)

  nombreSalida <- paste0(variables[i], ".png")
  ggsave(nombreSalida, plot = plot, width = 8, height = 6, units = "cm",
             dpi = 300)
  }

################################################################################
# ANaLISIS DATOS IV E INDICADORES SOCIOECONoMICOS
################################################################################
#____ Carpeta de salida
# Directorio en el que guardar el producto de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/99_analisisDatos"
# Nombre de la carpeta con las exportaciones, dejarlo asI
newFolderName <- "exportacionesAnalisisDatosSocioEcon"
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete, donde se guardarAn las tablas
dir.create(exportPathComplete)
setwd(exportPathComplete)

#____ Directorios de los datos de entrada
# Ruta del shp de municipios y datos socioeconOmicos
pathMuniSocioEcon <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/5_socioeconom/exportacionesSocioeconom/muniSocioEcon.shp"
# Ruta de los datos de IV por municipio
pathMuniIV <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/9_integracionIndicadores/3_valoracionIVMunicipios/separaEntreApples_Municipios_valIV_0/muniConDatos.shp"
# Ruta de los datos de IV por municipio y APs
pathMuniDFIV <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/9_integracionIndicadores/3_valoracionIVMunicipios/separaEntreApples_Municipios_valIV_0/muniDFConDatos.shp"

#____ Lectura de datos
# Se leen los datos socioeconOmicos por municipio
muniSocioEcon <- st_read(pathMuniSocioEcon)
# Se leen los datos de IV por municipio
muniIV <- st_read(pathMuniIV)
# Se leen los datos de IV por municipio y Dentro/Fuera de las Areas Principales
muniDFIV <- st_read(pathMuniDFIV)

#____ ManipulaciOn de datos
# Se une la informaciOn de muniIV a muniSocioEcon
muniIV_df <- as.data.frame(muniIV)
muniIV_df[ , c("geometry")] <- NULL # Se elimina la columna "geometry"
muniSocioEcon_df <- as.data.frame(muniSocioEcon)
muniSocioEcon_df[ , c("geometry")] <- NULL

muniIVSocioEcon <- merge(muniSocioEcon_df, muniIV_df, by = "NATCODE", all.x = T)

#____ GeneraciOn de grAficas
# Vector sobre el que iterar la generaciOn de grAficas
variables <- c("tasaPob", "dens19", "renta18", "tasaRnt", "slPrc17", "slDfPrc")
nombresVars <- c("Tasa de crecimiento poblacional","Densidad de población 2019 (habitantes/km2)",
                 "Renta bruta per cápita 2018 (euros/año)", "Tasa variación renta 2005-2018",
                 "Porcentaje de suelo protegido en 2017", "Variación del % de suelo protegido 2005-2017")
varDependiente <- "MEAN"
varDependienteNombre <- "Valoración Infraestructura Verde"


for (i in 1:length(variables)){
  plot <- ggscatter(muniIVSocioEcon, x = variables[i], y = varDependiente,
                    color = "#011816", shape = 20, size = 1,
                    add = "reg.line",
                    # add.params = list(color = "grey", fill = "lightgray"),
                    conf.int = TRUE,
                    cor.coef = TRUE, 
                    cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = 1),
                    xlab = nombresVars[i], ylab = varDependienteNombre,
                    ylim = c(0, 1)) +
    font("xlab", size = 9) +
    font("ylab", size = 9) +
    font("xy.text", size = 9)
                    
  
  nombreSalida <- paste0(variables[i], ".png")
  ggsave(nombreSalida, plot = plot, width = 8, height = 6, units = "cm",
         dpi = 300)
}


# GrAfico IV vs tasa crecimiento poblacional quitando el outlier de Arroyomolinos
muniIVSocioEconMod <- muniIVSocioEcon[!muniIVSocioEcon$NAMEUNIT_x == "Arroyomolinos", ]
ggscatter(muniIVSocioEconMod, x = "tasaPob", y = "MEAN",  # Datos
          color = "#011816", shape = 20, size = 2,
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE,
          cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", label.x.npc = "left", label.y.npc = 1),
          xlab = "Tasa de crecimiento poblacional", ylab = "Valoración Infraestructura Verde",
          ylim = c(0, 1)) +
          font("xlab", size = 15) +
          font("ylab", size = 15) +
          font("xy.text", size = 15) +

 ggsave("IV_vs_tasaCrecPobSinOutlier.png", width = 20, height = 20, units = "cm",
       dpi = 300)

################################################################################
#_____ AnAlisis de correlaciOn entre variables
# SelecciOn de variables
datosCorr <- muniIVSocioEcon[ , c("MEAN", "tasaPob", "dens19", "renta18", "tasaRnt", "slPrc17", "slDfPrc")]
# Visionado general
chart.CorrelationMod(datosCorr, histogram=TRUE, pch=19)
# Matriz de correlaciones
matrizCorrelac <- corstars(datosCorr)
matrizCorrelac
# ExportaciOn de la matriz
write.table(matrizCorrelac, "matrizCorrelacionIVSocioEcon.csv", sep = ";",
          col.names = NA)
