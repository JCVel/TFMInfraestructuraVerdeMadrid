################################################################################
# Archivo: 9_TFM_integracionIndicadores.R
# Autor: Juan Carlos VelAzquez Melero 
# Fecha: marzo - junio 2021
# DescripciOn: 
# En el marco del TFM del MTIG de la UAH, este script lee un conjunto de indicadores
# que forman parte de la Infraestructura Verde (IV) y los integra mediante suma,
# suma ponderada, multiplicaciones etc... Criterio a determinar por el productor.
# 
# Datos de entrada: los indicadores en formato raster (misma resoluciOn y extent)
# 
# Datos de salida: el fichero rAster que representarIa la IV (IV.tif)

################################################################################
rm(list = ls(all = TRUE)) # Eliminar objetos previos, si los hubiera

# install.packages("raster")
library(raster)

################################################################################
##################____CARPETAS DE ENTRADA Y SALIDA_____#########################
################################################################################

# Directorio del indicador 1: contribuciOn a los SE
pathInd1 <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/1_SE/1_valoracionSE/exportacionesValoracionSE/t_poligonosSE_raster.tif"
# Directorio del indicador 2: Accesibilidad
pathInd2 <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/4_accesibilidad/varCost_JCV/accesibilidadVarCostJCV_clipped.tif"
# Directorio del indicador 3: indice de biodiversidad
pathInd3 <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/3_biodiversidad/exportacionesIndBiodiv/indicadorBiodiv.tif"
# Directorio de las restricciones
restr <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/8_restricciones/exportacionesRestricciones/siose_restric.tif"

#  Carpeta de salida
# Introduce aquI un directorio en el que guardar el producto de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/9_integracionIndicadores"
# Nombre de la carpeta con las exportaciones, dejarlo asI
newFolderName <- "exportacionesIntegraIndic" 
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete, donde se guardarAn las tablas
dir.create(exportPathComplete)

################################################################################
# PONDERACIoN DE LOS INDICADORES
################################################################################
# FunciOn para normalizar valores de un raster entre 0 y 1
normaliza01 <- function(x) { # Necesita que estE cargado el paquete "raster"
  return ((x - minValue(x)) / (maxValue(x) - minValue(x)))
}

# Se leen las capas de entrada
ind1 <- raster(pathInd1)
ind2 <- raster(pathInd2)
ind3 <- raster(pathInd3)
restr <- raster(restr)

# Se invierte y normaliza el indicador 2 (accesiblidad)
ind2 <- raster.invert(ind2)
ind2 <- normaliza01(ind2)
# Se exporta el indicador de accesibilidad invertido y normalizado 
# exportPathCompleteAcess <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/4_accesibilidad/varCost_JCV"
# path_access <- file.path(exportPathCompleteAcess, "accesibilidadVarCostJCVClippedNormInvert.tif")
# writeRaster(ind2, path_access, overwrite = T)

# Se genera la capa de Infraestructura Verde
IV <- (ind1 + (ind2 * 0.5) + ind3)  * restr

# Se normalizan los valores entre 0 y 1 para la capa de iv
IV <- normaliza01(IV)

# Se crea otra capa en la que PIxeles = 0 se cambian a NoData (restricciones)
IV_NA <- IV
IV_NA[IV_NA == 0] <- NA 

################################################################################
# Se exportan las capas anteriores
################################################################################
path_IV <- file.path(exportPathComplete, "IV_05acc_restr0.tif")
writeRaster(IV, path_IV, overwrite = T)

path_IV_NA <- file.path(exportPathComplete, "IV_05acc_restrNA.tif")
writeRaster(IV_NA, path_IV_NA, overwrite = T)

