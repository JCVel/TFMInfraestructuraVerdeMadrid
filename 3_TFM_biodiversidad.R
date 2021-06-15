################################################################################
# Archivo: 3_TFM_biodiversidad.R
# Autor: Juan Carlos VelAzquez Melero 
# Fecha: 20/04/2021
# DescripciOn: 
# En el marco del TFM del MTIG de la UAH, este script lee una capa rAster que
# contiene un Indice de Biodiversidad a 1km x 1km
# Datos de entrada: 
#   - Capa con el Indice de Biodiversidad (Proyecto del MITECO: IdentificaciOn 
#     de Areas a desfragmentar para reducir los impactos de las infraestructuras 
#     lineales de transporte en la biodiversidad)
#   - Capa con el primer indicador (contribuciOn a los SE) para usar como referencia
#   
# Datos de salida:
#   - Capa del indicador de biodiversidad recortada acorde al Area de estudio y
#     remuestreada a 25x25m
# 
# NOTA: (1) Las tildes han sido sustituidas por mayUsculas
################################################################################
# InstalaciOn y carga de las librerias necesarias
# install.packages("raster")
library(raster)

################################################################################
##################____CARPETAS DE ENTRADA Y SALIDA_____#########################
################################################################################

# Introduce aquI la ruta donde estA la capa rAster con el Indice de biodiversidad 
# del proyecto de fragmentaciOn del MITERD
pathIndBiodiv <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/0_datos_brutos_entrada/fragmentacion_miterd/fragm-capas-resultados-estudio_tcm30-195799/Capas de resultados del estudio/Capas usadas para obtener el índice de vulnerabilidad/ind_biodiv.tif"
# Introduce aquI la ruta donde estE el rAster del primer indicador: 
# "t_poligonosSE_raster.tif" para recortar la capa con el Indice de biodiversidad 
pathReferenciaRaster <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/1_SE/1_valoracionSE/exportacionesValoracionSE/t_poligonosSE_raster.tif"

#  Carpeta de salida
# Introduce aquI un directorio en el que guardar el producto de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/3_biodiversidad"
# Nombre de la carpeta con las exportaciones, dejarlo asI
newFolderName <- "exportacionesIndBiodiv" 
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete, donde se guardarAn las tablas
dir.create(exportPathComplete)

################################################
#PROCESADO DE LA CAPA DE INDICE DE BIODIVERSIDAD
################################################

# Se lee la capa del Indice de Biodiv y la capa de referencia del indicador de SE
indBiodiv <- raster(pathIndBiodiv)
referenciaRaster <- raster(pathReferenciaRaster)

# Se remuestrea la capa de menor resoluciOn en funciOn de la de mayor resoluciOn (capa de referencia)
# Además, se queda con el extent de la capa de referencia (extent que corresponde a Madrid)
indBiodivResampMad = resample(indBiodiv, referenciaRaster, "bilinear")

# Se eliminan los "pIxeles sobrantes" del extent usando la capa de referencia como mAscara.
indBiodivResampMad = mask(indBiodivResampMad, referenciaRaster)

# FunciOn para normalizar valores de un raster entre 0 y 1
normaliza01 <- function(x) { # Necesita que estE cargado el paquete "raster"
  return ((x - minValue(x)) / (maxValue(x) - minValue(x)))
}

# Se aplica la funciOn al raster con el ind. de biodiv. de Madrid a 25x25m
indBiodivResampMad <- normaliza01(indBiodivResampMad)

# Se exporta el raster anterior
pathRasterIndBiodiv <- file.path(exportPathComplete, "indicadorBiodiv.tif")
writeRaster(indBiodivResampMad, pathRasterIndBiodiv, overwrite = T)


