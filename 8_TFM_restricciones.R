################################################################################
# Archivo: 8_TFM_restricciones.R
# Autor: Juan Carlos VelAzquez Melero 
# Fecha: marzo - junio 2021
# DescripciOn: 
# En el marco del TFM del MTIG de la UAH, este script selecciona unos usos del
# suelo determinados del SIOSE 2014 para crear una capa de restricciones 
# correspondiente a la Infraestructura Gris
# 
# Datos de entrada: Geodatabase del SIOSE 2014, de la que se obtiene la capa
# "T_POLIGONOS" y la tabla "TC_SIOSE_CODIIGE"
# 
# Datos de salida: capa de restricciones en shapefile e imagen tif de restricciones
# 
################################################################################
rm(list = ls(all = TRUE)) # Eliminar objetos previos, si los hubiera
# InstalaciOn de paquetes tomada de Antoine Soetewey (https://statsandr.com)
packages <- c("sf", "raster", "fasterize")
# IntalaciOn de paquetes
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Carga de paquetes
invisible(lapply(packages, library, character.only = TRUE))

################################################################################
##################____CARPETAS DE ENTRADA Y SALIDA_____#########################
################################################################################
# Introduce aquI la carpeta donde estA la contenida la GDB con SIOSE 2014 Madrid
setwd("D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/0_datos_brutos_entrada/siose2014/SIOSE_Madrid_2014_GDB")

# Introduce aquI un directorio en el que guardar las tablas de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/8_restricciones"
# Nombre de la carpeta con las exportaciones, dejarlo asI
newFolderName <- c("exportacionesRestricciones") # Nombre de la carpeta con las exportaciones
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete, donde se guardarAn las tablas
dir.create(exportPathComplete)

################################################################################
##############____CREACIoN DE LA CAPA DE RESTRICCIONES_____#####################
################################################################################
# Abrir capa de polIgonos de SIOSE
siose <- st_read(dsn = "SIOSE_Madrid_2014.gdb", layer = "T_POLIGONOS")
# Abrir tabla de datos "TC_SIOSE_CODIIGE" 
codiige <- st_read(dsn = "SIOSE_Madrid_2014.gdb", layer = "TC_SIOSE_CODIIGE")

# Vector con las clases de tipo "restricciOn"
restricCod <- c(111, 112, 113, 121, 123, 130, 140, 161, 162, 163, 171, 172)
# Se crea un df con las restricciones
restric <- data.frame(restricCod, rep(c("restric"), length(restricCod)))
colnames(restric) <- c("CODIIGE", "restric")

# Unir las restricciones a los polIgonos del SIOSE
siose <- merge(siose, restric, by = "CODIIGE", all.x = T)
siose[is.na(siose)] <- "no_restric" # Quitar los NAs

siose$valRestr <- ifelse(siose$restric == "restric", 0, 1)
# Seleccionar solo los polIgonos que son restricciones
siose_restric <- siose[siose$restric == "restric", ]
# Unir la leyenda del CODIIGE
siose_restric <- merge(siose_restric, codiige, by = "CODIIGE", all.x = T)

# Se exporta el shapefile
path_restric <- file.path(exportPathComplete, "siose_restric.shp")
st_write(siose_restric, path_restric, append = F)

# Rasterizar la capa anterior y exportarla
# Se crea un rAster vacIo con la extensiOn de t_poligonos y tamaNo de pIxel = 25
siose_raster <- raster(siose, res = 25)
# Se vuelca la info de siose_restric (vectorial) al raster anterior
siose_restric_raster <- fasterize(siose, siose_raster, field = "valRestr", fun="sum")
# Se exporta el raster de restricciones
pathRaster <- file.path(exportPathComplete, "siose_restric.tif")
writeRaster(siose_restric_raster, pathRaster, overwrite = T)
