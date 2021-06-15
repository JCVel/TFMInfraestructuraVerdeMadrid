################################################################################
# Archivo: 1_TFM_valoracionSE.R
# Autor: Juan Carlos VelAzquez Melero 
# Fecha: marzo - junio 2021
# DescripciOn: 
# En el marco del TFM del MTIG de la UAH, este script genera un mapa en formato
# vectorial y rAster de la contribuciOn de los usos del suelo a los Servicios 
# EcosistEmicos (SE).
# Datos de entrada: 
#   - tabla con las coberturas "descendientes" de la tabla T_Valores
#   "t_valores_coberturasClasificadas.csv" (ver script extraccionCoberturasDescendSIOSE14.R,
#   https://github.com/JCVel/extraccionCoberturasDescendSIOSE)
#   - tabla que contiene la contribuciOn de cada cobertura de SIOSE14 Madrid a los SE
#   - GDB de SIOSE 2014 Madrid
# Datos de salida:
#   - capa vectorial del SIOSE 2014 Madrid con el campo "valorPonder" que contiene la
#   contribuciOn de dicho polIgono a los SE
#   - capa rAster 25x25m de la capa anterior en base al campo "valorPonder"

# NOTA: Este script solo ha sido testeado para trabajar con datos SIOSE14 Madrid
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
# Carga de la tabla con las coberturas descendientes de la tabla T_Valores
t_valores_coberClasif <- read.csv("D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/0_extraccionCoberturasDescendSIOSE14/exportacionesCoberturasDescendSIOSE14/t_valores_coberturasClasificadas.csv")
# Carga de la tabla que contiene la contribuciOn de cada cobertura de SIOSE14 Madrid a los SE
coberConValSE <- read.csv("D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/1_SE/1_valoracionSE/coberturasDescendientesUnicasConValorSE.csv")
# Introduce aquI la ruta donde estE la GDB de SIOSE 2014 Madrid
pathSIOSE <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/0_datos_brutos_entrada/siose2014/SIOSE_Madrid_2014_GDB"

# Introduce aquI un directorio en el que guardar los productos de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/1_SE/1_valoracionSE"
# Nombre de la carpeta con las exportaciones, dejarlo asI
newFolderName <- c("exportacionesValoracionSE") # Nombre de la carpeta con las exportaciones
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete, donde se guardarAn las tablas
dir.create(exportPathComplete)

################################################################################
#####################____PROCESADO DE DATOS_____################################
################################################################################
# Se reduce el df a las dos columnas necesarias para realizar la uniOn
coberConValSE <- coberConValSE[, c("ID_COBERTURAS", "valorSE")]

#  Se dejan solo las coberturas descendientes del df t_valores_coberClasif
t_valores_coberClasif <- t_valores_coberClasif[t_valores_coberClasif$INTER_ID_tipo == "descendiente", ]
# UniOn entre t_valores_coberClasif y la tabla con la contribuciOn de cada uso del suelo a los SE
t_valores_coberClasifConValSE <- merge(t_valores_coberClasif, coberConValSE, by = "ID_COBERTURAS",
                             all.x = T)

# FunciOn para normalizar variables
normaliza <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Se aplica la funciOn a la variable que contiene los valores de SE
t_valores_coberClasifConValSE$valorSE <- normaliza(t_valores_coberClasifConValSE$valorSE)

# Reordenar campos del df t_valores
ordenColumnas <- c("ID_POLYGON", "ID_COBERTURAS", "DESCRIPCION_COBERTURAS", 
                   "valorSE", "SUPERF_POR")
t_valores_coberClasifConValSE <- t_valores_coberClasifConValSE[,ordenColumnas]       
t_valores_coberClasifConValSE <- t_valores_coberClasifConValSE[order(t_valores_coberClasifConValSE$ID_POLYGON),]

t_valores_coberClasifConValSE$valorPonder <- t_valores_coberClasifConValSE$valorSE * t_valores_coberClasifConValSE$SUPERF_POR * 0.01

# Crea df en el que  se suman los valores ponderados y normalizados
# de los SE por cada polígono
poligValorSE <- aggregate(valorPonder ~ ID_POLYGON, t_valores_coberClasifConValSE, sum)
summary(poligValorSE$valorPonder)

# Se lee la capa de polIgonos de SIOSE para exportarla con la informaciOn 
# anterior en su tabla de atributos
pathSIOSEComplete <- file.path(pathSIOSE, "SIOSE_Madrid_2014.gdb" )
t_poligonos <- st_read(dsn = pathSIOSEComplete, layer = "T_POLIGONOS")
t_poligonosSE <- merge(t_poligonos, poligValorSE, by = "ID_POLYGON")
pathT_poligonosSE <- file.path(exportPathComplete, "T_POLIGONOS_SE.shp")
st_write(t_poligonosSE, pathT_poligonosSE, append = F)

# Rasterizar la capa anterior y exportarla
# Se crea un rAster vacIo con la extensiOn de t_poligonosSE y tamaNo de pIxel = 25
t_poligonosSE_raster <- raster(t_poligonosSE, res = 25)
# Se vuelca la info de t_poligonosSE (vectorial) al raster anterior
t_poligonosSE_raster <- fasterize(t_poligonosSE, t_poligonosSE_raster, field = "valorPonder", fun="sum")

# Se exporta el raster con la contribuciOn de los usos del suelo a los SE
pathRaster <- file.path(exportPathComplete, "t_poligonosSE.tif")
writeRaster(t_poligonosSE_raster, pathRaster, overwrite = T)

# ExportaciOn de la tabla t_valores_coberClasifConValSE
path_t_valores_coberClasifConValSE <- file.path(exportPathComplete, "t_valores_coberClasifConValSE.csv")
write.table(t_valores_coberClasifConValSE, path_t_valores_coberClasifConValSE, row.names = F, sep = ",")
