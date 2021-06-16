################################################################################
# Archivo: 5_TFM_socioeconom
# Autor: Juan Carlos VelAzquez Melero 
# Fecha: marzo - junio 2021
# DescripciOn: 
# En el marco del TFM del MTIG de la UAH, este script calcula un conjunto de 
# indicadores socioeconOmicos a partir de datos de la base de datos "Almudena"
# Los datos estAn desagregados por municipios.
# Datos entrada:
#   - Municipios de LIneas lImite IGN: "recintos_municipales_inspire_peninbal_etrs89"
#   - Descargas de la BBDD Almudena
#     - "poblacion_referencia.xls" 
#     - "renta_disponible_bruta_percapita.xls"
#     - "suelo_no_urbanizable_protegido.xls"

# Datos de salida:
#   - shapefile de municipios modificado en la que se fusionan dos registros por
#   ser territorios mancomunados (IGN tiene 180 municipios y BD Almudena 179)
#   - shapefile de municipios fusionados (179) con los datos de los indicadores
#   
################################################################################
# rm(list = ls(all = TRUE)) # Eliminar objetos previos, si los hubiera
options(scipen = 0) # Desactiva notaciOn cientIfica

# InstalaciOn de paquetes tomada de Antoine Soetewey (https://statsandr.com)
packages <- c("sf", "ggplot2", "dplyr", "rgeos", "readxl")
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
# Introduce aquI la ruta donde estAn los ficheros excel descargados del banco de datos Almudena
pathDatos <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/0_datos_brutos_entrada/datosBDAlmudena"
# Introduce aquI la ruta donde estAn las lIneas lImite del IGN
pathMuni <- "D:/0_cartografia/lineas_limite/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89"

# Carpeta de salida
# Introduce aquI un directorio en el que guardar el producto de salida
exportPath <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/5_socioeconom"
# Nombre de la carpeta con las exportaciones, dejarlo asI
newFolderName <- "exportacionesSocioeconom"
# Path completo para crear la nueva carpeta con las exportaciones
exportPathComplete <- file.path(exportPath, newFolderName)
# Crear carpeta en el exportPathComplete, donde se guardarAn las tablas
dir.create(exportPathComplete)

################################################################################
############____ADAPTACIoN MUNICIPIOS LiNEAS LiMITE IGN_____####################
################################################################################

# Se leen los municipios de EspaNA
muni <- st_read(pathMuni)
# Se seleccionan solo los de Madrid
muni <- muni[muni$CODNUT3 == "ES300", ]
# Se pasa de coord.geogrAficas a proyectadas (EPSG:25830)
muni <- st_transform(muni, crs = 25830)

# Hay dos municipios que no estAn en la BD de Almudena: Los BaldIos (NATCODE =
# = 34132853057) que pertenece a Cercedilla (34132828038) y El Redeguelo (34132853058)
# que se asigna a El Boalo (34132828023)
baldios <- muni[muni$NATCODE == "34132853057", ] # Se guarda el polIgono 
redeguelo <- muni[muni$NATCODE == "34132853058", ] # Se guarda el polIgono
# Se le da a Los BaldIos el mismo cOdigo que a Cercedilla
muni[muni$NATCODE == "34132853057", "NATCODE"] <- "34132828038"
# Se le da a El Redeguelo el mismo cOdigo que a El Boalo
muni[muni$NATCODE == "34132853058", "NATCODE"] <- "34132828023"

# Se transforma de objeto sf a SPDF para usar la funciOn gUnaryUnion que hace 
# un "dissolve" en funciOn de un campo (en este caso NATCODE)
muni <- as(muni, Class = "Spatial")
muniFusion<- gUnaryUnion(muni, id = muni@data$NATCODE)
# Se pierde la BD asociada excepto el ID de cada polIgono, que se corresponde con 
# el cOdigo NATCODE. Se sacan los IDs a un df y despuEs se aNaden al objeto espacial
NATCODEs <- data.frame(NATCODE = sapply(slot(muniFusion, "polygons"), function(x) slot(x, "ID")))
rownames(NATCODEs)  <- NATCODEs$NATCODE
muniFusion <- SpatialPolygonsDataFrame(muniFusion, NATCODEs)
# Se vuelve a transformar a objeto sf
muni <- st_as_sf(muni)
muniFusion <- st_as_sf(muniFusion)

# # VisualizaciOn de la fusiOn de municipios
# muniPlot <- ggplot() +
#   geom_sf(data = muni) +
#   ggtitle("Baldios y Redeguelo sin fusionar") + 
#   theme(plot.title = element_text(size = 10)) + 
#   geom_sf(data = baldios, color = "red") +
#   geom_sf(data = redeguelo, color = "red")
# 
# muniFusionPlot <- ggplot() +
#   geom_sf(data = muniFusion) +
#   ggtitle("Baldios y Redeguelo fusionados") + 
#   theme(plot.title = element_text(size = 10))
# 
# grid.arrange(muniPlot, muniFusionPlot, nrow = 1)

rm(baldios, redeguelo, NATCODEs)
# Se lee el excel que contiene las correspondencias entre los municipios de las 
# lIneas lImite del IGN y los nombres de municipios de la BD 
pathCorresp <- "D:/Drive/JC/2_Academico/2_Masters/MTIG/TFM/1_procesado_datos/5_socioeconom/muniMadrid_deLineasLimite_tablaAtributos_mod.xls"
corresp <- read_excel(pathCorresp, sheet = "correspLinLimAlmu")
# Se vuelcan las correspondencias a la capa de municipios fusionados
muniFusion <- merge(muniFusion, corresp, by.x = "NATCODE", by.y = "NATCODE", all.x = T)
#  Se aNade un campo con el Area
muniFusion$km2 <- as.numeric(st_area(muniFusion)) / 1000000


################################################################################
##############____CaLCULO INDICADORES SOCIOECONÓMICOS_____######################
################################################################################
# Se crea un df que contenga todos los indicadores por municipio partiendo de
# la capa de municipios fusionados anterior
socioEcon <- as.data.frame(muniFusion) 
socioEcon$geometry <- NULL # Se quita el campo geometrIa para que vaya mAs rApido

###############################
###___1___PoblaciOn, densidad pob. y sus variaciones 1996-2019
###############################

# Se lee el excel que contiene los datos de poblaciON
pathPob <- file.path(pathDatos, "poblacion_referencia.xls")
pob <- read_excel(pathPob, skip = 10, col_names = T)
pob <- pob[-c(1:14, 194:195),] # Se eliminan los 14 primeros registros y los dos Ultimos, que no son municipios

# Se introduce el dato de Area de cada municipio
pob <- merge(pob, socioEcon, by.x = "Serie", by.y = "almuTxt", all.x = T)

# CorrecciOn del error de dato de poblaciOn en Collado Mediano en 1996
pob[pob$NATCODE == "34132828046", "1996"] <- pob[pob$NATCODE == "34132828046", "1996"]/10
# Se calculan los indicadores de poblaciOn
pob$pob96 <- pob$"1996"
pob$pob19 <- pob$"2019"
pob$pobDifPorcen <- ((pob$pob19 * 100) / pob$pob96) - 100 # Dif. de pob. en porcentaje
pob$tasaPob <- ((pob$pob19 - pob$pob96) / (pob$pob96 * 23 )) * 100 # Tasa de incremento poblacional
pob$dens96 <- pob$pob96 / pob$km2
pob$dens19 <- pob$pob19 / pob$km2
pob$densDifPorcen <- ((pob$dens19 * 100) / pob$dens96) - 100 # Dif. de dens. pob. en porcentaje

# Se seleccionan unicamente los campos de interEs
campos <- c("NATCODE", "NAMEUNIT", "pob96", "pob19", "pobDifPorcen", "tasaPob", "dens96",
            "dens19", "densDifPorcen")
pob <- pob[ , campos]

#  Se traspasan estos datos al df con la inform. de los indicadores socioeconom
socioEcon <- merge(socioEcon, pob, by = "NATCODE", all.x = T)

###############################
###___2___Renta disponible bruta per cApita
###############################
# Se lee el excel que contiene los datos de renta
pathRenta <- file.path(pathDatos, "renta_disponible_bruta_percapita.xls")
renta <- read_excel(pathRenta, skip = 10, col_names = T)
renta <- renta[-c(1:14, 194:199),] # Se eliminan los 14 primeros registros y los dos Ultimos, que no son municipios

# Se calculan los indicadores de renta
renta$renta00 <- renta$"2000"
renta$renta18 <- renta$"2018(A)"
renta$rentaDifPorcen <- ((renta$renta18 * 100) / renta$renta00) - 100 # Dif. de renta. en porcentaje
renta$tasaRenta <- ((renta$renta18 - renta$renta00) / (renta$renta00 * 18 )) * 100 # Tasa de incremento de renta

# Se seleccionan unicamente los campos de interEs
campos <- c("Serie", "renta00", "renta18", "rentaDifPorcen", "tasaRenta")
renta <- renta[ , campos]

# Se traspasan estos datos al df con la inform. de los indicadores socioeconom
socioEcon <- merge(socioEcon, renta, by.x = "almuTxt", by.y = "Serie", all.x = T)

###############################
###___3___Suelo no urbanizable protegido
###############################
# Se lee el excel que contiene los datos de suelo no urbanizable protegido
pathSuelo <- file.path(pathDatos, "suelo_no_urbanizable_protegido.xls")
suelo <- read_excel(pathSuelo, skip = 10, col_names = T)
suelo <- suelo[-c(1:14, 194:195), ] # Se eliminan los 14 primeros registros y los dos Ultimos, que no son municipios

# Se introduce el dato de Area de cada municipio
suelo <- merge(suelo, socioEcon[ , c("almuTxt", "km2")], by.x = "Serie", by.y = "almuTxt", all.x = T)

# Se calculan los indicadores de suelo
suelo$sueloPorcen05 <- (suelo$"2005" / (suelo$km2 * 100)) * 100
suelo$sueloPorcen17 <- (suelo$"2017" / (suelo$km2 * 100)) * 100
suelo$sueloPorcen17 <- ifelse(suelo$sueloPorcen17 > 100, 100, suelo$sueloPorcen17)
suelo$sueloDifPorcen <- suelo$sueloPorcen17 - suelo$sueloPorcen05 # Dif. de suelo. en porcentaje
suelo$tasaSuelo <- ((suelo$"2017" - suelo$"2005") / (suelo$"2005" * 12 )) * 100

# Eliminar valores Inf por divisiOn entre 0
suelo$sueloDifPorcen <- ifelse(suelo$sueloDifPorcen == Inf, suelo$sueloPorcen17, suelo$sueloDifPorcen)
suelo[is.na(suelo)] <- 0 # Cambiar los Nas a cero
# Se seleccionan unicamente los campos de interEs
campos <- c("Serie", "sueloPorcen05", "sueloPorcen17", "sueloDifPorcen", "tasaSuelo")
suelo <- suelo[ , campos]

# Se traspasan estos datos al df con la inform. de los indicadores socioeconom
socioEcon <- merge(socioEcon, suelo, by.x = "almuTxt", by.y = "Serie", all.x = T)

###############################
###___ExportaciOn a una capa con todos los indicadores socioeconOmicos
###############################
# Se seleccionan los campos de interEs de socioEcon
campos <- c("NATCODE", "NAMEUNIT.x",
            "pob96", "pob19", "pobDifPorcen", "tasaPob", "dens96","dens19", "densDifPorcen",
            "renta00", "renta18", "rentaDifPorcen", "tasaRenta",
            "sueloPorcen05", "sueloPorcen17", "sueloDifPorcen", "tasaSuelo")
socioEcon <- socioEcon[ , campos]
colnames(socioEcon)[2] <- "NAMEUNIT"

# Se unen a la capa de municipios "fusionados"
muniSocioEcon <- merge(muniFusion, socioEcon, by = "NATCODE", all.x = T)

# # Se generan mapas para ver la distribuciOn espacial de los indicadores
# mapasSocioEcon <- c("pob96", "pob19", "pobDifPorcen", "dens96","dens19",
#                     "densDifPorcen", "renta00", "renta18", "rentaDifPorcen",
#                     "sueloPorcen05", "sueloPorcen17", "sueloDifPorcen")
# counter <- 0
# for (mapa in mapasSocioEcon){
#   print(paste0("Generando mapa ", counter, ": ", mapa, "..."))
#   counter <- counter + 1
#  mapaTematico <- ggplot() +
#     geom_sf(data = muniSocioEcon, mapping = aes_string(fill = mapa)) +
#     ggtitle(mapa) +
#     theme(plot.title = element_text(size = 20))
#  nombreIndicador <- paste0(counter, mapa, ".png")
#  pathMapa <- file.path(exportPathComplete, nombreIndicador)
#  ggsave(pathMapa, width = 35, height = 35, units = "cm", dpi = 300)
# 
# }


# ExportaciOn de la capa de municipios fusionados con los datos de los indicadores
pathMuniSocioEcon <- file.path(exportPathComplete, "muniSocioEcon.shp")
st_write(muniSocioEcon, pathMuniSocioEcon, delete_layer = T)

# ExportaciOn de la capa de municipios fusionados
muniFusion <- muniFusion[ , c("NATCODE", "NAMEUNIT")]
pathMuniFusion <- file.path(exportPathComplete, "muniFusion.shp")
st_write(muniFusion, pathMuniFusion, delete_layer = T)
