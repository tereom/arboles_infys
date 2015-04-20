## Lectura y preparación de datos

options(digits = 2)
source("src/tema_ggplot.R")
library(ggplot2)


library(plyr)
library(tidyr)
library(dplyr)

library(rgdal)
library(maptools)

library(gridExtra)

library(Hmisc)
library(data.table)


arbolado <- read.csv("../../datos/bases_infys_2009_2013/Arbolado_2009_2013_mac.txt", 
  stringsAsFactors=FALSE, na.strings = c("-9999", "n/a", "NULL", "NA", "N/A"))

# Recodificar NAs
arbolado[arbolado == -9999] <- NA
arbolado[arbolado == 999] <- NA
arbolado[arbolado == "En identificación En identificación"] <- NA
arbolado[arbolado == "ZZ"] <- NA
arbolado[arbolado == "En identificación en identificacion"] <- NA

save(arbolado, file = "../../datos/bases_procesadas_R/arbolado.Rdata")

glimpse(arbolado)

conglomerados <- read.csv("../../datos/bases_infys_2009_2013/Conglomerados_mac.txt", 
  stringsAsFactors=FALSE, na.strings = c("-9999", "n/a", "NULL", "NA", "N/A"))
conglomerados$Cgl <- as.character(conglomerados$Conglomerado)

## Clasificación de vegetación Pedro Diaz
clasif_veg <- read.csv("../../datos/catalogos_adicionales/Clasificacion_serie_V.csv", 
                       stringsAsFactors = FALSE)
clasif_veg

clasif_veg_1 <- clasif_veg %>% 
  select(formacion, Veg_prim_levantad = clave.serie.v ) %>%
  mutate(Veg_prim_levantad = toupper(Veg_prim_levantad))

conglomerados <- inner_join(conglomerados, clasif_veg_1)

# agregamos la información de la base de datos de arbolado a nivel cgl. en 
# particular nos interesan las variables altura y diámetro

arbolado_cgl <- arbolado %>%
  group_by(Cgl) %>%
  summarise(
    num_arboles = n(),
    altura_media = mean(AlturaTotal, na.rm = TRUE),
    altura_sd = sd(AlturaTotal, na.rm = TRUE),
    altura_05 = quantile(AlturaTotal, 0.05, na.rm = TRUE),
    altura_95 = quantile(AlturaTotal, 0.95, na.rm = TRUE),
    diametro_media = mean(DiametroNormal, na.rm = TRUE),
    diametro_sd = sd(DiametroNormal, na.rm = TRUE), 
    diametro_05 = quantile(DiametroNormal, 0.05, na.rm = TRUE),
    diametro_95 = quantile(DiametroNormal, 0.95, na.rm = TRUE)
    ) %>%
  mutate(
    num_cat = cut2(num_arboles, g = 6))

# unimos con la base de datos de conglomerado
cgls <- filter(arbolado_cgl, complete.cases(arbolado_cgl)) %>%
  inner_join(conglomerados, by = c("Cgl" = "Cgl"))

save(cgls, file = "datos/cgls.Rdata")
