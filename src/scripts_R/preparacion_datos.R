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


##### Quitar outliers
arbolado_2 <- arbolado %>%
  mutate(
    AlturaTotal = ifelse(AlturaTotal > 50 | AlturaTotal <= 0, NA, AlturaTotal),
    DiametroNormal = ifelse(DiametroNormal > 34 | DiametroNormal <= 0, 
                            NA, DiametroNormal),
    DiametroCopa = ifelse(DiametroCopa > 10 | DiametroCopa <= 0, NA, 
                          DiametroCopa)
  )

save(arbolado_2, file = "../../datos/bases_procesadas_R/arbolado_2.Rdata")

conglomerados <- read.csv("../../datos/bases_infys_2009_2013/Conglomerados_mac.txt", 
  stringsAsFactors=FALSE, na.strings = c("-9999", "n/a", "NULL", "NA", "N/A"))
conglomerados$Cgl <- as.character(conglomerados$Conglomerado)

## Clasificación de vegetación Pedro Diaz
clasif_veg <- read.csv("../../datos/catalogos_adicionales/Clasificacion_serie_V.csv", 
                       stringsAsFactors = FALSE)
clasif_veg

clasif_veg_1 <- clasif_veg %>% 
  dplyr::select(formacion, Veg_prim_levantad = clave.serie.v ) %>%
  mutate(Veg_prim_levantad = toupper(Veg_prim_levantad))

conglomerados <- inner_join(conglomerados, clasif_veg_1)

# agregamos la información de la base de datos de arbolado a nivel cgl. en 
# particular nos interesan las variables altura y diámetro

arbolado_cgl <- arbolado_2 %>%
  group_by(Cgl) %>%
  summarise(
    num_arboles = n(),
    alturaT_media = mean(AlturaTotal, na.rm = TRUE),
    alturaT_sd = sd(AlturaTotal, na.rm = TRUE),
    alturaT_05 = quantile(AlturaTotal, 0.05, na.rm = TRUE),
    alturaT_95 = quantile(AlturaTotal, 0.95, na.rm = TRUE),
    diametroN_media = mean(DiametroNormal, na.rm = TRUE),
    diametroN_sd = sd(DiametroNormal, na.rm = TRUE), 
    diametroN_05 = quantile(DiametroNormal, 0.05, na.rm = TRUE),
    diametroN_95 = quantile(DiametroNormal, 0.95, na.rm = TRUE),
    diametroC_media = mean(DiametroCopa, na.rm = TRUE),
    diametroC_sd = sd(DiametroCopa, na.rm = TRUE), 
    diametroC_05 = quantile(DiametroCopa, 0.05, na.rm = TRUE),
    diametroC_95 = quantile(DiametroCopa, 0.95, na.rm = TRUE)
    ) %>%
  mutate(
    num_cat = cut2(num_arboles, g = 6))


# unimos con la base de datos de conglomerado
cgls <- filter(arbolado_cgl, complete.cases(arbolado_cgl)) %>%
  inner_join(conglomerados, by = c("Cgl" = "Cgl"))

save(cgls, file = "../../datos/bases_procesadas_R/cgls.Rdata")

#######################################################################
############################# Bases 2013 ##############################
#######################################################################

arbolado_bqs <- read.delim("../../datos/bases_infys_2009_2013/Arbolado_2013_bqs_db.csv", 
  stringsAsFactors = FALSE, 
  na.strings = c("-9999", "n/a", "NULL", "NA", "N/A", "(null)"))

arbolado_oc <- read.csv("../../datos/bases_infys_2009_2013/Arbolado_2013_oc_db.csv", 
  stringsAsFactors = FALSE, 
  na.strings = c("-9999", "n/a", "NULL", "NA", "N/A", "(null)"))

cbind(names(arbolado_bqs), names(arbolado_oc))

# por ahora eliminamos AlturaFusteLimpio (no está en otras comunidades)
vars_conj <- names(arbolado_bqs)[names(arbolado_bqs) %in% names(arbolado_oc)]

arbolado_bqs <- arbolado_bqs[, vars_conj][, 1:16]
arbolado_bqs$comunidad <- "bosque selva"
arbolado_oc <- arbolado_oc[, vars_conj][, 1:16]
arbolado_oc$comunidad <- "otras comunidades"

str(arbolado_bqs)
str(arbolado_oc)

arbolado_13 <- rbind(arbolado_bqs, arbolado_oc)

hist_altura <- ggplot(arbolado_13, aes(x = AlturaTotal)) + 
  geom_histogram(aes(y = ..density..), binwidth = 2) +
  labs(y = "", title = "Altura Total")

hist_diametro <- ggplot(arbolado_13, aes(x = DiametroNormal)) + 
  geom_histogram(aes(y = ..density..), binwidth = 3) +
  labs(y = "", title = "Diámetro Normal")

hist_diametroC <- ggplot(arbolado_13, aes(x = DiametroCopa)) + 
  geom_histogram(aes(y = ..density..), binwidth = 2) +
  labs(y = "", title = "Diámetro Copa")

hist_altura_log <- ggplot(arbolado_13, aes(x = log(AlturaTotal))) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.2) +
  scale_x_continuous("m", labels = exp, 
    breaks = log(sapply(-3:6, function(i) 2 ^ i)), limits = c(-3, 6)) +
  labs(y = "", title = "Altura Total (log)")

hist_diametro_log <- ggplot(arbolado_13, aes(x = log(DiametroNormal))) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.09) +
  scale_x_continuous("m", labels = exp, 
    breaks = log(sapply(-1:8, function(i) 2 ^ i)), limits = c(2, 8)) +
  labs(y = "", title = "Diámetro Normal (log)")

hist_diametroC_log <- ggplot(arbolado_13, aes(x = log(DiametroCopa))) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.3) +
  scale_x_continuous("m", labels = exp, 
    breaks = log(sapply(-3:5, function(i) 2 ^ i)), limits = c(-3, 4)) +
  labs(y = "", title = "Diámetro Copa (log)")

grid.arrange(hist_altura, hist_diametro, hist_diametroC, hist_altura_log, 
             hist_diametro_log, hist_diametroC_log, nrow = 2)

quantile(arbolado_13$AlturaTotal, seq(0, 1, 0.05), na.rm = T)
quantile(arbolado_13$DiametroNormal, seq(0, 1, 0.05), na.rm = T)
quantile(arbolado_13$DiametroCopa, seq(0, 1, 0.05), na.rm = T)

##### Quitar outliers
arbolado_13_2 <- arbolado_13 %>%
  mutate(
    AlturaTotal = ifelse(AlturaTotal > 50 | AlturaTotal <= 0, NA, AlturaTotal),
    DiametroNormal = ifelse(DiametroNormal > 34 | DiametroNormal <= 0, 
                            NA, DiametroNormal),
    DiametroCopa = ifelse(DiametroCopa > 10 | DiametroCopa <= 0, NA, 
                          DiametroCopa)
  )

# unir con número de conglomerado
# para esto hace falta unir primero con TblSitio
sitio_13 <- read.csv("../../datos/bases_infys_2009_2013/Sitio_2013_db.csv", 
  stringsAsFactors = FALSE, 
  na.strings = c("-9999", "n/a", "NULL", "NA", "N/A", "(null)"))

conglomerados_db_13 <- read.csv("../../datos/bases_infys_2009_2013/Conglomerado_2013_db.csv",
  stringsAsFactors=FALSE)

conglomerado_13 <- select(conglomerados_db_13, IdConglomerado, 
       TipoFormato, Fecha, TipoConglomerado, Municipio)

sitio_cgl <- left_join(sitio_13, conglomerado_13)
arbolado_13_3 <- left_join(arbolado_13_2, sitio_cgl)

arbolado_13_3 %>%
  arrange(arbolado_13_3, IdConglomerado)[1:100, c(1, 18)]

head(arbolado_13_3)
save(arbolado_13_2, file = "../../datos/bases_procesadas_R/arbolado_13.Rdata")

### Chequeos

chequeo <- arbolado_13_3 %>%
  group_by(IdConglomerado) %>%
  mutate(
    n_sitios = length(unique(IdSitio))
  )

table(chequeo$n_sitios)
