## code to prepare `DATASET` dataset goes here

library(here)
library(sf)
library(tidyverse)

mx_estados <- st_read(paste0(here(), "/data-raw/mexstates.shp"))

mx_estados <- mx_estados %>%
  transmute(ID = OBJECTID,
            nombre = case_when(ADMIN_NAME == "Distrito Federal" ~ "Ciudad de México",
                                ADMIN_NAME == "Mexico" ~ "México",
                                ADMIN_NAME == "Michoacan" ~ "Michoacán",
                                ADMIN_NAME == "Nuevo Leon" ~ "Nuevo León",
                                ADMIN_NAME == "Queretaro" ~ "Querétaro",
                                ADMIN_NAME == "San Luis Potosi" ~ "San Luis Potosí",
                                ADMIN_NAME == "Yucatan" ~ "Yucatán",
                                TRUE ~ ADMIN_NAME),
            region = case_when(nombre %in% c("Baja California", "Sonora", "Chihuahua", "Coahuila", "Nuevo León", "Tamaulipas") ~ "Norte",
                               nombre %in% c("Baja California Sur", "Sinaloa", "Nayarit", "Durango", "Zacatecas") ~ "Norte-occidente",
                               nombre %in% c("Jalisco", "Aguascalientes", "Colima", "Michoacán", "San Luis Potosí") ~ "Centro-norte",
                               nombre %in% c("Guanajuato", "Querétaro", "Hidalgo", "México", "Ciudad de México", "Morelos", "Tlaxcala", "Puebla") ~ "Centro",
                               nombre %in% c("Guerrero", "Oaxaca", "Chiapas", "Veracruz", "Tabasco", "Campeche", "Yucatán", "Quintana Roo") ~ "Sur"),
            region = factor(region))

usethis::use_data(mx_estados, overwrite = TRUE)
