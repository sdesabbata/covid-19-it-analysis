##########
# Covid19 map
# Author: Stefano De Sabbata
# Date: 30 April 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
##########



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rgdal)
library(tmap)




# Load data ---------------------------------------------------------------

# # Load Istat population data
# DCIS_POPRES1_30042020224629266 <- read_csv("Istat/Data/DCIS_POPRES1_30042020224629266.csv")
# 
# # Extract totals
# population <- DCIS_POPRES1_30042020224629266 %>%
#   filter(
#     `Sesso` == "totale",
#     `Età` == "totale",
#     `Stato civile` == "totale"
#   ) %>%
#   select(
#     ITTER107, Territorio, Value
#   ) %>%
#   rename(
#     Popolazione = Value
#   )

# Load Istat population data
provincie_2011 <- read_delim("Istat/Data/Censimento_2011_Indicatori_famiglie_per_Province.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(
    provincia_id = as.numeric(CodiceIstat)
  )

# Load borders for provinces
provincie_shp <- readOGR("Istat/Data/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")
regioni_shp <- readOGR("Istat/Data/Reg01012020_g/Reg01012020_g_WGS84.shp")

# Load Covid data
dpc_covid19_ita_province_latest <- read_csv("../COVID-19/dati-province/dpc-covid19-ita-province-latest.csv") %>%
  mutate(
    provincia_id = as.numeric(codice_provincia)
  )



# Merge data --------------------------------------------------------------

provincie_covid19 <- left_join(
  provincie_2011,
  dpc_covid19_ita_province_latest,
  by = "provincia_id"
  ) %>%
  select(
    provincia_id, Nome, `Ampiezza demografica`, totale_casi
  ) %>%
  rename(
    provincia_name =  Nome, 
    population = `Ampiezza demografica`,
    covid19_cases = totale_casi
  )  %>%
  mutate(
    provincia_id_str = as.character(provincia_id),
    covid19_cases_incidence100k = covid19_cases / (population / 100000)
  )

provincie_covid19_shp <- merge(provincie_shp, provincie_covid19, by.x = "COD_PROV", by.y = "provincia_id_str")
provincie_covid19_shp$covid19_cases_incidence100k = ifelse(is.na(provincie_covid19_shp$covid19_cases_incidence100k), 0, provincie_covid19_shp$covid19_cases_incidence100k)



# Mapping -----------------------------------------------------------------

tm_layout(
  frame = FALSE,
  ) + 
  tm_shape(provincie_covid19_shp) +
  tm_polygons(
    "covid19_cases_incidence100k",
    title = "Casi covid-19\nper 100 mila\nabitanti\n",
    n = 7,
    style = "jenks",
    palette = "viridis",
    #border.col = "#cccccc",
    lwd = 0.1
    ) +
  tm_shape(regioni_shp) +
  tm_borders(
    col = "#FFFFFF",
    lwd = 0.5
  ) +
  tm_credits(
    paste0(
      "Stefano De Sabbata - Contiene dati Istat,\nPresidenza del Consiglio dei Ministri,\nDipartimento della Protezione Civile\n",
      dpc_covid19_ita_province_latest %>% slice(1) %>% pull(data)
    ),
    position =c ("left", "bottom")
  ) 