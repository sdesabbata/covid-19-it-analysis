##########
# Covid19 map
# Author: Stefano De Sabbata
# Date: 30 April 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
##########


rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(rgdal)
library(tmap)
library(classInt)
library(sf)



# Load data ---------------------------------------------------------------

provincie_2019 <- read_csv("Istat/Data/tavola_pop_res01.csv") %>%
  mutate(
    prov_id = as.numeric(CodiceProvincia)
  )

# # Load Istat population data - Census 2011
# provincie_2019 <- read_delim("Istat/Data/Censimento_2011_Indicatori_famiglie_per_Province.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
#   mutate(
#     prov_id = as.numeric(CodiceIstat)
#   )
# 
# # Approximate population of Provincia del Sud Sardegna
# # to sum of Carbonia-Iglesias and Medio Campidano
# pop_sud_sardegna <- provincie_2019 %>%
#   filter(Nome %in% c("CARBONIA-IGLESIAS", "MEDIO CAMPIDANO")) %>%
#   select(TotalePopolazione) %>%
#   sum()
# 
# provincie_2019 <- provincie_2019 %>%
#   add_row(
#     prov_id = 111,
#     Nome = "Sud Sardegna",
#     TotalePopolazione = pop_sud_sardegna
#   )


# Load borders for provinces
provincie_shp <- readOGR("Istat/Data/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")
regioni_shp <- readOGR("Istat/Data/Reg01012020_g/Reg01012020_g_WGS84.shp")


# Load Covid data
#dpc_covid19_ita_province_latest <- read_csv("../COVID-19/dati-province/dpc-covid19-ita-province-latest.csv") %>%
dpc_covid19_ita_province_latest <- read_csv("../COVID-19/dati-province/dpc-covid19-ita-province-20200701.csv") %>%
  mutate(
    prov_id = as.numeric(codice_provincia)
  )



# Merge data --------------------------------------------------------------

# Merge population and covid19 cases
prov_covid19_latest <- provincie_2019 %>%
  select(
    prov_id, Nome, TotalePopolazione
  ) %>%
  right_join(
    dpc_covid19_ita_province_latest,
    by = "prov_id"
  ) %>%
  select(
    data, prov_id, Nome, TotalePopolazione, totale_casi
  ) %>%
  rename(
    covid19_date = data,
    prov_name =  Nome, 
    population = TotalePopolazione,
    covid19_cases = totale_casi
  )  %>%
  mutate(
    prov_id_str = as.character(prov_id),
    covid19_date = date(covid19_date),
    covid19_cases_incidence100k = covid19_cases / (population / 100000)
  ) 

# Merge with geometries
prov_covid19_shp <- st_as_sf(provincie_shp) %>%
  left_join(
    prov_covid19_latest,
    by = c("COD_PROV" = "prov_id_str")
  )



# Mapping -----------------------------------------------------------------

# Info
latest_date <- prov_covid19_latest %>% slice(1) %>% pull(covid19_date)
map_file_name <- paste0("Maps/covid19_cases_latest_", latest_date, ".png")
map_dpi = 300

# Map
covid19_cases_latest <- tm_layout(
    frame = FALSE,
  ) + 
  tm_shape(prov_covid19_shp) +
  tm_polygons(
    "covid19_cases_incidence100k",
    #title = "Totale casi covid-19\nper 100 mila abitanti",
    title = paste0(
      "Total covid-19 cases\nper 100,000 inhabitants\n(",
      #latest_date,
      "July 1st, 2020",
      ")"
    ),
    #
    #n = 7,
    #style = "jenks",
    style = "fixed",
    breaks = prov_covid19_shp %>% pull(covid19_cases_incidence100k) %>% classIntervals(style = "jenks", n = 7) %$% brks,
    as.count = TRUE,
    #legend.format = list(digits = 2),
    #
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
"Stefano De Sabbata - @maps4thought
https://github.com/sdesabbata/covid-19-it-analysis",
    position = c("right", "bottom"),
    size = 0.6,
    align = "right"
  ) +
  tm_credits(
"Contains data from Istat (CC BY 3.0 
IT) and Department of Civil Protection, 
Presidency of the Council of Ministers 
(CC-BY-4.0) — elaborazione di dati 
Istat (CC BY 3.0 IT) e Presidenza del 
Consiglio dei Ministri, Dipartimento 
della Protezione Civile (CC-BY-4.0).
Population estimates Jan. 1st, 2019.",
    position = c("left", "bottom"),
    size = 0.6
  )

# Save
tmap_save(
  covid19_cases_latest,
  bg="transparent",
  map_file_name,
  width = 210,
  height = 210,
  units = "mm",
  dpi = map_dpi
)
