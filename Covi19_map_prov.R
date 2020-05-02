##########
# Covid19 map
# Author: Stefano De Sabbata
# Date: 30 April 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
##########


rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rgdal)
library(tmap)



# Load data ---------------------------------------------------------------

# Load Istat population data - Census 2011
provincie_2011 <- read_delim("Istat/Data/Censimento_2011_Indicatori_famiglie_per_Province.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(
    prov_id = as.numeric(CodiceIstat)
  )


# Approximate population of Provincia del Sud Sardegna
# to sum of Carbonia-Iglesias and Medio Campidano
pop_sud_sardegna <- provincie_2011 %>%
  filter(Nome %in% c("CARBONIA-IGLESIAS", "MEDIO CAMPIDANO")) %>%
  select(`Ampiezza demografica`) %>%
  sum()

provincie_2011 <- provincie_2011 %>%
  add_row(
    prov_id = 111,
    Nome = "Sud Sardegna",
    `Ampiezza demografica` = pop_sud_sardegna
  )


# Load borders for provinces
provincie_shp <- readOGR("Istat/Data/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")
regioni_shp <- readOGR("Istat/Data/Reg01012020_g/Reg01012020_g_WGS84.shp")


# Load Covid data
dpc_covid19_ita_province_latest <- read_csv("../COVID-19/dati-province/dpc-covid19-ita-province-latest.csv") %>%
  mutate(
    prov_id = as.numeric(codice_provincia)
  )



# Merge data --------------------------------------------------------------

# Merge population and covid19 cases
prov_covid19_latest <- provincie_2011 %>%
  select(
    prov_id, Nome, `Ampiezza demografica`
  ) %>%
  right_join(
    dpc_covid19_ita_province_latest,
    by = "prov_id"
  ) %>%
  select(
    data, prov_id, Nome, `Ampiezza demografica`, totale_casi
  ) %>%
  rename(
    covid19_date = data,
    prov_name =  Nome, 
    population = `Ampiezza demografica`,
    covid19_cases = totale_casi
  )  %>%
  mutate(
    prov_id_str = as.character(prov_id),
    covid19_date = date(covid19_date),
    covid19_cases_incidence100k = covid19_cases / (population / 100000)
  ) 

# Merge with geometries
prov_covid19_shp <- st_as_sf(provincie_shp) %>%
  right_join(
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
    title = "Totale casi covid-19\nper 100 mila abitanti",
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
      "Stefano De Sabbata - @maps4thought
https://github.com/sdesabbata/covid-19-it-analysis
Contiene dati Istat e Presidenza del Consiglio dei Ministri,
Dipartimento della Protezione Civile
Note: Dati popolazione Censimento 2011,
Provincia del Sud Sardegna approssimata
alla somma delle ex provincie di Carbonia-
Iglesias and Medio Campidano. Le categorie
in legenda includono il valore all'estremo
minimo ed escludono quello all'estremo massimo.\n",
      latest_date
    ),
    position =c ("left", "bottom"),
    size = 0.5
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
