##########
# Covid19 map animated
# Author: Stefano De Sabbata
# Date: 30 April 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
##########


rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rgdal)
library(tmap)
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
dpc_covid19_ita_province_all <- 
  dir("../COVID-19/dati-province", pattern = "(dpc-covid19-ita-province-202).*\\.(csv)") %>%
  map(~ read_csv(file.path("../COVID-19/dati-province", .))) %>% 
  reduce(rbind) %>%
  mutate(
    prov_id = as.numeric(codice_provincia),
  )



# Merge data --------------------------------------------------------------

# Merge population and covid19 cases
prov_covid19_all <- provincie_2019 %>%
  select(
    prov_id, Nome, TotalePopolazione
  ) %>%
  right_join(
    dpc_covid19_ita_province_all,
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
  right_join(
    prov_covid19_all,
    by = c("COD_PROV" = "prov_id_str")
  )
  


# Mapping -----------------------------------------------------------------

# Maps
covid19_cases_facets <- tm_layout(
    frame = FALSE,
  ) + 
  tm_shape(prov_covid19_shp) +
  tm_polygons(
    "covid19_cases_incidence100k",
    title = "Totale casi covid-19\nper 100 mila abitanti",
    n = 9,
    style = "fixed",
    breaks = c(0, 1, 10, 25, 50, 100, 250, 500, 1000, 2500),
    palette = "viridis",
    #border.col = "#cccccc",
    lwd = 0.1
    ) +
  #tm_shape(regioni_shp) +
  tm_borders(
    col = "#FFFFFF",
    lwd = 0.5
  ) +
  tm_credits(
    "Stefano De Sabbata - @maps4thought
https://github.com/sdesabbata/covid-19-it-analysis
Contiene dati Istat e Presidenza del Consiglio dei 
Ministri, Dipartimento della Protezione Civile
Note: Totale della popolazione residente stimato al 
1° Gennaio 2019 Le categorie in legenda includono 
il valore all'estremo minimo ed escludono quello 
all'estremo massimo.",
    position =c ("left", "bottom"),
    size = 0.4
  ) +
  tm_facets(along = "covid19_date", free.coords = FALSE)

# Generate gif
tmap_animation(
  covid19_cases_facets, 
  filename = "Maps/covid19_cases_it_fixed_anim.gif",
  width = 1600, 
  delay = 20,
  restart.delay = 100
)
