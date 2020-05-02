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
library(sf)



# Load data ---------------------------------------------------------------

# Load Istat population data
provincie_2011 <- read_delim("Istat/Data/Censimento_2011_Indicatori_famiglie_per_Province.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(
    provincia_id = as.numeric(CodiceIstat)
  )

# Load borders for provinces
provincie_shp <- readOGR("Istat/Data/ProvCM01012020_g/ProvCM01012020_g_WGS84.shp")
regioni_shp <- readOGR("Istat/Data/Reg01012020_g/Reg01012020_g_WGS84.shp")

# Load Covid data
#dpc_covid19_ita_province_latest <- read_csv("../COVID-19/dati-province/dpc-covid19-ita-province-latest.csv") %>%
#  mutate(
#    provincia_id = as.numeric(codice_provincia)
#  )
dpc_covid19_ita_province_all <- 
  dir("../COVID-19/dati-province", pattern = "(dpc-covid19-ita-province-202).*\\.(csv)") %>%
  map(~ read_csv(file.path("../COVID-19/dati-province", .))) %>% 
  reduce(rbind) %>%
  mutate(
    provincia_id = as.numeric(codice_provincia)
  )



# Merge data --------------------------------------------------------------

provincie_covid19_all <- provincie_2011 %>%
  select(
    provincia_id, Nome, `Ampiezza demografica`
  ) %>%
  right_join(
    dpc_covid19_ita_province_all,
    by = "provincia_id"
  ) %>%
  select(
    data, provincia_id, Nome, `Ampiezza demografica`, totale_casi
  ) %>%
  rename(
    date = data,
    provincia_name =  Nome, 
    population = `Ampiezza demografica`,
    covid19_cases = totale_casi
  )  %>%
  mutate(
    provincia_id_str = as.character(provincia_id),
    covid19_cases_incidence100k = covid19_cases / (population / 100000)
  )

provincie_covid19_shp <- st_as_sf(provincie_shp) %>%
  right_join(
    provincie_covid19_all,
    by = c("COD_PROV" = "provincia_id_str")
  )


# Mapping -----------------------------------------------------------------

covid19_cases_facets <- tm_layout(
  frame = FALSE,
  ) + 
  tm_shape(provincie_covid19_shp) +
  tm_polygons(
    "covid19_cases_incidence100k",
    title = "Casi covid-19\nper 100 mila\nabitanti\n",
    n = 7,
    style = "fisher",
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
    paste0(
      "Stefano De Sabbata - Contiene dati Istat,\nPresidenza del Consiglio dei Ministri,\nDipartimento della Protezione Civile\n"
      #,
      #provincie_covid19_all %>% slice(1) %>% pull(data)
    ),
    position =c ("left", "bottom")
  ) +
  tm_facets(along = "date", free.coords = FALSE)

tmap_animation(covid19_cases_facets, filename = "covid19_cases_anim.gif",
               delay = 200, restart.delay = 200)
