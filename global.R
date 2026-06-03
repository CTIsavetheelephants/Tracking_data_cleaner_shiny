library(shiny)

options(shiny.maxRequestSize = 2000 * 1024^2)  # 2 GB
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(leaflet.extras)
library(DT)
library(plotly)
library(readr)
library(stringr)
library(lutz)
library(rnaturalearth)
library(purrr)
library(fs)
library(tidyr)
library(ggplot2)

source("R/utils_prep.R")
source("R/utils_flags.R")
source("R/mod_ingest.R")
source("R/mod_assign.R")
source("R/mod_flag.R")
source("R/mod_review.R")
source("R/mod_clean.R")
source("R/mod_export.R")
source("R/mod_dashboard.R")

# Load once at startup — reused by get_tz_for_points() and detect_utm_epsg()
NE_COUNTRIES <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

FLAG_COLOURS <- c(
  ok                   = "#888888",
  suspicious_vehicle   = "#ff7f00",
  suspicious_airborne  = "#377eb8",
  predeployment        = "#e41a1c",
  immobility           = "#b15928",
  shift_episode        = "#984ea3",
  outside_bbox         = "#006837",
  hq                   = "#d4aa00"
)
