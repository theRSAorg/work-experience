# Required libraries
library(httr)
library(readxl)
library(dplyr)
library(here)
library(sf)
library(janitor)
library(readr)

rm(list = ls())

#### Access to greenspace data ####
url <- 'https://www.ons.gov.uk/file?uri=/economy/environmentalaccounts/datasets/accesstogardensandpublicgreenspaceingreatbritain/accesstopublicparksandplayingfieldsgreatbritainapril2020/ospublicgreenspacereferencetables.xlsx'

filename <- basename(url)
filepath <- here::here("./data/", filename)

if(!file.exists(filepath)){
  cat("Downloading data\n")
  download.file(url, destfile = filepath, mode = "wb")
} else{
  cat("Data already in directory. Loading it.\n")
}

greenspace_data <- read_excel(filepath, sheet = 'LSOA Parks and Playing Fields', range = cell_cols("A:J,M"))

greenspace_data <- greenspace_data %>%
  rename(lsoa11cd = `LSOA code`,
         lsoa11nm = `LSOA name`,
         average_distance = `Average distance to nearest Park, Public Garden, or Playing Field (m)`,
         rgn11nm = `Region name`) %>%
  filter(!is.na(`Country name`)) %>% # drop metadata-like stuff in final rows 
  select(lsoa11cd,lsoa11nm, rgn11nm, average_distance)

# Check the data
# head(greenspace_data)

##### Spatial data ####

# LAD boundaries
pattern <- here('data','LAD_Dec_2020_boundaries', '*.shp')
files <- Sys.glob(pattern)
lad_sf <- sf::st_read(files[1]) %>%
  janitor::clean_names() # make colnames lowercase

# geog lookup
pattern <- here('data','LSOA11_LAD20_Lookup', '*.csv')
files <- Sys.glob(pattern)
geog_lookup <- readr::read_csv(files[1]) %>%
  janitor::clean_names() %>% # make colnames lowercase
  select('lsoa11nm', 'lsoa11cd', 'ladnm','ladcd') %>%
  distinct() # remove repeated rows because this messes up the join to lsoa_sf

# LSOA boundaries
pattern <- here('data','LSOA_11_boundaries', '*.shp')
files <- Sys.glob(pattern)
lsoa_sf <- sf::st_read(files[1]) %>%
  janitor::clean_names() %>% # make colnames lowercase
  left_join(., geog_lookup, by = c('lsoa11cd','lsoa11nm'))
 
# region boundaries
pattern <- here('data','RGN_23_boundaries', '*.shp')
files <- Sys.glob(pattern)
england_region_boundaries <- sf::st_read(files[1]) %>%
  janitor::clean_names() # make colnames lowercase

# country boundaries
pattern <- here('data','CTRY_23_boundaries', '*.shp')
files <- Sys.glob(pattern)
country_boundaries <- sf::st_read(files[1]) %>%
  janitor::clean_names() # make colnames lowercase


# Combine regions and countries to avoid overlap in plots
country_boundaries_altered <- country_boundaries %>%
  select(-'ctry23nmw') %>%
  rename(
    rgn23cd = ctry23cd,
    rgn23nm = ctry23nm
    )

regions_filled <- bind_rows(england_region_boundaries, country_boundaries_altered) %>%
  filter(rgn23nm != "England") # drop England so that we don't double plot it

# merge lsoa_sf with greenspace data
lsoa_sf_greenspace <- lsoa_sf %>%
  left_join(., greenspace_data, by = c("lsoa11nm","lsoa11cd"))

greenspace_data <- greenspace_data %>%
  left_join(., geog_lookup, by = c("lsoa11nm","lsoa11cd")) %>%
  relocate(rgn11nm, .after = everything()) %>%
  relocate(average_distance, .after = everything())

saveRDS(lsoa_greenspace, file = "./data/lsoa_greenspace.rds")

readr::write_csv(greenspace_data, file = "./data/greenspace_data.csv")

sf::st_write(lsoa_sf_greenspace, "./data/lsoa_greenspace.shp")

#### prepare london sf ####
london_lads <-  c("E09000001",
                  "E09000002",
                  "E09000003",
                  "E09000004", 
                  "E09000005", 
                  "E09000006", 
                  "E09000007", 
                  "E09000008", 
                  "E09000009", 
                  "E09000010", 
                  "E09000011", 
                  "E09000012", 
                  "E09000013",
                  "E09000014", 
                  "E09000015", 
                  "E09000016", 
                  "E09000017",
                  "E09000018", 
                  "E09000019", 
                  "E09000020", 
                  "E09000021",
                  "E09000022", 
                  "E09000023",
                  "E09000024", 
                  "E09000025",
                  "E09000026", 
                  "E09000027", 
                  "E09000028", 
                  "E09000029",
                  "E09000030", 
                  "E09000031", 
                  "E09000032", 
                  "E09000033")

# load london geometry
lon_coords <- sf::read_sf("./data/LAD_Dec_2020_Boundaries/Local_Authority_Districts__December_2020__Full_Clipped_Boundaries_UK.shp") %>%
  janitor::clean_names() %>%
  filter(lad20cd %in% london_lads)

sf::st_write(lon_coords, "./data/london_coords.shp")

