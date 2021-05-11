library(tidyverse)
library(rjson)
library(googledrive)
library(googlesheets4)
googledrive::drive_auth()
sheets_auth(token = drive_token())
anna_sites <- read_sheet("https://docs.google.com/spreadsheets/d/1_VMGjAWod4f8jAEx2Hf7VKAu3L1MKVKHHY_19QP39wY/edit#gid=177375130")
names(anna_sites)[c(2,3)] <- c("name","country")
# replace "and" in names by ","
anna_sites$name <- gsub(" and ", ",", anna_sites$name)
anna_sites_mod <- anna_sites %>%
  # split multiple PAs into separate rows
  tidyr::separate_rows(name,sep = "[ ]?[,&+/][ ]?")  %>%
  tidyr::separate_rows(name,sep = " - ")

# # load wdpaid, name english, name original, iso3, country
# wdpa_json <- fromJSON(file = "https://dopa-services.jrc.ec.europa.eu/services/d6biopamarest/d6biopama/get_wdpa?format=json")
# # fill NULL with NA
# for (i in 1:length(wdpa_json$records)){
#   for (j in 1:length(wdpa_json$records[[i]])){
#     if (is.null(wdpa_json$records[[i]][[j]])){
#       wdpa_json$records[[i]][[j]] <- NA
#     }
#   }
# }
# wdpa <- as.data.frame(matrix(unlist(wdpa_json$records), ncol = max(lengths(wdpa_json$records)), byrow = TRUE), stringsAsFactors = FALSE)
# names(wdpa) <- names(wdpa_json$records[[which(lengths(wdpa_json$records)>0)[1]]])
# rm(wdpa_json)
# save(wdpa, file = "wdpa_acp.rdata")
# # note: 8565 PAs < 8573 reported on rris.biopama.org/PA_Dashboard
# wdpa <- wdpa %>%
#   mutate(name_low = tolower(name)) %>%
#   mutate(name_desig = tolower(paste(name, desig_eng)))

# load latest wdpa from protectedplanet.net
wdpa <- read_csv(file = "../../data/WDPA_May2020-csv/WDPA_May2020-csv.csv")
names(wdpa) <- tolower(names(wdpa))
# get country names
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
countries <- 
  # remove spatial info
  as.data.frame(world) %>% 
  select(iso_a3, name) %>% 
  rename(country_name = name) %>%
  distinct()

wdpa <- wdpa %>%
  left_join(countries,
            by = c("iso3" = "iso_a3"))


# compare anna_sites_mod$name with wdpa$name
ind <- anna_sites_mod$name %in% wdpa$name
sum(ind)/length(ind)
# 15 % of entries have a direct match. worth to do a join to start with

anna_sites_wdpa <- anna_sites_mod %>%
  select(-WDPAID, -ISO3, -KLC_ID) %>%
  mutate(name_low = tolower(name)) %>%
  left_join(wdpa %>%
              select(wdpaid, name_low)
            ) %>%
  left_join(wdpa %>%
              select(wdpaid, name_desig),
            by = c("name_low" = "name_desig")
            ) %>%
  left_join(wdpa %>%
              mutate(orig_name_low = tolower(orig_name)) %>%
              select(wdpaid, orig_name_low),
            by = c("name_low" = "orig_name_low")
            ) %>%
  # merge wdpaid, wdpaid.x, wdpaid.y
  mutate(
    WDPAID = case_when(
      !is.na(wdpaid.x)   ~ wdpaid.x,
      !is.na(wdpaid.y) & is.na(wdpaid.x) ~ wdpaid.y,
      !is.na(wdpaid)   & is.na(wdpaid.x) & is.na(wdpaid.y) ~ wdpaid,
      TRUE ~ "NA"
      )
    ) %>%
  # remove useless columns
  select(-wdpaid, -wdpaid.x, -wdpaid.y, -name_low) %>%
  # join with wdpa once more to get country
  left_join(wdpa %>%
              select(wdpaid, iso3, country_name),
              by = c("WDPAID" = "wdpaid")) %>%
  # add iso3 for countries without wdpaid
  left_join(countries, by = c("country" = "country_name"))

# export anna_sites_wdpa for manual editing
write_excel_csv(anna_sites_wdpa, "anna_sites_wdpa.csv")
# manually edit file and save as xlsx
# NB I edited from R1:R222
# load edited file
anna_sites_xl <- readxl::read_xlsx("anna_sites_wdpa.xlsx", sheet = 1, n_max = 623, 
                                   col_types = c("text", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "text"))
# entries should have ID, name, WDPAID or iso3.y and district or KLC/TFCA
anna_sites_clean <- anna_sites_xl %>%
  select(ID, name, country, WDPAID, iso3.y, `KLC/TFCA`, Note, district) %>%
  # join with wdpa to get countries
  left_join(wdpa %>% select(wdpaid, iso3), by = c("WDPAID" = "wdpaid")) %>%
  # rename iso3 to iso3.x
  rename(Name = name, iso3.x = iso3, KLC_TFCA = `KLC/TFCA`) %>%
  # keep only one iso3
  mutate(ISO3 = if_else(!is.na(iso3.x), iso3.x, iso3.y)) %>%
  # split KLC and TFCAs
  mutate(KLC_TFCA_L = grepl(x = KLC_TFCA, pattern = "[[:upper:]]{3}_[[:digit:]]{2}")) %>%
  mutate(KLC = if_else(KLC_TFCA_L, KLC_TFCA, as.character(NA))) %>%
  mutate(TFCA = if_else(is.na(KLC), KLC_TFCA, as.character(NA))) %>%
  # modify R# so that they sort correctly
  separate(col = ID, sep = 'R', into = c("ID_1", "ID_2"), remove = FALSE) %>%
  arrange(as.numeric(ID_2)) %>%
  # set missing WDPAID to 0
  mutate(WDPAID0 = if_else(is.na(WDPAID), 0, WDPAID)) %>%
  select (-WDPAID) %>%
  rename(WDPAID = WDPAID0) %>%
  # Nota
  mutate(Nota = case_when(
    !is.na(Note) ~ Note,
    WDPAID != 0 ~ "PA",
    WDPAID == 0 & !is.na(ISO3) & !is.na(district)  ~ "Country",
    TRUE ~ as.character(NA)
  )) %>%
  # select columns
  select(ID, Name, country, WDPAID, ISO3, KLC, TFCA, Nota, district) 

write_excel_csv(anna_sites_clean, path = "anna_sites_clean_R1_R222.csv")
# manually add to Google sheet

anna_pa <- anna_sites_clean %>%
  filter(WDPAID > 0) %>%
  select(ID, Name, WDPAID, ISO3) %>%
  distinct()
write_excel_csv(anna_pa, path = "anna_sites_clean_pa_R1_R222.csv")
# manually add to Google sheet

# country map
# load from Google sheet
anna_countries <- read_sheet("https://docs.google.com/spreadsheets/d/1_VMGjAWod4f8jAEx2Hf7VKAu3L1MKVKHHY_19QP39wY/edit#gid=177375130",
                             sheet = 3) %>%
  select(ID, ISO3) %>%
  distinct() %>%
  count(ISO3) %>%
  mutate(count_class = case_when(
    n <= 1 ~ "1",
    n > 1 & n <= 2 ~ "2",
    n > 2 & n <= 10 ~ "3-10",
    n > 10 & n <= 50 ~ "11-50",
    n > 50 & n <= 100 ~ "51-100",
    n > 100 ~ ">100",
    TRUE ~ as.character(NA)
  )) %>%
  right_join(world %>% select(iso_a3, geometry),
            by = c("ISO3" = "iso_a3")) 
# export to geoJson or something
sf::st_write(anna_countries, "anna_countries_geo.geojson")

library(wesanderson)

ggplot(data = anna_countries) +
  geom_sf(aes(fill = count_class, geometry = geometry)) +
  scale_fill_brewer(palette = "YlGnBu")

# Download from Google sheets
anna_clean_google <- read_sheet("https://docs.google.com/spreadsheets/d/1_VMGjAWod4f8jAEx2Hf7VKAu3L1MKVKHHY_19QP39wY/edit#gid=177375130",
                       sheet = 3)
anna_pa <- anna_clean_google %>%
  filter(WDPAID > 0) %>%
  select(ID, Name, WDPAID, ISO3) %>%
  distinct() %>%
  select(WDPAID) %>%
  count(WDPAID) %>%
  left_join(wdpa %>% select (type, wdpaid, name, desig_eng, desig_type, iucn_cat, marine, iso3, country_name),
            by = c("WDPAID" = "wdpaid"))
# write to file
write_excel_csv(anna_pa, path = "anna_sites_clean_pa_list.csv")
# NB 9 sites are points (no polygon)

# join with wdpa polygons
left_join(wdpa_sf) %>%
  sf::st_write("anna_pa_geo.geojson")

# load points_and_centroids created with qgis
points_and_centroids <- sf::st_read("anna_survey_WDPA_May2020.gpkg", layer = "points_and_polygons_centroids")
anna_pa_geo <- points_and_centroids %>%
  select(WDPAID) %>%
  left_join(anna_pa)
sf::st_write(anna_pa_geo, dsn = "anna_survey_WDPA_May2020.gpkg", layer = "anna_pa_geo")


# Anna sent me another version of her data with respondants/countries
anna_countries_new <- read_sheet("https://docs.google.com/spreadsheets/d/1Sj-6frMyIlN9PMkJZIQMudY8IkfP0OiuL2mD6ON-i7Q/edit#gid=1628275330",
                                 sheet = 2
                                 ) %>% 
  right_join(world %>% 
              select(name, iso_a3),
            by = c("country_name" = "name")
  ) %>%
  mutate(count_class = case_when(
    n == 1 ~ "1",
    n > 1 & n <= 2 ~ "2",
    n > 2 & n <= 10 ~ "3-10",
    n > 10 & n <= 50 ~ "11-50",
    n > 50 & n <= 100 ~ "51-100",
    n > 100 ~ ">100",
    TRUE ~ as.character(NA)
  ))
# export to geoJson or something
sf::st_write(anna_countries_new, "anna_countries_new_geo.geojson")
