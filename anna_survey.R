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

# load wdpaid, name english, name original, iso3, country
wdpa_json <- fromJSON(file = "https://dopa-services.jrc.ec.europa.eu/services/d6biopamarest/d6biopama/get_wdpa?format=json")
# fill NULL with NA
for (i in 1:length(wdpa_json$records)){
  for (j in 1:length(wdpa_json$records[[i]])){
    if (is.null(wdpa_json$records[[i]][[j]])){
      wdpa_json$records[[i]][[j]] <- NA
    }
  }
}
wdpa <- as.data.frame(matrix(unlist(wdpa_json$records), ncol = max(lengths(wdpa_json$records)), byrow = TRUE), stringsAsFactors = FALSE)
names(wdpa) <- names(wdpa_json$records[[which(lengths(wdpa_json$records)>0)[1]]])
rm(wdpa_json)
save(wdpa, file = "wdpa_acp.rdata")
# note: 8565 PAs < 8573 reported on rris.biopama.org/PA_Dashboard
wdpa <- wdpa %>%
  mutate(name_low = tolower(name)) %>%
  mutate(name_desig = tolower(paste(name, desig_eng)))

countries <- wdpa %>%
  select(iso3, country_name) %>%
  distinct()

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
anna_sites_xl <- readxl::read_xlsx("anna_sites_wdpa.xlsx", sheet = 1)
# entries should have ID, name, WDPAID or iso3.y and district or KLC/TFCA
anna_sites_clean <- anna_sites_xl %>%
  select(ID, name, WDPAID, iso3.y, Note, district, `KLC/TFCA`) %>%
  # join with wdpa to get countries
  left_join(wdpa %>% select(wdpaid, iso3), by = c("WDPAID" = "wdpaid")) %>%
  # rename iso3 to iso3.x
  rename(iso3.x = iso3) %>%
  # keep only one iso3
  mutate(iso3 = if_else(!is.na(iso3.x), iso3.x, iso3.y)) %>%
  # select columns
  select(ID, name, WDPAID, iso3, Note, district, `KLC/TFCA`) %>%
  # select only the ID I've worked on
  filter(ID <= "R222")

save(anna_sites_clean, file = "anna_sites_clean_R1_R222.rdata")

anna_pa <- anna_sites_clean %>%
  select(WDPAID) %>%
  distinct() %>%
  left_join(wdpa_sf)


# country map
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
anna_countries <- anna_sites_clean %>%
  select(ID, iso3) %>%
  distinct() %>%
  group_by(iso3) %>%
  summarise(n()) %>%
  right_join(world %>% select(iso_a3, geometry),
            by = c("iso3" = "iso_a3")) 
  
ggplot(data = anna_countries) +
  geom_sf(aes(fill = `n()`, geometry = geometry))


