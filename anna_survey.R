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

# compare anna_sites_mod$name with wdpa$name
ind <- anna_sites_mod$name %in% wdpa$name
sum(ind)/length(ind)
# 15 % of entries have a direct match. worth to do a join to start with

anna_sites_wdpa <- anna_sites_mod %>%
  select(-WDPAID, -ISO3) %>%
  left_join(wdpa %>%
              select(wdpaid, name, country_name, iso3)
            )



