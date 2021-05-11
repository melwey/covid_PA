klc <- sf::st_read(dsn = "/Users/mela/JRCbox/BIOPAMA/klc_201909_proposal.gpkg", layer = "klc_201909_proposal", stringsAsFactors = FALSE)
View(as.data.frame(klc) %>% select(KLC_name, target_PAs, KLC_ID))
klc_df <- klc %>% as.data.frame() %>%
  select(KLC_ID, KLC_name, target_PAs) %>%
  # unnest target_PAs
  mutate(PAs = gsub("[[:punct:]]", ",", target_PAs)) %>%
  separate_rows(PAs, sep = ",") %>%
  mutate(WDPAID = as.numeric(PAs)) %>%
  drop_na(WDPAID) %>%
  select(-target_PAs, -PAs) %>%
  left_join(wdpa %>% select(type, wdpaid, name),
            by = c("WDPAID" = "wdpaid") )
  
  # remove empty lines
  filter(nchar(PAs) > 0)
  
# it seems there are still some eroors in my KLC layer: WDPAID not in WDPA???
  
  
# additional sites on Anna's list
anna <- data.frame(WDPAID = c(1342, 26070, 555622048, 7391, 198302, 555697861, 555697918, 352222)) %>%
  left_join(wdpa %>% select(type, wdpaid, name),
            by = c("WDPAID" = "wdpaid") )


## intersect klc with biodiversity hotspots
# hotspots
hotspots <- sf::st_read(dsn = "/Users/mela/Documents/JRC/BIOPAMA/ESS/data/hotspots_2016_1/hotspots_2016_1.shp") %>%
  filter(Type == "hotspot area")
# check validity
sf::st_is_valid(hotspots, reason = TRUE)
sf::st_is_valid(klc, reason = TRUE)
# intersect
klc_hotspots <- sf::st_intersection(klc, hotspots)

kh <- klc_hotspots  %>%
  select(KLC_ID, Area_km2, NAME) %>%
  arrange((KLC_ID)) %>%
  mutate(hot_area = sf::st_area())
