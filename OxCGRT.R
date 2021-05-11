# OxCGRT
# Hale, Thomas, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira (2020). Oxford COVID-19 Government Response Tracker, Blavatnik School of Government. Data use policy: Creative Commons Attribution CC BY standard.
# download latest data
OxCGRT <- readr::read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv")
# Stay at home: 
# C6_Stay at home requirements
# C6_Flag
# C7_Restrictions on internal movement
# C8_International travel controls
names(OxCGRT) <- gsub(" ", "_", names(OxCGRT))

zaf <- OxCGRT %>% filter(CountryCode == "ZAF") %>%
  mutate(date = parse_date(as.character(Date), format ="%Y%m%d"))
ggplot() +
  geom_line(data = zaf, aes(x = date, y = C7_Restrictions_on_internal_movement), colour = "red") +
  geom_line(data = zaf, aes(x = date, y = C8_International_travel_controls), colour = "blue") +
  # geom_line(data = mobility_country, aes(x = date, y = parks_percent_change_from_baseline))
  labs(y = "Restriction index")

            