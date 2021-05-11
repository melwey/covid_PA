# agro-ecological suitability index value (GAEZ)
# intermediate input level
# rain-fed agriculture
# reference year 2010 (for GAEZ v4) or baseline (1961-1990) for GAEZ v3

# which are the major crops, globally?
# query FAOSTAT
library(FAOSTAT)

# GAEZ v3
# crop_suitability would be max(crops)
# stack all layers
# take max
# reset nodata (all negative values should be set as nodata)
