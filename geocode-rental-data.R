# Helper script to geocode the Toronto rental apartments data

library(ggmap) # For geocoding
library(tmap) # For plotting
# Load API key
# To get a google maps API key go to https://cloud.google.com/maps-platform/
# Save it in a text file and load it using parse()
key <- scan("~/google-maps-key",what = "character")
ggmap::register_google(key)

# FSA (Forward Sortation Area- the first three digits of a postal code,
# like M4K) boundary files were downloaded from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
# Read them in
fsa <- rgdal::readOGR(
  "/Users/alexstringer/teaching/w20/STA238/materials/book/data/shape-files/canada-fsa/gfsa000a11a_e.shp"
)
# It's a SpatialPolygonsDataFrame:
class(fsa)
slotNames(fsa)
glimpse(fsa@data)

# Read in the apartments data
apartmentdata <- readr::read_csv(
  file = "./data/apartment-data/toronto-apartment-building-evaluations.csv"
)

# Geocode the apartments data
# We want the output to be a spatialpointsdataframe with the apartment data
# combined with its postal code and lat/lon

geocode_apartment <- function(address,joinid) {
  # Address: single string containing one address from the apartments data
  
  # Combine the address with "Toronto", as street names/numbers aren't
  # nationally unique.
  address2 <- stringr::str_c(address," TORONTO, ON, CANADA")
  
  # Geocode it using ggmap::geocode()
  geo <- ggmap::geocode(address2,output = "all",source = "google")
  
  # Parse the results
  
  postal = tryCatch(
    geo$results[[1]]$address_components[[8]]$long_name,
    error = function(e) "N/A"
  )
  
  lat = tryCatch(
    geo$results[[1]]$geometry$location$lat,
    error = function(e) -1
  )
  
  lon = tryCatch(
    geo$results[[1]]$geometry$location$lng,
    error = function(e) -1
  )
  
  tibble(
    joinid = joinid,
    address = address,
    postal = postal,
    lat = lat,
    lon = lon 
  )
}

# Geocode all the addresses.
# First add a joinid to the apartment data.
# This is so I don't have to join on address.
# Joining on long character strings without a consistent format
# is a bad idea.
apartmentdata$joinid <- 1:nrow(apartmentdata)
geocoded_addresses <- purrr::map2( # Takes about xx minutes
  apartmentdata$SITE_ADDRESS,
  apartmentdata$joinid,
  geocode_apartment
) %>%
  reduce(dplyr::bind_rows)

save(geocoded_addresses,file = "./data/apartment-data/geocoded-addresses.Rdata")

# Now merge back to the apartment data, clean, and save
apartmentclean <- apartmentdata %>% 
  filter(!is.na(SCORE)) %>% # Remove apartments with missing scores
  dplyr::select(joinid,
                ward = WARD,
                score = SCORE,
                property_type = PROPERTY_TYPE,
                year_built = YEAR_BUILT
  )

apartmentmerged <- apartmentclean %>%
  left_join(geocoded_addresses,by = "joinid")

nrow(apartmentclean)
nrow(geocoded_addresses)
nrow(apartmentmerged)

# Save to disk
saveRDS(apartmentmerged,"./data/apartment-data/apartment-postal-merged.rds")


# Get Toronto map and save to disk
