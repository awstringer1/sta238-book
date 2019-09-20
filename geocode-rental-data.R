# Helper script to geocode the Toronto rental apartments data

library(ggmap)
# Load API key
# To get a google maps API key go to https://cloud.google.com/maps-platform/
# Save it in a text file and load it using parse()
key <- scan("~/google-maps-key",what = "character")
ggmap::register_google(key)


# Read in the apartments data
apartmentdata <- readr::read_csv(
  file = "./data/apartment-data/toronto-apartment-building-evaluations.csv"
)

ggmap::geocode("569  BROADVIEW AVE")


