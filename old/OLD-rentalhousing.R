
## Case study: which wards have the highest/lowest quality rental housing?


```{r make-map-1,message=FALSE}
library(tmap) # For maps!
library(sp) # Spatial datatypes
# Read in the pre-cleaned apartment data with postal codes
apartmentpostal <- readRDS("./data/apartment-data/apartment-postal-merged.rds")
# Render it as an appropriate spatial datatype
apartmentpoints <- SpatialPointsDataFrame(
  coords = apartmentpostal[ ,c("lon","lat")],
  data = apartmentpostal,
  proj4string = CRS(proj4string(torontoct))
)
```

```{r make-map-2}
library(tmap) # Better map plotting software.
# Load the shape file.
# This is a spatial data format that tells us where to draw polygons
# You don't have to know about it for this course (won't be tested).
canadashape <- rgdal::readOGR( # FSA- 3 digit postal code
  "/Users/alexstringer/teaching/w20/STA238/materials/book/data/shape-files/canada-fsa/gfsa000a11a_e.shp"
)

canadact <- rgdal::readOGR( # Census tracts
  "/Users/alexstringer/teaching/w20/STA238/materials/book/data/shape-files/canada-ct/gct_000b11a_e.shp"
)

canadada <- rgdal::readOGR( # Dissemination areas
  "/Users/alexstringer/teaching/w20/STA238/materials/book/data/shape-files/canada-da/gda_000b11a_e.shp"
)

# Filter out only toronto postals.
# Can't use the filter() function on a spatial dataframe.
# Subset manually. Use a 'regular expression', common knowledge among CS folks
# but maybe not so much among statisticians?
# Toronto postals start with "M". "^[A-Z]" is a regular expression which 
# matches any string that starts with "M".
torontoshape <- subset(canadashape,
                       stringr::str_extract(canadashape@data$CFSAUID,"^[A-Z]") == "M")

torontoct <- subset(canadact,
                    stringr::str_detect(canadact@data$CMANAME,"Toronto"))

torontoda <- subset(canadada,
                    stringr::str_detect(canadada@data$CDNAME,"Toronto"))

# Summarize the apartment scores by FSA (first 3 digits of postal).
# Another clever (?) use of regular expressions.
apartmentsummaryfsa <- apartmentpostal %>%
  mutate(fsa = stringr::str_extract(postal,"M[0-9][A-Z]")) %>% # Extracts the first three digits of the postal of the form MNX where N is a number and X is any letter.
  group_by(fsa) %>%
  summarize(score = mean(score),numapartments = n())

glimpse(apartmentsummaryfsa)

# How big are the FSA's?
apartmentsummaryfsa %>%
  ggplot(aes(x = numapartments)) +
  theme_light() +
  geom_histogram(bins = 20,colour = "black",fill = "lightgrey") +
  labs(title = "Number of rental buildings in each FSA",
       x = "Number of buildings",
       y = "Number of FSAs with that many buildings")

# What does the distribution of average score in an FSA look like?
apartmentsummaryfsa %>%
  ggplot(aes(x = score)) +
  theme_light() +
  geom_histogram(bins = 20,colour = "black",fill = "lightgrey") +
  labs(title = "Average score in each FSA",
       x = "Average score",
       y = "Number of FSAs with that average score")

# Map of toronto coloured by average score
# Merge the shape data with the apartment data
toronto_apartments_avgscore <- sp::merge(
  torontoshape,
  apartmentsummaryfsa,
  by.x = "CFSAUID",
  by.y = "fsa"
)

tmap_mode("plot")
tm_shape(toronto_apartments_avgscore) +
  tm_fill("score") +
  tm_shape(apartmentpoints) +
  tm_dots()

```

```{r map-3}
library(sp)
# Summary by Census Tract
# Find out which CT each apartment building is in
torontoapartmentsct <- sp::over(apartmentpoints,torontoct)
# Add it back on to the apartments data
apartmentpostal_withct <- apartmentpostal
apartmentpostal_withct$ct <- torontoapartmentsct$CTUID

apartmentsummaryct <- apartmentpostal_withct %>%
  group_by(ct) %>%
  summarize(score = mean(score),numapartments = n())

# How big are the Census Tracts?
apartmentsummaryct %>%
  ggplot(aes(x = numapartments)) +
  theme_light() +
  geom_histogram(bins = 20,colour = "black",fill = "lightgrey") +
  labs(title = "Number of rental buildings in each CT",
       x = "Number of buildings",
       y = "Number of CTs with that many buildings")

# What does the distribution of average score in an FSA look like?
apartmentsummaryct %>%
  ggplot(aes(x = score)) +
  theme_light() +
  geom_histogram(bins = 20,colour = "black",fill = "lightgrey") +
  labs(title = "Average score in each CT",
       x = "Average score",
       y = "Number of CTs with that average score")

toronto_apartments_avgscore_ct <- sp::merge(
  torontoct,
  apartmentsummaryct,
  by.x = "CTUID",
  by.y = "ct"
)

toronto_apartments_avgscore_ct@bbox <- toronto_apartments_avgscore@bbox

tmap_mode("plot")
tm_shape(toronto_apartments_avgscore_ct) +
  tm_fill("score") +
  tm_shape(apartmentpoints) +
  tm_dots()
```

The reason the map has grey is because I filtered based on the Toronto "Census Metropolitan Area" (CMA), which includes the suburbs (a bunch of "L" postal codes). When you replicate this for the Dissemination Areas, you won't have this grey, because they use the Toronto "Census Division", which is what we commonly think of as being "Toronto". Census data is confusing!


Exercise: reproduce the following analysis.

```{r map-4,echo = FALSE}
torontoapartmentsda <- sp::over(apartmentpoints,torontoda)
# Add it back on to the apartments data
apartmentpostal_withda <- apartmentpostal
apartmentpostal_withda$da <- torontoapartmentsda$DAUID

apartmentsummaryda <- apartmentpostal_withda %>%
group_by(da) %>%
summarize(score = mean(score),numapartments = n())

# How big are the Census Tracts?
apartmentsummaryda %>%
ggplot(aes(x = numapartments)) +
theme_light() +
geom_histogram(bins = 20,colour = "black",fill = "lightgrey") +
labs(title = "Number of rental buildings in each DA",
x = "Number of buildings",
y = "Number of DAs with that many buildings")

# What does the distribution of average score in an FSA look like?
apartmentsummaryda %>%
ggplot(aes(x = score)) +
theme_light() +
geom_histogram(bins = 20,colour = "black",fill = "lightgrey") +
labs(title = "Average score in each DA",
x = "Average score",
y = "Number of DAs with that average score")

toronto_apartments_avgscore_da <- sp::merge(
torontoda,
apartmentsummaryda,
by.x = "DAUID",
by.y = "da"
)

toronto_apartments_avgscore_ct@bbox <- toronto_apartments_avgscore@bbox

tmap_mode("plot")
tm_shape(toronto_apartments_avgscore_da) +
tm_fill("score")# +
# tm_shape(apartmentpoints) +
# tm_dots()
```
