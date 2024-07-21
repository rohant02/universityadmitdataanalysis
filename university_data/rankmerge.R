library(dplyr)
library(purrr)
library(zipcodeR)
library(ggplot2)
library(maps)

# Read the data
data <- read.csv('Kmeans_assignment_data.csv')
postcode <- read.csv("usn/USNews-College-Scraper/data.csv")

# Select relevant columns from postcode data
postcodes1 <- postcode %>%
  select(institution.displayName, institution.zip)

# Merge data with postcode data
merged_data <- merge(data, postcodes1, by.x = "X", by.y = "institution.displayName", all.x = TRUE)

filtered_data <- merged_data %>%
  filter(!is.na(institution.zip))

# Define a function to check if a ZIP code is valid using tryCatch
is_valid_zip <- function(zip) {
  tryCatch({
    !is.null(zipcodeR::geocode_zip(zip))
  }, error = function(e) {
    FALSE
  })
}

# Filter out invalid ZIP codes
valid_data <- filtered_data %>%
  filter(sapply(institution.zip, is_valid_zip))

# Geocode the valid zip codes using zipcodeR
geocoded_data <- valid_data %>%
  mutate(geocode = map(institution.zip, ~zipcodeR::geocode_zip(.x))) %>%
  unnest(cols = c(geocode))

# Filter out invalid coordinates (latitude and longitude ranges for the contiguous USA)
geocoded_data <- geocoded_data %>%
  filter(lat >= 24 & lat <= 49, lng >= -125 & lng <= -66)

# View the geocoded data
head(geocoded_data)

# Get the map of the USA
usa_map <- map_data("state")

# Plot the universities on the map
ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "white") +
  geom_point(data = geocoded_data, aes(x = lng, y = lat), color = "red", size = 2, alpha = 0.7) +
  labs(title = "Universities in the USA", x = "Longitude", y = "Latitude") +
  theme_minimal()

