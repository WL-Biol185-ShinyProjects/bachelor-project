library(dplyr)
library(tidygeocoder)

# Load the data
contestants <- read.csv("contestants.csv", stringsAsFactors = FALSE)
# Function to convert "Eliminated" to numeric week
get_week_number <- function(eliminated) {
  if (grepl("Winner", eliminated, ignore.case = TRUE)) {
    return(11)
  } else if (grepl("Runner", eliminated, ignore.case = TRUE)) {
    return(10)
  } else if (grepl("Week", eliminated, ignore.case = TRUE)) {
    week_num <- as.numeric(gsub(".*Week ([0-9]+).*", "\\1", eliminated))
    return(week_num)
  } else {
    return(1)
  }
}

# Process Bachelor data
bachelor_map_data <- contestants %>%
  filter(Show == "The Bachelor") %>%
  mutate(
    week_number = sapply(Eliminated, get_week_number),
    city_state = Hometown
  ) %>%
  group_by(city_state) %>%
  summarise(
    count = n(),
    avg_week = mean(week_number, na.rm = TRUE),
    max_week = max(week_number, na.rm = TRUE)
  ) %>%
  ungroup()

# Process Bachelorette data
bachelorette_map_data <- contestants %>%
  filter(Show == "The Bachelorette") %>%
  mutate(
    week_number = sapply(Eliminated, get_week_number),
    city_state = Hometown
  ) %>%
  group_by(city_state) %>%
  summarise(
    count = n(),
    avg_week = mean(week_number, na.rm = TRUE),
    max_week = max(week_number, na.rm = TRUE)
  ) %>%
  ungroup()

# Geocode Bachelor data (this will take a few minutes - ONE TIME ONLY)
print("Geocoding Bachelor data...")
bachelor_geocoded <- bachelor_map_data %>%
  geocode(city_state, method = 'osm', lat = latitude, long = longitude)

# Geocode Bachelorette data
print("Geocoding Bachelorette data...")
bachelorette_geocoded <- bachelorette_map_data %>%
  geocode(city_state, method = 'osm', lat = latitude, long = longitude)

# Save the geocoded data
write.csv(bachelor_geocoded, "bachelor_map_data.csv", row.names = FALSE)
write.csv(bachelorette_geocoded, "bachelorette_map_data.csv", row.names = FALSE)

print("Done! Geocoded data saved.")
