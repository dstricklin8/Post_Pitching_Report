#### Load Packages ----
library(tidyverse)

# Load Statcast Data
load("data/statcast_23.rda")

# Create Pitch Colors Palette for ggplot fill
pitch_colors <- c(
  "Sinker" = "#fe9d00",
  "Slider" = "#efe717",
  "Changeup" = "#1fbe3a",
  "4-Seam Fastball" = "#d22d4a",
  "Split-Finger" = "#3bacac",
  "Curveball" = "#04d1ed",
  "Knuckle Curve" = "purple3",
  "Cutter" = "#933f1f",
  "Slurve" = "#b3b1fa",
  "Sweeper" = "gold3",
  "Screwball" = "limegreen",
  "Forkball" = "#aaf0d1",
  "Slow Curve" = "royalblue",
  "Knuckleball" = "darkgrey"
)

# Create Pitch Colors Palette for gt table fill
pitch_colors_tibble <- tibble(
  pitch_name = c("Sinker", "Slider", "Changeup", "4-Seam Fastball", "Split-Finger",
                 "Curveball", "Knuckle Curve", "Cutter", "Slurve", "Sweeper", "Screwball",
                 "Forkball", "Slow Curve", "Knuckleball"),

  pitch_hex = c("#fe9d00", "#efe717", "#1fbe3a", "#d22d4a", "#3bacac", "#04d1ed", "purple3",
                "#933f1f", "#b3b1fa", "gold3", "limegreen", "#aaf0d1", "royalblue", "darkgrey")
  )

statcast_colors <- left_join(statcast_23, pitch_colors_tibble, by = c("pitch_name"))

# Clean up Statcast data (works for any statcast dataset)
mutate_sc <- function(df) {
  
  df_mutate <- df %>%
    filter(
      pitch_type != "UN",
      pitch_name != "Pitch Out",
      pitch_name != "Other",
      pitch_name != "Eephus",
      game_year != "Spring Training"
    )
  
  return(df_mutate)
  
}
sc_23 <- mutate_sc(statcast_colors)

# Find the number of pitches each pitcher threw for their respective pitches
metrics <- sc_23 %>%
  group_by(player_name, pitch_name) %>%
  summarise(count = n())

# Join metric data and full data
statcast_count <- left_join(sc_23, metrics, by = c("player_name", "pitch_name"))

# Only pic pitches that pitchers threw more than once
sc_23 <- statcast_count %>% 
  filter(
    count > 1
  )

# Create dataset of MLB pitchers
mlb_arms <- sc_23 %>% 
  select(player_name) %>% 
  arrange(player_name) %>% 
  unique()

# Visuals Setup ----
#### Home Plate 
x <- c(-8.5, -8, 0, 8, 8.5, -8.5)
z <- c(0, 2, 4, 2, 0, 0)

home_plate <- data.frame(x, z)

# Strike Zone 
x <- c(-10, 10, 10, -10, -10)
z <- c(18, 18, 42, 42, 18)
sz <- data.frame(x, z)

# New Strike Zone
x <- c(-10/3, -10/3, 10/3, 10/3, -10/3)
z <- c(18, 42, 42, 18, 18)
sz_2 <- data.frame(x, z)

# New Strike Zone
x <- c(-10, -10, 10, 10, -10)
z <- c(26, 34, 34, 26, 26)
sz_3 <- data.frame(x, z)

### Outer Zones 
x <- c(-10, -14, -14, 0, 0, -10, -10)
z <- c(30, 30, 46, 46, 42, 42, 30)
kzone_11 <- data.frame(x, z)

x <- c(-10, -14, -14, 0, 0, -10, -10)
z <- c(30, 30, 14, 14, 18, 18, 30)
kzone_13 <- data.frame(x, z)

x <- c(10, 10, 14, 14, 0, 0, 10)
z <- c(42, 30, 30, 46, 46, 42, 42)
kzone_12 <- data.frame(x, z)

x <- c(10, 10, 14, 14, 0, 0, 10)
z <- c(18, 30, 30, 14, 14, 18, 18)
kzone_14 <- data.frame(x, z)

# Save Data
save(sc_23, mlb_arms, pitch_colors, home_plate, sz, sz_2, sz_3,
     kzone_11, kzone_12, kzone_13, kzone_14, file = "data/info.rda")


