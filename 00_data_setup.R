#### Load Packages ----
library(tidyverse)

# STATCAST APP ----
# Load Statcast Data
# load("data/statcast_23.rda")

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
save(sc_23, mlb_arms, mutate_sc, pitch_colors, home_plate, sz, sz_2, sz_3,
     kzone_11, kzone_12, kzone_13, kzone_14, file = "data/info.rda")

# TRACKMAN APP ----

season <- read_csv("data/23_season.csv")

test <- season

test$pitch_tag <- test$TaggedPitchType

unique(season$TaggedPitchType)

# Create Pitch Colors Palette for ggplot fill
pitch_colors <- c(
  "Sinker" = "#fe9d00",
  "Slider" = "#efe717",
  "ChangeUp" = "#1fbe3a",
  "Changeup" = "#1fbe3a",
  "Four-Seam" = "#d22d4a",
  "Splitter" = "#3bacac",
  "Curveball" = "#04d1ed",
  "Cutter" = "#933f1f",
  "Fastball" = "#d22d4a",
  "TwoSeamFastBall" = "maroon3",
  "Undefined" = "darkgrey",
  "Other" = "darkgrey"
)

pitch_colors_tibble <- tibble(
  pitch_tag = c("Sinker", "Slider", "Changeup", "ChangeUp",
                "Four-Seam", "Splitter", "Curveball",
                 "Cutter", "Fastball", "TwoSeamFastBall", "Undefined", "Other"),
  
  pitch_hex = c("#fe9d00", "#efe717", "#1fbe3a", "#1fbe3a",
                "#d22d4a", "#3bacac", "#04d1ed",
                "#933f1f", "#d22d4a", "maroon3", "darkgrey", "darkgrey")
)

tm_colors <- left_join(test, pitch_colors_tibble, by = c("pitch_tag"))

season %>% 
  filter(
    Pitcher == "Utagawa, David"
  ) %>% 
  select(
    TaggedPitchType, AutoPitchType
  ) %>% 
  view()


test <- season %>% 
  filter(Pitcher == "Utagawa, David",
         !is.na(AutoPitchType),
         !is.na(TaggedPitchType),
         !AutoPitchType %in% c("Undefined", "Other"),
         !TaggedPitchType %in% c("Undefined", "Other")) %>% 
  mutate(
    TaggedPitchType = case_when(
      TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
      T ~ TaggedPitchType),
    PlateLocSide = 12*PlateLocSide,
    PlateLocHeight = 12*PlateLocHeight,
    
    swing = ifelse(
      PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 1, 0),
    
    contact = ifelse(
      PitchCall %in% c("FoulBall", "InPlay"), 1, 0),
    
    # fpK = case_when(
    #   !PitchCall %in% c("BallCalled") & Count == "0-0" ~ 1,
    #   PitchCall %in% c("BallCalled") & Count == "0-0" ~ 0,
    #   !PitchCall %in% c("BallCalled") & Count != "0-0" ~ NA),
    
    strike = ifelse(
      !PitchCall %in% c("BallCalled"), 1, 0),
    
    in_zone = if_else(
      PlateLocSide >= -10 & PlateLocSide <= 10 &
        PlateLocHeight >= 18 & PlateLocHeight <= 42, 1, 0),
    
    out_zone = 1 - in_zone,
    
    z_swing = case_when(
      in_zone == "1" & swing == "1" ~ 1,
      !in_zone == "1" & !swing == "1" ~ NA),
    o_swing = case_when(
      out_zone == "1" & swing == "1" ~ 1,
      !out_zone == "1" & !swing == "1" ~ NA),
    z_contact = case_when(
      in_zone == "1" & contact == "1" ~ 1,
      !in_zone == "1" & !contact == "1" ~ NA),
    o_contact = case_when(
      out_zone == "1" & contact == "1" ~ 1,
      !out_zone == "1" & !contact == "1" ~ NA)
  ) %>% 
  mutate(date = ymd(Date)) %>% 
  mutate_at(
    vars(date),
    funs(year, month, day)
    )

ggplot(test %>% filter(Pitcher == "Utagawa, David")) +
  geom_bar(aes(AutoPitchType, fill = AutoPitchType)) +
  scale_fill_manual(values = pitch_colors) +
  theme_minimal() +
  guides(colour = guide_legend(ncol = 1)) +
  theme(
    legend.key.size = unit(rel(1.2), 'cm'),
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_blank(),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    legend.box = element_blank(),
    legend.box.margin = margin(0, -1, 0, -1),
    legend.position = "left"
  )

test %>% 
  group_by(year) %>% 
  summarise(
    num_pitches = n(),
    zone_prop = mean(in_zone)*100,
    Z_Swing = sum(z_swing, na.rm = TRUE)/sum(in_zone, na.rm = TRUE)*100,
    Z_Contact = sum(z_contact, na.rm = TRUE)/sum(z_swing, na.rm = TRUE)*100,
    chase = sum(o_swing, na.rm = TRUE)/sum(out_zone, na.rm = TRUE)*100,
    chase_contact = sum(o_contact, na.rm = TRUE)/sum(o_swing, na.rm = TRUE)*100,
    swing_prop = mean(swing)*100,
    contact_prop = sum(contact)/sum(swing)*100,
  ) %>% 
  gt(rowname_col = "year") %>% 
  cols_label(
    num_pitches = md("Pitches"),
    zone_prop = md("Zone %"),
    Z_Swing = md("Zone Swing %"),
    Z_Contact = md("Zone Contact %"),
    chase = md("Chase %"),
    chase_contact = md("Chase Contact %"),
    swing_prop = md("Swing%"),
    contact_prop = md("Contact%"),
  ) %>% 
  fmt_number(
    columns = c("zone_prop":"contact_prop"),
    decimals = 1) %>% 
  tab_options(table_body.hlines.style = "none") %>% 
  gt_theme_nytimes()


