#### Load Packages ----
library(tidyverse)
library(shiny)
library(patchwork)
library(reactable)
library(jpeg)
library(grid)
library(ggridges)
library(sf)
library(sp)
library(rsconnect)
library(scales)
library(ggpubr)
library(bslib)
library(shinythemes)
library(gridlayout)
library(gt)
library(gtExtras)
library(bslib)

load("data/info.rda")

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

ui = grid_page(
  theme = bs_theme(version = 5, bootswatch = "pulse"),
  layout = c(
    "|    |250px   |1fr        |1fr       |1fr         |1fr    |
    |1fr  |sidebar |loc        |loc_2k    |loc_swings  |loc_bip |
    |1fr  |sidebar |loc        |loc_2k    |loc_swings  |loc_bip |
    |1fr  |sidebar |loc        |loc_2k    |loc_swings  |loc_bip |
    |1fr  |sidebar |loc        |loc_2k    |loc_swings  |loc_bip |
    |1fr  |sidebar |loc        |loc_2k    |loc_swings  |loc_bip |
    |1fr  |sidebar |ivb_hb     |rel_pt    |metrics     |metrics  |
    |1fr  |legend  |ivb_hb     |rel_pt    |metrics     |metrics  |
    |1fr  |legend  |ivb_hb     |rel_pt    |metrics     |metrics  |
    |1fr  |legend  |ivb_hb     |rel_pt    |metrics     |metrics  |
    |1fr  |legend  |ivb_hb     |rel_pt    |metrics     |metrics  |"
  ),
  grid_card(
    "sidebar",
    card_header("Pitching Report"),
    fileInput("upload_1", "Upload Statcast .csv File", accept = c(".csv"), placeholder = ""),
    textInput("player_first",
              "Enter Pitchers's First Name",
              value = ""),
    textInput("player_last",
              "Enter Pitchers's Last Name",
              value = ""),
    selectInput("batter_stands", 
                label = "Batter Stands:",
                choices = c("All", "Right", "Left")),
    actionButton("goButton_1", "Create Visual")
  ),
  # legend ----
  grid_card(
    "legend",
    card_header("Pitch Types"),
    card_body(
      plotOutput("legend")
    )
  ),
  # pitch_locations ----
  grid_card(
    "loc",
    card_header("All Pitches"),
    card_body(
      plotOutput("pitch_locations")
    )
  ),
  # locations_2k ----
  grid_card(
    "loc_2k",
    card_header("2 Strikes"),
    card_body(
      plotOutput("locations_2k")
    )
  ),
  # locations_2k ----
  grid_card(
    "loc_swings",
    card_header("Whiffs"),
    card_body(
      plotOutput("locations_whiffs")
    )
  ),
  # locations_bip ----
  grid_card(
    "loc_bip",
    card_header("Balls In Play"),
    card_body(
      plotOutput("locations_bip")
    )
  ),
  # movement ----
  grid_card(
    "ivb_hb",
    card_header("Pitch Movement"),
    card_body(
      plotOutput("movement")
    )
  ),
  # rel_pt ----
  grid_card(
    "rel_pt",
    card_header("Release Point"),
    card_body(
      plotOutput("rel_pt")
    )
  ),
  # metrics ----
  grid_card(
    "metrics",
    card_header("Pitch Metrics"),
    card_body(
      gt_output("metrics")
    )
  )
)

#### Server ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 10 * 1024^4)
  
  # Hitters ----
  df_1 <- eventReactive(input$upload_1, {
    read.csv(input$upload_1$datapath)
  })
  
  data <- eventReactive(input$goButton_1, {
    
    sc_df <- mutate_sc(df_1())
    
    playername <- paste(input$player_last, input$player_first, sep = ", ")
    
    sc_df <- sc_df %>% 
      filter(player_name == playername)
    
    sc_df <- left_join(sc_df, pitch_colors_tibble, by = c("pitch_name"))
    
    sc_df <- sc_df %>% 
      mutate(pitch_name = fct_infreq(pitch_name))
    
    if (input$batter_stands == "Right") {sc_df <- sc_df %>% filter(stand == "R")}
    else if (input$batter_stands == "Left") {sc_df <- sc_df %>% filter(stand == "L")}
    
    return(sc_df)
    
  })
  
  output$legend <- renderPlot({
    p1 <- ggplot(data()) +
      geom_point(aes(plate_x*-12, plate_z*12, fill = pitch_name), shape = 21, size = 6) +
      scale_fill_manual(values = pitch_colors)
    
    library(cowplot)
    
    legend_b <- get_legend(
      p1 +
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
        ))
    
    plot_grid(legend_b, ncol = 1)
  })
  output$pitch_locations <- renderPlot({
    # df <- df_1()
    # if (input$batter_stands == "Right") {df <- df %>% filter(stand == "R")}
    # else if (input$batter_stands == "Left") {df <- df %>% filter(stand == "L")}
    
    ggplot() +
      geom_point(data(),
                 mapping = aes(x = plate_x*-12, y = plate_z*12, fill = pitch_name),
                 size = 4, shape = 21, show.legend = F) +
      geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
      geom_path(sz, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_11, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_12, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_13, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_14, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
      scale_fill_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  })
  output$locations_2k <- renderPlot({
    # df <- df_1()
    # if (input$batter_stands == "Right") {df <- df %>% filter(stand == "R")}
    # else if (input$batter_stands == "Left") {df <- df %>% filter(stand == "L")}
    
    ggplot() +
      geom_point(data() %>% filter(strikes == 2),
                 mapping = aes(x = plate_x*-12, y = plate_z*12, fill = pitch_name),
                 size = 4, shape = 21, show.legend = F) +
      geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
      geom_path(sz, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_11, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_12, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_13, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_14, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
      scale_fill_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  })
  output$locations_whiffs <- renderPlot({
    ggplot() +
      geom_point(data() %>% filter(description %in% c("swinging_strike",
                                                           "swinging_strike_blocked",
                                                           "foul_tip")),
                 mapping = aes(x = plate_x*-12, y = plate_z*12, fill = pitch_name),
                 size = 4, shape = 21, show.legend = F) +
      geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
      geom_path(sz, mapping = aes(x, z), lty = 1, color = "darkgrey", alpha = 0.5) +
      geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "darkgrey", alpha = 0.5) +
      geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "darkgrey", alpha = 0.5) +
      geom_path(kzone_11, mapping = aes(x, z), lty = 1, color = "darkgrey", alpha = 0.5) +
      geom_path(kzone_12, mapping = aes(x, z), lty = 1, color = "darkgrey", alpha = 0.5) +
      geom_path(kzone_13, mapping = aes(x, z), lty = 1, color = "darkgrey", alpha = 0.5) +
      geom_path(kzone_14, mapping = aes(x, z), lty = 1, color = "darkgrey", alpha = 0.5) +
      coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
      scale_fill_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  })
  output$locations_bip <- renderPlot({
    ggplot() +
      geom_point(data() %>% filter(description %in% c("hit_into_play")),
                 mapping = aes(x = plate_x*-12, y = plate_z*12, fill = pitch_name),
                 size = 4, shape = 21, show.legend = F) +
      geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
      geom_path(sz, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_11, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_12, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_13, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      geom_path(kzone_14, mapping = aes(x, z), lty = 1, color = "darkgrey") +
      coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
      scale_fill_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  })
  output$movement <- renderPlot({
    ggplot(data()) +
      geom_point(mapping = aes(pfx_x*12, pfx_z*12, fill = pitch_name),
                 shape = 21, size = 3, show.legend = F) +
      coord_equal(xlim = c(-24, 24), ylim = c(-30, 30)) +
      theme_linedraw() +
      geom_vline(mapping = aes(xintercept = 0), linetype = 2, alpha = 0.5)+
      geom_hline(mapping = aes(yintercept = 0), linetype = 2, alpha = 0.5)+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_fill_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      )
  })
  output$rel_pt <- renderPlot({
    ggplot(data()) +
      geom_point(aes(release_pos_x*-12, release_pos_z*12, fill = pitch_name),
                 shape = 21, size = 2, show.legend = F) +
      coord_equal(xlim = c(-42, 42), ylim = c(0, 84)) +
      theme_linedraw() +
      geom_vline(mapping = aes(xintercept = 0), linetype = 2, alpha = 0.5)+
      geom_hline(mapping = aes(yintercept = 0), linetype = 2, alpha = 0.5)+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_fill_manual(values = pitch_colors) +
      scale_fill_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      )
  })
  output$metrics <- render_gt({
    
    data () %>% 
      group_by(pitch_name, pitch_hex) %>% 
      summarise(
        num_pitches = n(),
        max_rel_speed = max(release_speed, na.rm = T),
        release_speed = mean(release_speed, na.rm = T),
        pfx_x = mean(pfx_x*12, na.rm = T),
        pfx_z = mean(pfx_z*12, na.rm = T),
        release_spin_rate = mean(release_spin_rate, na.rm = T)
      ) %>% 
      arrange(pitch_name) %>% 
      ungroup() %>% 
      mutate(
        prop = num_pitches/sum(num_pitches),
        color = ""
      ) %>% 
      gt()  %>% 
      cols_hide(columns = c(pitch_hex)) %>% 
      cols_label(
        color = "",
        pitch_name = md(""),
        prop = md("Usage"),
        release_speed = md("Avg. Velo"),
        max_rel_speed = md("Max. Velo"),
        release_spin_rate = md("Spin Rate"),
        pfx_x = md("HB"),
        pfx_z = md("IVB"),
        num_pitches = md("Pitches")
      ) %>% 
      cols_move(columns = c(color), after = pitch_name)  %>% 
      cols_move(columns = c(prop), after = num_pitches)  %>% 
      fmt_number(columns = c("release_speed", "max_rel_speed", "pfx_x", "pfx_z"), decimals = 1) %>%  
      fmt_number(columns = c("release_spin_rate"), decimals = 0) %>%  
      fmt_percent(columns = c("prop"), decimals = 1) %>% 
      tab_style(
        style = list(
          cell_borders(
            sides = c("top", "bottom"),
            color = "grey90",
            weight = px(1))),
        locations = cells_body()) %>% 
      cols_align(
        align = c("center"),
        columns = everything()
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = from_column(column = "pitch_hex")),
          cell_text(color = "grey8", weight = "bold",
                    font = system_fonts(name = "geometric-humanist"))),
        locations = cells_body(columns = color)) %>%
      tab_options(table_body.hlines.style = "none") %>% 
      gt_theme_538()
  })
}

shinyApp(ui, server)

