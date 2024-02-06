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

load("data/info.rda")

ui = grid_page(
  layout = c(
    "|     | 250px  |1fr        |1fr       |1fr        |1fr    |
       |1fr  |sidebar |loc        |loc_2k    |ivb_hb     |rel_pt |
       |1fr  |sidebar |loc        |loc_2k    |ivb_hb     |rel_pt |
       |1fr  |legend  |loc_swings |loc_bip   |metrics    |metrics  |
       |1fr  |legend  |loc_swings |loc_bip   |metrics    |metrics  |"
  ),
  grid_card(
    "sidebar",
    card_header("Pitching Report"),
    fileInput("upload_1", "Upload .csv File", accept = c(".csv")),
    selectizeInput("pitcher",
                   label = "Select Pitcher",
                   choices = mlb_arms,
                   selected = NULL),
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
  # # velo_dist ----
  # grid_card(
  #   "velo_dist",
  #   card_header("Velocity Distribution"),
  #   card_body(
  #     plotOutput("velo_dist")
  #   )
  # ),
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
    
    df <- df_1()
    
    sc_df <- mutate_sc(df)
    
    sc_df <- sc_df %>% 
      filter(player_name == input$pitcher)
    
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
      geom_point(mapping = aes(pfx_x*-12, pfx_z*12, fill = pitch_name),
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
  # output$velo_dist <- renderPlot({
  #   ggplot(data()) +
  #     # geom_density(aes(RelSpeed, fill = AutoPitchType), show.legend = F, alpha = 0.7) +
  #     stat_density_ridges(aes(release_speed, pitch_name, fill = pitch_name), show.legend = F, alpha = 0.7,
  #                         quantile_lines = TRUE, quantiles = 0.5) +
  #     scale_fill_manual(values = pitch_colors) +
  #     theme_minimal() +
  #     labs(
  #       x = NULL,
  #       y = NULL
  #     ) +
  #     theme(
  #       panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()
  #     )
  # })
  # output$spin_dist <- renderPlot({
  #   ggplot(data()) +
  #     # geom_density() +
  #     stat_density_ridges(aes(release_spin_rate, pitch_name, fill = pitch_name), show.legend = F, alpha = 0.7,
  #                         quantile_lines = TRUE, quantiles = 0.5) +
  #     scale_fill_manual(values = pitch_colors) +
  #     theme_minimal() +
  #     labs(
  #       x = NULL,
  #       y = NULL
  #     ) +
  #     theme(
  #       panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()
  #     )
  # })
  
  output$metrics <- render_gt({
    
    data () %>% 
      group_by(pitch_name) %>% 
      summarise(
        num_pitches = n(),
        release_speed = mean(release_speed, na.rm = T),
        max_rel_speed = max(release_speed, na.rm = T),
        pfx_x = mean(pfx_x*12, na.rm = T),
        pfx_z = mean(pfx_z*12, na.rm = T),
        release_spin_rate = mean(release_spin_rate, na.rm = T)
      ) %>% 
      arrange(desc(num_pitches)) %>% 
      ungroup() %>% 
      gt()  %>% 
      cols_label(
        pitch_name = md(""),
        release_speed = md("Avg. Velo"),
        max_rel_speed = md("Max. Velo"),
        release_spin_rate = md("Spin Rate"),
        pfx_x = md("HB"),
        pfx_z = md("IVB"),
        num_pitches = md("Pitches")
      ) %>% 
      fmt_number(columns = c("release_speed", "max_rel_speed", "pfx_x", "pfx_z"), decimals = 1) %>%  
      fmt_number(columns = c("release_spin_rate"), decimals = 0) %>%  
      tab_options(table_body.hlines.style = "none")
  })
}

shinyApp(ui, server)
