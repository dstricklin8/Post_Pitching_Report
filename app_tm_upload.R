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
library(ggforce)
library(gt)
library(gtExtras)

load("data/info.rda")

pitch_colors <- c(
  "Sinker" = "#fe9d00",
  "Slider" = "#efe717",
  "ChangeUp" = "#1fbe3a",
  "FourSeamFastBall" = "#d22d4a",
  "Splitter" = "#3bacac",
  "Curveball" = "#04d1ed",
  "Cutter" = "#933f1f",
  "Fastball" = "red2",
  "TwoSeamFastBall" = "maroon3",
  "Undefined" = "darkgrey"
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
    fileInput("upload_1", "Upload TrackMan .csv File", accept = c(".csv"), placeholder = ""),
    selectizeInput("pitcher", "Select Pitcher", choices = NULL),
    selectInput("batter_stands", 
                label = "Batter Stands:",
                choices = c("All", "Right", "Left")),
    selectInput("pitch_tag", 
                label = "Pitch Tag Type:",
                choices = c("TaggedPitchType", "AutoPitchType")),
    actionButton("goButton_1", "Create Visual")
  ),
  # legend ----
  grid_card(
    "legend",
    card_header("Pitch Arsenal"),
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
  
  df_1 <- eventReactive(input$upload_1, {
    read.csv(input$upload_1$datapath)
  })
  
  df_upload <- reactive({
    filter(df_1(), !is.na(RelSpeed)) %>% 
      arrange(Pitcher)
  })
  observeEvent(df_upload(), {
    choices <- unique(df_upload()$Pitcher)
    updateSelectizeInput(inputId = "pitcher", choices = choices) 
  })
  
  data <- eventReactive(input$goButton_1, {
    
    sc_df <- df_1() %>% 
      filter(Pitcher == input$pitcher,
             !is.na(AutoPitchType),
             !is.na(TaggedPitchType),
             !AutoPitchType %in% c("Undefined", "Other"),
             !TaggedPitchType %in% c("Undefined", "Other"))
    
    if (input$batter_stands == "Right") {sc_df <- sc_df %>% filter(BatterStands == "Right")}
    else if (input$batter_stands == "Left") {sc_df <- sc_df %>% filter(BatterStands == "Left")}
    
    if (input$pitch_tag == "TaggedPitchType") {
      sc_df$pitch_tag <- sc_df$TaggedPitchType
    }
    else if (input$pitch_tag == "AutoPitchType") {
      sc_df$pitch_tag <- sc_df$AutoPitchType 
    }
    
    sc_df <- left_join(sc_df, pitch_colors_tibble, by = c("pitch_tag"))
    
    sc_df <- sc_df %>% 
      mutate(
        pitch_tag = fct_infreq(pitch_tag)
      )
    
    return(sc_df)
    
  })
  
  output$legend <- renderPlot({
    p1 <- ggplot(data()) +
      geom_point(aes(PlateLocSide*-12, PlateLocHeight*12, fill = pitch_tag), shape = 21, size = 6) +
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
    
    ggplot() +
      geom_point(data(),
                 mapping = aes(x = PlateLocSide*12, y = PlateLocHeight*12, fill = pitch_tag),
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
    
    ggplot() +
      geom_point(data() %>% filter(Strikes == 2),
                 mapping = aes(x = PlateLocSide*12, y = PlateLocHeight*12, fill = pitch_tag),
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
      geom_point(data() %>% filter(PitchCall %in% c("StrikeSwinging")),
                 mapping = aes(x = PlateLocSide*12, y = PlateLocHeight*12, fill = pitch_tag),
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
      geom_point(data() %>% filter(PitchCall %in% c("InPlay")),
                 mapping = aes(x = PlateLocSide*12, y = PlateLocHeight*12, fill = pitch_tag),
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
      geom_point(mapping = aes(HorzBreak, InducedVertBreak, fill = pitch_tag),
                 shape = 21, size = 3, show.legend = F) +
      coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30)) +
      theme_linedraw() +
      geom_vline(mapping = aes(xintercept = 0), linetype = 2, alpha = 0.5)+
      geom_hline(mapping = aes(yintercept = 0), linetype = 2, alpha = 0.5)+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = abs) +
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
      geom_circle(aes(x0=0, y0=-15.5, r=16), fill = "#ededed", color = "lightgrey") + 
      geom_point(aes(RelSide*-1, RelHeight, fill = pitch_tag),
                 shape = 21, size = 2, show.legend = F) +
      coord_cartesian(xlim = c(-4, 4), ylim = c(0, 10), expand = c(0)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = abs) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_fill_manual(values = pitch_colors) +
      theme_minimal() +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme(
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank()
      )
  })
  output$metrics <- render_gt({
   data() %>% 
      group_by(pitch_tag, pitch_hex) %>% 
      summarise(
        num_pitches = n(),
        max_rel_speed = max(RelSpeed, na.rm = T),
        RelSpeed = mean(RelSpeed, na.rm = T),
        HorzBreak = abs(mean(HorzBreak, na.rm = T)),
        InducedVertBreak = mean(InducedVertBreak, na.rm = T),
        SpinRate = mean(SpinRate, na.rm = T)
      ) %>% 
      arrange(pitch_tag) %>% 
      ungroup() %>% 
      mutate(
        color = ""
      ) %>% 
      gt()  %>% 
      cols_hide(columns = c(pitch_hex)) %>% 
      cols_label(
        color = "",
        pitch_tag = md(""),
        RelSpeed = md("Avg. Velo"),
        max_rel_speed = md("Max. Velo"),
        SpinRate = md("Spin Rate"),
        HorzBreak = md("HB"),
        InducedVertBreak = md("IVB"),
        num_pitches = md("Pitches")
      ) %>% 
      cols_move(
        columns = color,
        after = pitch_tag
      )  %>% 
      fmt_number(columns = c("RelSpeed", "max_rel_speed", "HorzBreak", "InducedVertBreak"), decimals = 1) %>%  
      fmt_number(columns = c("SpinRate"), decimals = 0) %>%  
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
      tab_options(table_body.hlines.style = "none")
    
    })
}

shinyApp(ui, server)

