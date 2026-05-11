library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(hexbin)
library(ggridges)
library(tidyr)
library(shinythemes)

load("data/nba_app_data.RData")

draw_court <- function() {
  theta <- seq(0, 2 * pi, length.out = 200)
  half <- seq(0, pi, length.out = 200)
  three_start <- acos(220 / 237.5)
  three_end <- pi - three_start
  three_theta <- seq(three_start, three_end, length.out = 200)
  list(
    annotate("path", x = 7.5 * cos(theta), y = 7.5 * sin(theta), color = "#333"),
    annotate("segment", x = -30, xend = 30, y = -7.5, yend = -7.5, color = "#333"),
    annotate("rect", xmin = -80, xmax = 80, ymin = -47.5, ymax = 143.5, fill = NA, color = "#333"),
    annotate("path", x = 60 * cos(half), y = 143.5 + 60 * sin(half), color = "#333"),
    annotate("segment", x = -220, xend = -220, y = -47.5, yend = 89.48, color = "#333"),
    annotate("segment", x = 220, xend = 220, y = -47.5, yend = 89.48, color = "#333"),
    annotate("path", x = 237.5 * cos(three_theta), y = 237.5 * sin(three_theta), color = "#333"),
    annotate("rect", xmin = -250, xmax = 250, ymin = -47.5, ymax = 422.5, fill = NA, color = "#333")
  )
}

# tab 1
tab1_ui <- tabPanel("About & Overview",
                    fluidPage(
                      fluidRow(
                        column(10, offset = 1,
                               br(),
                               h2("The Analytics Era: How Data Changed the NBA"),
                               p("Welcome to the NBA Statistical Explorer. This interactive dashboard is designed to let you visually explore the greatest paradigm shift in modern sports history: the spatial revolution of professional basketball."),
                               
                               h4("The Story: Pace, Space, and the 3-Point Boom"),
                               p("If you look at how basketball was played twenty years ago, the court was crowded. Teams relied heavily on physical centers posting up near the basket and guards shooting long, inefficient two-point jumpers. However, with the rise of modern data analytics, teams realized a simple mathematical truth: a 3-point shot is worth 50% more than a 2-point shot. This realization birthed the modern 'analytics' era."),
                               p("As you explore this tool, keep an eye on the explosion of the 3-point shot. Notice how the 'mid-range' area (shots taken between the paint and the 3-point line) completely evaporates over time. Players are now shooting from deeper, playing faster, and spreading the floor like never before."),
                               
                               hr(),
                               
                               h4("How to Use This Dashboard"),
                               tags$ul(
                                 tags$li(strong("Shot Chart Heat Map: "), "Select individual players to see their exact spatial footprint. Adjust the bins to see their favorite spots on the floor."),
                                 tags$li(strong("The Disappearing Mid-Range: "), "Use the checkboxes to select specific eras and watch the mid-range jumper literally vanish from the game. Notice the peaks shift toward the 3-point line (23.75+ feet)."),
                                 tags$li(strong("Era Shift: Shot Zones: "), "Directly compare the shot selection of any two seasons using the dumbbell plot. Watch how the percentage of 'Above the Break 3s' skyrockets while 'Mid-Range' plummets."),
                                 tags$li(strong("Historical League Trends: "), "Track macro-level box score statistics. Watch the league-average 3-point attempts climb, or select your favorite team to see their historical performance.")
                               ),
                               
                               hr(),
                               
                               h4("Data Sources & References"),
                               p("The data powering this application contains millions of play-by-play events and box score metrics sourced originally from the NBA Stats API. The data was cleaned, aggregated, and compressed into a lightweight RData format to allow for seamless interactive web exploration.")
                        )
                      )
                    )
)

# tab 2
tab2_ui <- tabPanel("Shot Chart Heat Map",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          h5("Spatial Controls"),
                          selectizeInput("shot_player", "Choose a Player", choices = NULL, options = list(placeholder = "Type a name...")),
                          selectInput("shot_season", "Choose a Season", choices = shot_seasons, selected = shot_seasons),
                          radioButtons("shot_outcome", "Shot Outcome", choices = c("All", "Made", "Missed"), selected = "All", inline = TRUE),
                          hr(),
                          sliderInput("hex_bins", "Detail Level (Hex Size)", min = 10, max = 40, value = 25, step = 5)
                        ),
                        mainPanel(
                          h4(textOutput("sc_title")),
                          p(textOutput("sc_summary")),
                          plotOutput("shot_chart", height = "600px")
                        )
                      )
                    )
)

# tab 3
tab3_ui <- tabPanel("The Disappearing Mid-Range",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          h5("Distribution Controls"),
                          checkboxGroupInput("ridge_years", "Select Seasons to Compare", 
                                             choices = sort(shot_seasons, decreasing = TRUE), 
                                             selected = c(max(shot_seasons), min(shot_seasons))),
                          hr(),
                          sliderInput("max_distance", "Max Distance (ft)", min = 25, max = 40, value = 35)
                        ),
                        mainPanel(plotOutput("ridgeline_plot", height = "750px"))
                      )
                    )
)

# tab 4
tab4_ui <- tabPanel("Era Shift: Shot Zones",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          h5("Era Comparison Controls"),
                          selectInput("db_year1", "Base Year (Blue)", choices = sort(shot_seasons), selected = min(shot_seasons)),
                          selectInput("db_year2", "Comparison Year (Red)", choices = sort(shot_seasons), selected = max(shot_seasons))
                        ),
                        mainPanel(plotlyOutput("dumbbell_plot", height = "600px"))
                      )
                    )
)

# tab 5
tab5_ui <- tabPanel("Historical League Trends",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          h5("Historical Stat Tracker"),
                          p("Track the evolution of the game using aggregate box score data."),
                          selectizeInput("hist_team", "Select Scope", choices = c("League Average", all_teams), selected = "League Average"),
                          selectInput("hist_metric", "Select Statistic", choices = metric_choices, selected = stat_cols),
                          hr(),
                          p(em("Note: Stats like 3-Pointers, Blocks, and Steals were not officially tracked in the early decades of the league."))
                        ),
                        mainPanel(plotlyOutput("hist_plot", height = "600px"))
                      )
                    )
)

# ui
ui <- navbarPage(
  title = "NBA Statistical Explorer",
  theme = shinytheme("yeti"),
  tab1_ui,
  tab2_ui,
  tab3_ui,
  tab4_ui,
  tab5_ui
)

# server
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "shot_player", choices = shot_players, selected = "LeBron James", server = TRUE)
  
  shot_data <- reactive({
    req(input$shot_player, input$shot_season)
    df <- shots %>% filter(player_name == input$shot_player, season == as.integer(input$shot_season))
    if (input$shot_outcome == "Made") df <- filter(df, shot_made == 1)
    if (input$shot_outcome == "Missed") df <- filter(df, shot_made == 0)
    df
  })
  
  output$sc_title <- renderText({ paste(input$shot_player, "-", input$shot_season, "Season") })
  output$sc_summary <- renderText({
    df <- shot_data()
    if (nrow(df) == 0) return("No shots found for this selection.")
    made <- sum(df$shot_made == 1)
    sprintf("%d shots recorded | %d made | %.1f%% FG", nrow(df), made, 100 * made / nrow(df))
  })
  
  output$shot_chart <- renderPlot({
    df <- shot_data()
    p <- ggplot(df, aes(x = loc_x, y = loc_y)) +
      draw_court() + coord_fixed(xlim = c(-260, 260), ylim = c(-60, 420)) +
      theme_void() + theme(panel.background = element_rect(fill = "#fcfcfc", color = NA), legend.position = "bottom")
    
    if (nrow(df) > 0) {
      p <- p + geom_hex(bins = input$hex_bins, alpha = 0.9) + scale_fill_viridis_c(option = "inferno", name = "Shot Volume")
    }
    p
  })
  
  output$ridgeline_plot <- renderPlot({
    req(input$ridge_years, input$max_distance)
    
    selected_years <- as.integer(input$ridge_years)
    
    df <- shots %>%
      filter(season %in% selected_years, shot_distance_ft <= input$max_distance) 
    
    validate(need(nrow(df) > 0, "No data available for the selected seasons."))
    
    df$season_fac <- factor(df$season, levels = sort(selected_years))
    
    ggplot(df, aes(x = shot_distance_ft, y = season_fac, fill = after_stat(x))) +
      geom_density_ridges_gradient(scale = 3.5, rel_min_height = 0.005, color = "black", linewidth = 0.3) +
      geom_vline(xintercept = 23.75, linetype = "dashed", color = "black", linewidth = 0.8) +
      annotate("text", x = 23.75, y = 1, label = " 3pt line", angle = 90, hjust = 0, vjust = -0.5, fontface = "bold") +
      scale_fill_viridis_c(option = "plasma", direction = -1) +
      scale_x_continuous(breaks = seq(0, 40, by = 5)) +
      scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) + 
      labs(title = "The Disappearing Mid-Range Jumper", subtitle = "Distribution of NBA shot distances by season", x = "Shot Distance (ft)", y = NULL) +
      theme_ridges(font_size = 13, grid = FALSE) + 
      theme(legend.position = "none", plot.title = element_text(face = "bold", size = 20), axis.text.y = element_text(vjust = 0))
  })
  
  output$dumbbell_plot <- renderPlotly({
    req(input$db_year1, input$db_year2)
    y1 <- as.integer(input$db_year1)
    y2 <- as.integer(input$db_year2)
    df <- zone_freq %>% filter(season %in% c(y1, y2))
    
    validate(
      need(y1 %in% df$season, paste("No shot location data available for", y1)),
      need(y2 %in% df$season, paste("No shot location data available for", y2))
    )
    
    df_wide <- df %>% select(season, shot_zone, pct_of_total) %>% pivot_wider(names_from = season, values_from = pct_of_total, names_prefix = "year_")
    col_y1 <- sym(paste0("year_", y1))
    col_y2 <- sym(paste0("year_", y2))
    
    df_wide <- df_wide %>% mutate(diff = coalesce(!!col_y2, 0) - coalesce(!!col_y1, 0)) %>% arrange(diff)
    df_wide$shot_zone <- factor(as.character(df_wide$shot_zone), levels = unique(as.character(df_wide$shot_zone)))
    
    p <- ggplot(df_wide) +
      geom_segment(aes(x = !!col_y1, xend = !!col_y2, y = shot_zone, yend = shot_zone), color = "gray70", linewidth = 2) +
      geom_point(aes(x = !!col_y1, y = shot_zone, text = paste(y1, ": ", percent(!!col_y1, 0.1))), color = "#3498db", size = 5) +
      geom_point(aes(x = !!col_y2, y = shot_zone, text = paste(y2, ": ", percent(!!col_y2, 0.1))), color = "#e74c3c", size = 5) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = paste(y1, "vs.", y2, "Shot Selection"), subtitle = "Share of all NBA shot attempts by zone", x = "Percentage of Total Shots", y = NULL) +
      theme_minimal() + theme(plot.title = element_text(face = "bold", size = 16), panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$hist_plot <- renderPlotly({
    req(input$hist_metric, input$hist_team)
    
    metric_name <- names(metric_choices)[which(metric_choices == input$hist_metric)]
    
    if (input$hist_team == "League Average") {
      plot_data <- league_season
      title_prefix <- "League Average:"
      line_color <- "#27ae60"
      area_fill <- "#2ecc71"
    } else {
      plot_data <- team_season %>% filter(team == input$hist_team)
      title_prefix <- paste(input$hist_team, "Average:")
      line_color <- "#2980b9"
      area_fill <- "#3498db"
    }
    
    p <- ggplot(plot_data, aes(x = season, y = !!sym(input$hist_metric), 
                               text = paste("Season:", season, "<br>", metric_name, ":", round(!!sym(input$hist_metric), 2)))) +
      geom_area(fill = area_fill, alpha = 0.3) +
      geom_line(color = line_color, linewidth = 1.2) +
      geom_point(color = "#2c3e50", size = 1.5) +
      scale_x_continuous(breaks = seq(min(plot_data$season), max(plot_data$season), by = 5)) +
      labs(title = paste(title_prefix, metric_name), x = "Season", y = metric_name) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16))
    
    if(grepl("pct", input$hist_metric)) {
      p <- p + scale_y_continuous(labels = percent_format(accuracy = 0.1))
    }
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
  })
}

shinyApp(ui, server)