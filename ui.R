library(shinydashboard)
library(DT)

AUDIO_FEATURES <- c("Danceability", "Energy", "Acousticness", "Liveness", "Valence")

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Spotify Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Anylize playlists", tabName = "playlist", icon = icon("list")),
      menuItem("Anylize artists", tabName = "artist", icon = icon("microphone-lines")),
      menuItem("About the dashboard", tabName = "about", icon = icon("address-card"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      # Playlist view
      tabItem(tabName = "playlist",
        fluidRow(align = "center",
          textInput('playlist_id', label = "Playlist id:", value = '37i9dQZEVXbN6itCcaL3Tt') %>% tagAppendAttributes(class = 'custom-text-input'),
          actionButton("submit_playlist", "Submit") %>% tagAppendAttributes(class = "submit-button")
        ),
        fluidRow(
          box(
            title = "Playlist Analysis",
            h3("Genre Distribution"),
            plotlyOutput("genre_distribution_plot"),
            h3("Average Audio Features"),
            checkboxGroupInput("average_chosen_features", label = NULL,
                               choices = AUDIO_FEATURES,
                               selected = AUDIO_FEATURES,
                               inline = TRUE),
            plotOutput("average_audio_features_plot"),
          ),
          box(
            title = "Track list",
            uiOutput("genre_selection"),
            DT::DTOutput("table_playlist"),
          ),
          div(
            class = "track-card",
            div(
              class = "track-info",
              htmlOutput("track_img"),
              htmlOutput("track_preview"),
              htmlOutput("author"),
              htmlOutput("track_name"),
              htmlOutput('album'),
            ),
            div(
              class = "track-plots",
              checkboxGroupInput("track_chosen_features", label = NULL,
                                 choices = AUDIO_FEATURES,
                                 selected = AUDIO_FEATURES,
                                 inline = TRUE),
              plotOutput("track_audio_features_plot"),
            )
          )
        )
      ),
      tabItem(tabName = "artist",
              fluidRow(align = "center",
                       textInput('artist_id', label = "Artist id:", value = '7CJgLPEqiIRuneZSolpawQ?') %>% tagAppendAttributes(class = 'custom-text-input'),
                       actionButton("submit_artist", "Submit") %>% tagAppendAttributes(class = "submit-button")
              ),
              fluidRow(
                box(
                  title = "Artist Information",
                  div(
                    class = "artist-card",
                    div(
                      class = "artist-info",
                      htmlOutput("artist_name"),
                      htmlOutput("artist_img"),
                    )

                  )
                ),
                box(
                  title = "Tracks Analysis",
                  selectInput("track_analysis_x_axis", "Select x-axis feature:", AUDIO_FEATURES, selected = "Valence"),
                  selectInput("track_analysis_y_axis", "Select y-axis feature:", AUDIO_FEATURES, selected = "Energy"),
                  plotlyOutput("tracks_scatter_plot")
                ),
              fluidRow(
                box(
                  title = "Album Comparison",
                  uiOutput("album_comparison_first"),
                  uiOutput("album_comparison_second"),
                  uiOutput("album_comparison_feature"),
                  plotOutput("album_comparison_plot")
                )
              )
        )
      )
    )
  )
)
