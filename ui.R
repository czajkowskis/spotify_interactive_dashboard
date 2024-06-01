library(shinydashboard)
library(DT)

AUDIO_FEATURES <- c("Danceability", "Energy", "Acousticness", "Liveness", "Valence")

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Spotify Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Anylize playlists", tabName = "playlist", icon = icon("list")),
      menuItem("Anylize artists", tabName = "artist", icon = icon("microphone-lines")),
      menuItem("About the dashboard", tabName = "about", icon = icon("circle-info"))
    ),
    div(
      tags$img(
        style = "padding: 5px; text-align: center; width: 200px;",
        src = "logo.png"
      )
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
        uiOutput("playlist_page")
      ),
      tabItem(tabName = "artist",
              fluidRow(align = "center",
                       textInput('artist_id', label = "Artist id:", value = '7CJgLPEqiIRuneZSolpawQ') %>% tagAppendAttributes(class = 'custom-text-input'),
                       actionButton("submit_artist", "Submit") %>% tagAppendAttributes(class = "submit-button")
              ),
              uiOutput("artist_page"),
      ),
      tabItem(tabName = "about",
        fluidRow(
          column(
            width = 12,
            h1("About Spotify Analyzer Dashboard", style = "text-align: center;"),
            p("The Spotify Analyzer Dashboard was created as part of the Data Visualization course at Poznan University of Technology.
            It uses Spotify API to fetch data about playlists, artists and albums based on IDs and present the data in clear and interactive manner."),
            p("The dashboard was divided into 2 parts - one regarding the playlist analysis and one regarding artist analysis."),
            h2("Playlist Analyzer:"),
            p("Playlist Analyzer was designed to present the data about the playlist chosen by the user. After providing the correct playlist ID the user can:"),
            tags$ul(
              tags$li(
                "View the pie chart describing the distribution of track genres in the playlist."
              ),
              tags$li(
                "View the interactive radar chart presenting the average audio features of all tracks in the playlist.
              The user can choose which audio features should be included in the chart."
              ),
              tags$li(
                "View the list of tracks in the playlist. After selecting the record from the table, the informative card
              about the track including basic information and a radar chart similar to the one described above but regarding
              only the specific track will be displayed. It is possible to listen to the song preview if it is available"
              ),
            ),
            h2("Artist Analyzer:"),
            p("Artist Analyzer presents the data about the artist chosen by the user. After providing the correct artist ID the user can:"),
            tags$ul(
              tags$li(
                "View the informative card summarising artist's releases"
              ),
              tags$li(
                "View the interactive scatter plot showing all the artist's regarding two selected audio features. 
              The user can freely change both the x-axis and the y-axis features."
              ),
              tags$li(
                "View the ridgeline plot comparing two chosen albums of the artist in terms of the chosen audio feature. 
                The user can choose both of the albums and the feature on which they are to be compared."
              ),
            ),
          )
         
        )
      )
    )
  )
)
