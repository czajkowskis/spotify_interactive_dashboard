library('DT')

playlist_page <- function(){
  fluidPage(  
    fluidRow(align = "center",
            h4("Enter the playlist ID to anylse its content"),
            textInput('playlist_id', label = "Playlist id:", value = '37i9dQZEVXbN6itCcaL3Tt'),
            actionButton("submit_playlist", "Submit"),
    ),
    fluidRow(align = "center",
             h4("Playlist statistics")
             ),
    fluidRow(align = "center",
             column(6, 
                    plotOutput("genre_pie_chart")
                    ),
             column(6,
                   plotOutput("average_audio_features_plot"))
            ),
    fluidRow(align = "center",
             h4("Track list")
    ),
    uiOutput("genre_selection"),
    DT::DTOutput("table_playlist"),
    fluidRow(align = 'center',
             htmlOutput("track_name"),
             htmlOutput("track_img"),
             htmlOutput('track_preview')
    ),
    fluidRow(
      column(6,
             plotOutput("audio_features_plot")
             ),
      column(4,
             plotOutput("track_popularity_vs_average_plot")
      ),
    )
    
  )
}
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Playlist", playlist_page()),
    tabPanel("Artist", 
             h2("Artist analyser"),
             textInput('artist_id', label = "Artist id:", value = '0iBTVnJ1Sff92zCDujfvyJ'),
             actionButton("submit_artist", "Submit")
    ),
    tabPanel("Song")
  )
)