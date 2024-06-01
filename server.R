library('dplyr')
library('DT')
library('tibble')
library('spotifyr')
library('fmsb')
library('ggplot2')
library('plotly')
library('tools')
library(ggjoy)

AUDIO_FEATURES <- c("Danceability", "Energy", "Acousticness", "Liveness", "Valence")

my_palette <- c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7")

# Geting information from the Spotify API
get_tracks_info_from_playlist <- function(playlist){
  img_urls = c()
  for(img_urls_list in playlist$track.album.images){
    img_urls = c(img_urls, img_urls_list$url[1])
  }
  
  danceability = c()
  energy = c()
  acousticness = c()
  liveness = c()
  valence = c()
  tempo  = c()
  popularity = c()
  tracks_ids = c()
  inst = c()
  loudness = c()
  speech = c()
  
  for(track_id in playlist$track.id){
    tracks_ids = c(tracks_ids, track_id)
    audio_features <- get_track_audio_features(track_id)
    danceability = c(danceability, audio_features$danceability)
    energy = c(energy, audio_features$energy)
    acousticness = c(acousticness, audio_features$acousticness)
    liveness = c(liveness, audio_features$liveness)
    valence = c(valence, audio_features$valence)
    tempo = c(tempo, audio_features$tempo)
    #inst = c(inst, audio_features$instrumentalness)
    loudness = c(loudness, audio_features$loudness)
    #speech = c(energy, audio_features$energy)
  }
  
  genres = c()
  
  for(artists in playlist$track.artists){
    artist_id = artists$id[1]
    artist = get_artist(artist_id)
    if(length(artist$genres) < 1){
      genres = c(genres, "Not specified")
    }
    else{
      genres = c(genres, toTitleCase(artist$genres[[1]]))
    }
  }
  
  artists_comma_separated = c()
  for(artists in playlist$track.artists){
    current_artists_record = ""
    for(artist_name in artists$name){
      if (current_artists_record == ""){
        current_artists_record = artist_name
      }else{
        current_artists_record = paste(current_artists_record, artist_name, sep = ", ")
      }
    }
    artists_comma_separated = c(artists_comma_separated, current_artists_record)
  }

  dance_avg <<- round(mean(danceability), 2)
  energy_avg <<- round(mean(energy), 2)
  valence_avg <<- round(mean(valence), 2)
  liveness_avg <<- round(mean(liveness), 2)
  acousticness_avg <<- round(mean(acousticness), 2)
  
  tracks_info = data.frame(Name = playlist$track.name,
                           Album_name = playlist$track.album.name, 
                           artists = artists_comma_separated,
                           genre = genres, 
                           img_url = img_urls, 
                           preview_url = playlist$track.preview_url, 
                           popularity = playlist$track.popularity, 
                           danceability = danceability, 
                           energy = energy, 
                           acousticness = acousticness, 
                           liveness = liveness, 
                           valence = valence, 
                           tempo = tempo,
                           duration_ms = playlist$track.duration_ms,
                           track_id = tracks_ids,
                           loudn = loudness)
  return(tracks_info)
}


# Get information of an artist based on his/her ID
get_artist_information <- function(artist_id){
  artist = get_artist(artist_id)
  artist_info = data.frame(name = artist$name, 
                           id = artist$id,
                           followers = artist$followers$total, 
                           popularity = artist$popularity, 
                           img_url = artist$images$url[1])
  return(artist_info)
}

# PLOTS

generate_genre_distribution_chart <- function(tracks_info){
  genres_info = tracks_info %>% select(genre) %>% group_by(genre) %>% summarise(nr_of_occ = n())
  genres_info = arrange(genres_info, nr_of_occ)
  top_5_genres = top_n(genres_info, 5)
  
  plot_data = add_row(top_5_genres, genre = "Other", nr_of_occ = nrow(genres_info %>% filter(!genre %in% top_5_genres)))
  
  plot_ly(labels = plot_data$genre, values = plot_data$nr_of_occ, marker = list(colors = my_palette), type = "pie")
}

generate_average_audio_features_plot <- function(danceability, energy, acousticness, liveness, valence, chosen_features){
  audio_features = data.frame(Danceability = danceability, 
                              Energy = energy, 
                              Acousticness = acousticness, 
                              Liveness = liveness, 
                              Valence = valence)
  par(bg = NULL)

  filtered_df = audio_features %>% select(chosen_features)
  radarchart(rbind(rep(1, length(filtered_df)), rep(0, length(filtered_df)), filtered_df),
             axistype=1,
             pcol=rgb(0.84,0.28,0.84,0.7) , pfcol=rgb(0.84,0.28,0.84,0.5)  , plwd=4 , 
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8
  )
}

generate_track_audio_features_plot <- function(danceability, energy, acousticness, liveness, valence, chosen_features){
  audio_features = data.frame(Danceability = danceability, 
                              Energy = energy, 
                              Acousticness = acousticness, 
                              Liveness = liveness, 
                              Valence = valence)
  par(bg = NULL)
  filtered_df = audio_features %>% select(chosen_features)
  radarchart(rbind(rep(1, length(filtered_df)), rep(0, length(filtered_df)), filtered_df),
             axistype=1,
             pcol=rgb(0.84,0.28,0.84,0.7) , pfcol=rgb(0.84,0.28,0.84,0.5)  , plwd=4 , 
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8
  )
}


generate_tracks_scatter_plot <- function(audio_features, x_axis_feature, y_axis_feature){
  plot <- plot_ly(data = audio_features, 
                  type = "scatter", 
                  mode = "markers", 
                  x = audio_features[[tolower(x_axis_feature())]], y = audio_features[[tolower(y_axis_feature())]],  # Change to dynamic input
                  color = ~album_name,
                  text = paste("<b>Track Title:</b>", audio_features[["track_name"]], "\n<b>Album:</b>", audio_features[["album_name"]], "\n<b>", 
                               x_axis_feature(), ":</b>", audio_features[[tolower(x_axis_feature())]], "\n<b>", y_axis_feature(),":</b>", audio_features[[tolower(y_axis_feature())]]),
                  hoverinfo = 'text')
  plot %>% layout(
    xaxis = list(title = x_axis_feature()),  # Use labels from the map
    yaxis = list(title = y_axis_feature())  # Use labels from the map
  )
}

generate_ridgeline_album_comparison_plot <- function(data, chosen_feature){
  ggplot(data, aes(x = data[[tolower(chosen_feature())]], y = album_name, fill = as.factor(album_name))) + 
    geom_joy() + 
    theme_joy() +
    xlim(0, 1) +
    scale_fill_manual(values = c("#1ED760", "#B9A5E2")) + 
    labs(x = chosen_feature(), y = "Album names", fill = "Album name")
}

# Generate UI only after submition

generate_playlist_page_UI <- function(){
  renderUI({
    fluidRow(
      box(
        width = 5,
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
        width = 7,
        title = "Track list",
        uiOutput("genre_selection"),
        DT::DTOutput("table_playlist"),
      ),
      uiOutput("track_card_UI"),
    )
  })
}

generate_track_card <- function(){
  renderUI({box(
    width = 7,
    class = "track-card",
    div(
      class = "track-info",
      htmlOutput("track_name"),
      htmlOutput("track_artists"),
      htmlOutput("track_img"),
      htmlOutput("track_preview"),
      htmlOutput("author"),
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
  )})
}

generate_artsit_page_UI <- function(){
  renderUI({
           fluidRow(
             box(
               width = 5,
               # title = "Artist Information",
               div(
                 class = "artist-card",
                 column(
                   width = 6,
                   class = "artist-info",
                   htmlOutput("artist_name"),
                   htmlOutput("artist_img"),
                 ),
                 column(
                   width = 6,
                   class = "value-boxes",
                   fluidRow(
                     valueBoxOutput("popularity_box", width = 12) %>% tagAppendAttributes(class = "custom-value-box"),
                   ),
                   fluidRow(
                     valueBoxOutput("total_albums_box", width = 12) %>% tagAppendAttributes(class = "custom-value-box"),
                   ),
                   fluidRow(
                     valueBoxOutput("total_tracks_box", width = 12) %>% tagAppendAttributes(class = "custom-value-box")
                   )
                 )
                 
               )
             ),
             box(
               width = 7,
               title = "Tracks Analysis",
               fluidRow(
                 column(
                   width = 6,
                   selectInput("track_analysis_x_axis", "Select x-axis feature:", AUDIO_FEATURES, selected = "Valence"),
                 ),
                 column(
                   width = 6,
                   selectInput("track_analysis_y_axis", "Select y-axis feature:", AUDIO_FEATURES, selected = "Energy"),
                 ),
               ),
               plotlyOutput("tracks_scatter_plot")
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Album Comparison",
                 fluidRow(
                   column(
                     width = 6,
                     uiOutput("album_comparison_first"),
                   ),
                   column(
                     width = 6,
                     uiOutput("album_comparison_second"),
                   ),
                 ),
                 uiOutput("album_comparison_feature"),
                 plotOutput("album_comparison_plot")
               )
             )
           )})
}


server <- function(input, output) {
  
  # Defining global data frames
  tracks_info = data.frame()
  artist_audio_features = data.frame()
  
  # Selecting features to be displayed on radar plot
  average_chosen_features <- reactive(input$average_chosen_features)
  track_chosen_features <- reactive(input$track_chosen_features)
  
  # Selecting x and y axis for track analysis plot
  selected_x_feature <- reactive({ return(input$track_analysis_x_axis)})
  selected_y_feature <- reactive({return(input$track_analysis_y_axis)})
  
  
  # Selecting feature for album comparison
  
  album_comparison_feature <- reactive({return(input$album_comparison_feature)})
  
  # Displaying playlist info in the form of DTable
  observeEvent(input$submit_playlist, {
    pl_info = get_playlist(input$playlist_id)
    tracks_info <<- get_tracks_info_from_playlist(get_playlist_tracks(playlist_id = input$playlist_id))
    output$playlist_page <- generate_playlist_page_UI()
    output$genre_selection <- renderUI({
      selectInput("genre_selection", "Select genre", c("All genres", unique(tracks_info$genre)), selected = "All genres")
    })
    
    # Playlist box
    output$genre_distribution_plot = renderPlotly(generate_genre_distribution_chart(tracks_info))
    output$average_audio_features_plot = renderPlot(generate_average_audio_features_plot(mean(as.numeric(tracks_info$danceability)), 
                                                                                 mean(as.numeric(tracks_info$energy)), 
                                                                                 mean(as.numeric(tracks_info$acousticness)), 
                                                                                 mean(as.numeric(tracks_info$liveness)), 
                                                                                 mean(as.numeric(tracks_info$valence)),
                                                                                average_chosen_features()))
    # Track-list box
    output$genre_selection <- renderUI({
      selectInput("genre_selection", "Select genre", c("All genres", unique(tracks_info$genre)), selected = "All genres")
    })
    output$table_playlist = DT::renderDT(tracks_info[,c("Name","Album_name", "artists", "genre")], colnames = c("Track Title", "Album Title", "Artists", "Genre"), rownames = FALSE, selection = 'single')
  })
  
  # Change displayed tracks by genre
  observeEvent(input$genre_selection,{
    if(input$genre_selection == "All genres"){
      output$table_playlist = DT::renderDT(tracks_info[,c("Name","Album_name", "artists", "genre")], colnames = c("Track Title", "Album Title", "Artists", "Genre"), rownames = FALSE, selection = 'single')
    } else{
      output$table_playlist = DT::renderDT(tracks_info %>% dplyr::filter(genre == input$genre_selection) %>% select("Name", "Album_name", "artists", "genre"), colnames = c("Track Title", "Album Title", "Artists", "Genre"),  rownames = FALSE, selection = 'single')
    }
  })
  
  # Displaying the selected track info
  observeEvent(input$table_playlist_rows_selected, {
    if(input$genre_selection == "All genres"){
      selected_track <- tracks_info[input$table_playlist_rows_selected, ]
    } else{
      filtered_tracks <- tracks_info %>% dplyr::filter(genre == input$genre_selection)
      selected_track <- filtered_tracks[input$table_playlist_rows_selected, ]
    }
    output$track_card_UI <- generate_track_card()
    output$track_name = renderText({c('<h3 style="font-weight: bold">', selected_track$Name, '</h3>')})
    output$track_artists = renderText({c('<h4>', selected_track$artists, '</h>')})
    output$track_img = renderText({c('<img src="', selected_track$img_url,'" style="width: 300px; height: 300px;"/>')})
    output$track_preview = renderText(paste0('<audio class = "audio-player" src="', selected_track$preview_url,'"controls> </audio>'))
    output$track_audio_features_plot = renderPlot({generate_track_audio_features_plot(selected_track$danceability, 
                                                                                selected_track$energy, 
                                                                                selected_track$acousticness, 
                                                                                selected_track$liveness, 
                                                                                selected_track$valence,
                                                                                track_chosen_features())})
  })
  
  # Generating artist page
  observeEvent(input$submit_artist,{
    artist_info <<- get_artist_information(input$artist_id)
    artist_audio_features <<- get_artist_audio_features(artist_info$id)
    output$artist_page <- generate_artsit_page_UI()
    output$artist_name = renderText({c('<h3 style="font-weight: bold">', artist_info$name, '</h3>')})
    output$artist_img = renderText({c('<img src="',artist_info$img_url,'" style="width: 300px; height: 300px;"/>')})
    output$popularity_box <- renderValueBox({
      valueBox(value = artist_info$popularity, subtitle = "Popularity", icon = icon("arrow-trend-up"), color = "green")
    })
    output$total_albums_box <- renderValueBox({
      valueBox(value = length(unique(artist_audio_features$album_name)), subtitle = "Number of released albums", icon = icon("record-vinyl"), color = "purple")
    })
    output$total_tracks_box <- renderValueBox({
      valueBox(value = length(unique(artist_audio_features$track_name)), subtitle = "Number of released tracks", icon = icon("itunes-note"), color = "teal")
    })
    output$tracks_scatter_plot = renderPlotly(generate_tracks_scatter_plot(artist_audio_features, reactive({input$track_analysis_x_axis}), reactive({input$track_analysis_y_axis})))
    output$album_comparison_first = renderUI({
      selectInput("first_album_comparison", "Select first album to compare:", unique(artist_audio_features$album_name))
    })
    output$album_comparison_second = renderUI({
      selectInput("second_album_comparison", "Select second album to compare:", unique(artist_audio_features$album_name))
    })
    output$album_comparison_feature = renderUI({
      selectInput("album_comparison_feature", "Select the feature to compare the albums on:", AUDIO_FEATURES)
    })
    output$album_comparison_plot = renderPlot(generate_ridgeline_album_comparison_plot(artist_audio_features %>% filter(album_name == input$first_album_comparison | album_name == input$second_album_comparison),
                                                                                       reactive({input$album_comparison_feature})
    ))
  })
  
}


