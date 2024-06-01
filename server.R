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
  
  dance_avg <<- round(mean(danceability), 2)
  energy_avg <<- round(mean(energy), 2)
  valence_avg <<- round(mean(valence), 2)
  liveness_avg <<- round(mean(liveness), 2)
  acousticness_avg <<- round(mean(acousticness), 2)
  
  tracks_info = data.frame(Name = playlist$track.name,
                           Album_name = playlist$track.album.name, 
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
  genres_info = top_n(genres_info, 5)
  
  plot_ly(labels = genres_info$genre, values = genres_info$nr_of_occ, type = "pie")
}

generate_audio_features_plot <- function(danceability, energy, acousticness, liveness, valence){
  audio_features = data.frame(Danceability = danceability, 
                              Energy = energy, 
                              Acousticness = acousticness, 
                              Liveness = liveness, 
                              Valence = valence)
  par(bg = NULL)
  radarchart(rbind(rep(1,length(audio_features)) , rep(0,length(audio_features)), audio_features),
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


server <- function(input, output) {
  
  # Defining global data frames
  tracks_info = data.frame()
  artist_audio_features = data.frame()
  
  # Selecting x and y axis for track analysis plot
  selected_x_feature <- reactive({ return(input$track_analysis_x_axis)})
  selected_y_feature <- reactive({return(input$track_analysis_y_axis)})
  
  # Selecting feature for album comparison
  
  album_comparison_feature <- reactive({return(input$album_comparison_feature)})
  
  # Displaying playlist info in the form of DTable
  observeEvent(input$submit_playlist, {
    print(input$playlist_id)
    pl_info = get_playlist(input$playlist_id)
    tracks_info <<- get_tracks_info_from_playlist(get_playlist_tracks(playlist_id = input$playlist_id))
    output$genre_selection <- renderUI({
      selectInput("genre_selection", "Select genre", c("All genres", unique(tracks_info$genre)), selected = "All genres")
    })
    
    # Playlist box
    output$genre_distribution_plot = renderPlotly(generate_genre_distribution_chart(tracks_info))
    output$average_audio_features_plot = renderPlot(generate_audio_features_plot(mean(as.numeric(tracks_info$danceability)), 
                                                                                 mean(as.numeric(tracks_info$energy)), 
                                                                                 mean(as.numeric(tracks_info$acousticness)), 
                                                                                 mean(as.numeric(tracks_info$liveness)), 
                                                                                 mean(as.numeric(tracks_info$valence))))
    # Track-list box
    output$genre_selection <- renderUI({
      selectInput("genre_selection", "Select genre", c("All genres", unique(tracks_info$genre)), selected = "All genres")
    })
    output$table_playlist = DT::renderDT(tracks_info[,c("Name","Album_name", "genre")], rownames = FALSE, selection = 'single')
  })
  
  # Change displayed tracks by genre
  observeEvent(input$genre_selection,{
    if(input$genre_selection == "All genres"){
      output$table_playlist = DT::renderDT(tracks_info[,c("Name","Album_name", "genre")], rownames = FALSE, selection = 'single')
    } else{
      output$table_playlist = DT::renderDT(tracks_info %>% dplyr::filter(genre == input$genre_selection) %>% select("Name", "Album_name", "genre"),  rownames = FALSE, selection = 'single')
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
    output$track_name = renderText(paste('<h3 class = "track-title">', selected_track$Name, '</h3>'))
    output$track_img = renderText({c('<img src="', selected_track$img_url,'" style="width: 300px; height: 300px;"/>')})
    output$track_preview = renderText(paste0('<audio class = "audio-player" src="', selected_track$preview_url,'"controls> </audio>'))
    output$track_audio_features_plot = renderPlot({generate_audio_features_plot(selected_track$danceability, 
                                                                          selected_track$energy, 
                                                                          selected_track$acousticness, 
                                                                          selected_track$liveness, 
                                                                          selected_track$valence)})
  })
  
  # Generating artist page
  observeEvent(input$submit_artist,{
    artist_info <<- get_artist_information(input$artist_id)
    print(input$artist_id)
    artist_audio_features <<- get_artist_audio_features(artist_info$id)
    print(artist_audio_features)
    output$artist_name = renderText({c('<h3 style="font-weight: bold">', artist_info$name, '</h3>')})
    output$artist_img = renderText({c('<img src="',artist_info$img_url,'" style="width: 300px; height: 300px;"/>')})
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


