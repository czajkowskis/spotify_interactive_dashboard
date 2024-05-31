library('dplyr')
library('DT')
library('tibble')
library('spotifyr')
library('fmsb')
library('ggplot2')
library('plotly')

dance_avg = 0
energy_avg = 0
valence_avg = 0
liveness_avg = 0
acousticness_avg = 0

#get artist names whose tracks are on playlist
get_artists_from_playlist <- function(playlist){
  artists = data.frame(name=character(0))
  print("Getting playlist info")
  for(artist_list in playlist$track.artists){
    for(artist_name in artist_list$name){
      artists <- add_row(artists, name = artist_name)
    }
  }
  return(artists)
}

#Get: Name, genre, icon, audio, popularity, danceability, energy, acousticness, 
#liveness, valence, tempo, duration_ms
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
      genres = c(genres, artist$genres[[1]])
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

#Generate fancy radar plot
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

#Generate violin plot of duration
generate_duration_violin <- function(tracks_info){
  duration = select(tracks_info, duration_ms) %>% mutate(duration_s = round(duration_ms/1000)) %>% select(duration_s)
  violin_plot <- plot_ly(duration, y = ~duration_s, type = 'violin', box = list(visible = T),
                         meanline = list(visible = T), points = 'all', jitter = 0.3, pointpos = -1.8) %>%
    layout(title = 'Song Duration Distribution in Playlist',
           yaxis = list(title = 'Duration (seconds)'))
  return (violin_plot)
}

#Generate boxplots for loudness
generate_boxplots <- function(tracks_info){
  loudness = select(tracks_info, loudn)
  box_plot_loud <- plot_ly(loudness, y = ~loudn, type = 'box', boxpoints = FALSE) %>%
    layout(title = 'Loudness Distribution in Playlist\n(negative values mean that loudness is below reference level)',
           yaxis = list(title = 'Loudness (dB)'))
  return (box_plot_loud)
}

#Generate genre piechart
generate_genre_pie_chart <- function(tracks_info){
  genres_info = tracks_info %>% select(genre) %>% group_by(genre) %>% summarise(nr_of_occ = n())
  genres_info = arrange(genres_info, nr_of_occ)
  genres_info = top_n(genres_info, 5)

  pie_chart = plot_ly(labels = genres_info$genre, values = genres_info$nr_of_occ, type = "pie")
  return(pie_chart)
}

#Get information of an artist based on his/her ID
get_artist_information <- function(artist_id){
  artist = get_artist(artist_id)
  artist_info = data.frame(name = artist$name, 
                           followers = artist$followers$total, 
                           popularity = artist$popularity, 
                           img_url = artist$images$url[1])
  return(artist_info)
}

#Generate scatter plot (second card)
generate_audio_scatter_plot <- function(audio_features, x_axis_feature, y_axis_feature){
  plot_ly(data = audio_features, 
          type = "scatter", 
          mode = "markers", 
          x = ~valence, 
          y = ~energy, 
          color = ~album_name,
          text = ~track_name)
}

server <- function(input, output) {
  tracks_info = data.frame()
  artist_info = data.frame()
  # Displaying playlist info in the form of DTable
  observeEvent(input$submit_playlist, {
    print(input$playlist_id)
    pl_info = get_playlist(input$playlist_id)
    tracks_info <<- get_tracks_info_from_playlist(get_playlist_tracks(playlist_id = input$playlist_id))
    output$genre_selection <- renderUI({
      selectInput("genre_selection", "Select genre", c("All genres", unique(tracks_info$genre)), selected = "All genres")
    })
    output$table_playlist = DT::renderDT(tracks_info[,c("Name","Album_name", "genre")], rownames = FALSE, selection = 'single')    
    output$genre_pie_chart = renderPlotly(generate_genre_pie_chart(tracks_info))
    output$average_audio_features_plot = renderPlot(generate_audio_features_plot(mean(as.numeric(tracks_info$danceability)), 
                                                                                 mean(as.numeric(tracks_info$energy)), 
                                                                                 mean(as.numeric(tracks_info$acousticness)), 
                                                                                 mean(as.numeric(tracks_info$liveness)), 
                                                                                 mean(as.numeric(tracks_info$valence))), height = 300, width = 300)
    output$duration_violin = renderPlotly(generate_duration_violin(tracks_info))
    output$box = renderPlotly(generate_boxplots(tracks_info))
    output$descr = renderText(paste('<h4 style="font-weight: bold; display: inline;">Description: </h4>',
                     '<h4 style="font-weight: normal;display: inline;">', pl_info$description, '</h4>'))
    output$nr_of_songs = renderText(paste('<h4 style="font-weight: bold; display: inline;">Number of songs: </h4>',
                                          '<h4 style="font-weight: normal;display: inline;">', pl_info$tracks$total, '</h4>'))
    output$author_of_pl = renderText(paste('<h4 style="font-weight: bold; display: inline;">Author of playlist: </h4>',
                                           '<h4 style="font-weight: normal;display: inline;">', pl_info$owner$display_name, '</h4>'))
    if (pl_info$public == TRUE){
      type = "Public"
    }else{
      type = "Private"
    }
    output$type_of_pl = renderText(paste('<h4 style="font-weight: bold; display: inline;">Type of playlist: </h4>',
                     '<h4 style="font-weight: normal;display: inline;">', type, '</h4>'))
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
    output$track_name = renderText(paste('<h4 style="font-weight: bold; display: inline;">Title: </h4>',
                                    '<h4 style="font-weight: normal;display: inline;">', selected_track$Name, '</h4>'))
    output$track_img = renderText({c('<img src="', selected_track$img_url,'" style="width: 300px; height: 300px;"/>')})
    output$audio_features_plot = renderPlot({generate_audio_features_plot(selected_track$danceability, selected_track$energy, selected_track$acousticness, selected_track$liveness, selected_track$valence)}, height = 300, width = 300)
    
    output$track_preview = renderText(paste0('<audio src="', selected_track$preview_url,'"controls> </audio>'))
    #TODO: write code so it can display this info
    ###output$release_date = renderText(paste('<h4 style="font-weight: bold">',selected_track$,"</h4>"))
    
    ID_of_song = selected_track$track_id
    output$ID_of_song = renderText(paste('<h4 style="font-weight: bold">',ID_of_song,"</h4>"))
    
    track_info = get_track(ID_of_song)
    artist_curr = ""
    for(art in track_info$artists$name){
      if (artist_curr == ""){
        artist_curr = art
      }else{
        artist_curr = paste(artist_curr, art, sep = ", ")
      }
    }

    output$author = renderText(paste('<h4 style="font-weight: bold; display: inline;">Artist: </h4>',
                                    '<h4 style="font-weight: normal;display: inline;">', artist_curr, '</h4>'))
    
    output$album = renderText(paste('<h4 style="font-weight: bold; display: inline;">Album: </h4>',
                                    '<h4 style="font-weight: normal;display: inline;">', selected_track$Album_name, '</h4>'))
    duration_s = round(selected_track$duration/1000, 0)
    mins = round(duration_s/60, 0)
    secs = duration_s %% 60
    output$length = renderText(paste('<h4 style="font-weight: bold; display: inline;">Duration: </h4>',
                                    '<h4 style="font-weight: normal;display: inline;">', paste(mins, " min ", secs, " s", sep = ""), '</h4>'))
    
    #output$playlist_img = renderText({c('<img src="', get_playlist_cover_image(input$playlist_id)[1],'" style="width: 300px; height: 300px;"/>')})

    #Comparison of danceability
    if(selected_track$danceability >= dance_avg + 0.1){
      output$d = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Danceability: </h4>',
                                  '<h4 style="font-weight: bold; display: inline; color: lime">Above average</h4>'))
    }else{
      if(selected_track$danceability <= dance_avg - 0.1){
        output$d = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Danceability: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: red">Below average</h4>'))
      }else{
        output$d = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Danceability: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: grey">Average</h4>'))
      }
    }
    
    #comparison of Energy
    if(selected_track$energy >= energy_avg + 0.1){
      output$e = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Energy: </h4>',
                                  '<h4 style="font-weight: bold; display: inline; color: lime">Above average</h4>'))
    }else{
      if(selected_track$energy <= energy_avg - 0.1){
        output$e = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Energy: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: red">Below average</h4>'))
      }else{
        output$e = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Energy: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: grey">Average</h4>'))
      }
    }
    
    #Comparison of Valence
    if(selected_track$valence >= valence_avg + 0.1){
      output$v = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Valence: </h4>',
                                  '<h4 style="font-weight: bold; display: inline; color: lime">Above average</h4>'))
    }else{
      if(selected_track$valence <= valence_avg - 0.1){
        output$v = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Valence: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: red">Below average</h4>'))
      }else{
        output$v = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Valence: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: grey">Average</h4>'))
      }
    }
    
    #Comparison of acousticness
    if(selected_track$acousticness >= acousticness_avg + 0.1){
      output$a = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Acousticness: </h4>',
                                  '<h4 style="font-weight: bold; display: inline; color: lime">Above average</h4>'))
    }else{
      if(selected_track$acousticness <= acousticness_avg - 0.1){
        output$a = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Acousticness: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: red">Below average</h4>'))
      }else{
        output$a = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Acousticness: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: grey">Average</h4>'))
      }
    }
    
    #Comparison of liveness
    if(selected_track$liveness >= liveness_avg + 0.1){
      output$l = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Liveness: </h4>',
                                  '<h4 style="font-weight: bold; display: inline; color: lime">Above average</h4>'))
    }else{
      if(selected_track$liveness <= liveness_avg - 0.1){
        output$l = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Liveness: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: red">Below average</h4>'))
      }else{
        output$l = renderText(paste('<h4 style="font-weight: bold; display: inline; color: black">Liveness: </h4>',
                                    '<h4 style="font-weight: bold; display: inline; color: grey">Average</h4>'))
      }
    }
    })
  
  observeEvent(input$submit_artist,{
    artist_info <<- get_artist_information(input$artist_id)
    print(input$artist_id)
    print(input$artist_name)
    audio_features = get_artist_audio_features(as.character(input$artist_name))
    output$artist_name = renderText({c('<h3 style="font-weight: bold">', artist_info$name, '</h3>')})
    output$artist_img = renderText({c('<img src="',artist_info$img_url,'" style="width: 300px; height: 300px;"/>')})
    print(audio_features)
    output$tracks_scatter_plot = renderPlotly(generate_audio_scatter_plot(audio_features, "valence", "energy"))
  })
  
}

#artist_info <- get_artist("7CJgLPEqiIRuneZSolpawQ?")
#audio_features = get_artist_audio_features("Young Igi")


