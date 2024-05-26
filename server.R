library('dplyr')
library('DT')
library('tibble')
library('spotifyr')
library('fmsb')


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
  
  for(track_id in playlist$track.id){
    audio_features <- get_track_audio_features(track_id)
    danceability = c(danceability, audio_features$danceability)
    energy = c(energy, audio_features$energy)
    acousticness = c(acousticness, audio_features$acousticness)
    liveness = c(liveness, audio_features$liveness)
    valence = c(valence, audio_features$valence)
    tempo = c(tempo, audio_features$tempo)
  }
  tracks_info = data.frame(Name = playlist$track.name, Album_name = playlist$track.album.name, img_url = img_urls, preview_url = playlist$track.preview_url, popularity = playlist$track.popularity, danceability = danceability, energy = energy, acousticness = acousticness, liveness = liveness, valence = valence, tempo = tempo, duration_ms = playlist$track.duration_ms)
  return(tracks_info)
}

generate_audio_features_plot <- function(danceability, energy, acousticness, liveness, valence){
  audio_features = data.frame(Danceability = danceability, Energy = energy, Acousticness = acousticness, Liveness = liveness, Valence = valence)
  radarchart(rbind(rep(1,length(audio_features)) , rep(0,length(audio_features)), audio_features),
             axistype=1,
             pcol=rgb(0.84,0.28,0.84,0.7) , pfcol=rgb(0.84,0.28,0.84,0.5)  , plwd=4 , 
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,0.2,5), cglwd=0.8
             )
}

server <- function(input, output) {
  tracks_info = data.frame()
  # Displaying playlist info in the form of DTable
  observeEvent(input$submit_playlist, {
    tracks_info <<- get_tracks_info_from_playlist(get_playlist_tracks(playlist_id = input$playlist_id))
    output$table_playlist = DT::renderDT(tracks_info[,c("Name","Album_name")], rownames = FALSE, selection = 'single')
    output$average_audio_features_plot = renderPlot(generate_audio_features_plot(mean(as.numeric(tracks_info$danceability)), mean(as.numeric(tracks_info$energy)), mean(as.numeric(tracks_info$acousticness)), mean(as.numeric(tracks_info$liveness)), mean(as.numeric(tracks_info$valence))))
  })
  
  # Displaing the selected track info
  observeEvent(input$table_playlist_rows_selected, {
    selected_track <- tracks_info[input$table_playlist_rows_selected, ]
    output$track_name = renderText(paste('<h4 style="font-weight: bold">',selected_track$Name,"</h4>"))
    output$track_img = renderText({c('<img src="', selected_track$img_url,'" style="width: 300px; height: 300px;"/>')})
    output$audio_features_plot = renderPlot(generate_audio_features_plot(selected_track$danceability, selected_track$energy, selected_track$acousticness, selected_track$liveness, selected_track$valence))
    output$track_preview = renderText(paste0('<audio src="', selected_track$preview_url,'"controls> </audio>'))
    })

}
