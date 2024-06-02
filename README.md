# Spotify Analyzer Dashboard

The Spotify Analyzer Dashboard was created as part of the Data Visualization course at Poznan University of Technology. It uses Spotify API to fetch data about playlists, artists and albums based on IDs and present the data in clear and interactive manner.

The dashboard was divided into 2 parts - one regarding the playlist analysis and one regarding artist analysis.

## Playlist Analyzer:

Playlist Analyzer was designed to present the data about the playlist chosen by the user. After providing the correct playlist ID the user can:

- View the pie chart describing the distribution of track genres in the playlist.
- View the interactive radar chart presenting the average audio features of all tracks in the playlist. The user can choose which audio features should be included in the chart.
- View the list of tracks in the playlist. After selecting the record from the table, the informative card about the track including basic information and a radar chart similar to the one described above but regarding only the specific track will be displayed. It is possible to listen to the song preview if it is available

## Artist Analyzer

Artist Analyzer presents the data about the artist chosen by the user. After providing the correct artist ID the user can:

- View the informative card summarising artist's releases
- View the interactive scatter plot showing all the artist's regarding two selected audio features. The user can freely change both the x-axis and the y-axis features.
- View the ridgeline plot comparing two chosen albums of the artist in terms of the chosen audio feature. The user can choose both of the albums and the feature on which they are to be compared.

## Useage
To run the dashboard locally you need to set two enviornmental variables which will hold your Spotify API keys using the following lines of code:

```r
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')
```
