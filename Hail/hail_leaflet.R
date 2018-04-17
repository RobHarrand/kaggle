library(leaflet)

df = read.csv('hail-2015.csv', stringsAsFactors = F)

df$WSR_ID = NULL
df$CELL_ID = NULL
df$RANGE = NULL
df$AZIMUTH = NULL
df$SEVPROB = NULL

ex = (df$PROB == -999)
df = df[!ex,]

df$Month = substr(df$X.ZTIME, 5,6)
df$Day = substr(df$X.ZTIME, 7,8)
df$X.ZTIME = NULL
df$Month = as.numeric(df$Month)
df$Day = as.numeric(df$Day)
df$Month_name = month.abb[df$Month]
df$MAXSIZE = as.factor(df$MAXSIZE)
df$Size_bins[df$MAXSIZE %in% c(0.5,0.75,1,1.25,1.5,1.75)] = 'Small'
df$Size_bins[df$MAXSIZE %in% c(2,2.25,2.5,2.75,3)] = 'Medium'
df$Size_bins[df$MAXSIZE %in% c(3.25,3.5, 3.75,4)] = 'Buy a really good hat'
df$Size_bins = as.factor(df$Size_bins)


df_sample = df[df$MAXSIZE == 4,]


bighail <- leaflet(df_sample) %>%
  setView(-98, 40, zoom = 4) %>%
  addTiles() %>%
  addMarkers(~LON, ~LAT, popup=~PROB, 
             clusterOptions = markerClusterOptions(), 
             options = popupOptions(closeButton = TRUE))


bighail
