library('ggplot2')
library('animation')
install.packages('maps', dependencies = T)

df = read.csv('hail-2015.csv', stringsAsFactors = F)

df$WSR_ID = NULL
df$CELL_ID = NULL
df$RANGE = NULL
df$AZIMUTH = NULL
df$PROB = NULL


ex = ((df$SEVPROB == -999) | (df$SEVPROB == 0))
df = df[!ex,]


df$Month = substr(df$X.ZTIME, 5,6)
df$Day = substr(df$X.ZTIME, 7,8)
df$X.ZTIME = NULL
rm(ex)

save(df, file = 'hail_small.RData')



#df_agg = aggregate(. ~ Month + Day, data=df, mean, na.rm=TRUE)



df_sample = df[sample(nrow(df), 10000), ]

qplot(df_sample$LON, df_sample$LAT, size = df_sample$MAXSIZE, alpha = (100-df_sample$SEVPROB)/100)


#Load the map data,
s <- map_data("world")

#Set everywhere to green,
s$colour = "light green"

#Change a few obvious ones,
s$colour[s$region == "Antarctica"] = "white"
s$colour[s$region == "Greenland"] = "white"

df_sample$Month = as.numeric(df_sample$Month)

#Set the viewing position from the spline,        
orient_x = 40
orient_y = -100



i = 1

saveGIF(while (i <= 12) {

print(m <- ggplot(data = s, aes(x=long, y=lat, group=group)) + #Set ggplot2
  
  geom_path(data = s, aes(x=long, y=lat), colour="black") + #Plot the Earth
  
  geom_polygon(fill=s$colour, colour="black") + #Colour the countries
  
  theme(panel.grid.major = element_line(colour = "black")) + #Make the grid-lines black
  
  coord_map("ortho", orientation=c(orient_x,orient_y,0)) +
  
  geom_point(inherit.aes=FALSE, data = df_sample[df_sample$Month == 0+1,], colour = 'red', 
             aes(x=LON, y=LAT, alpha = 100-SEVPROB, size = MAXSIZE))

)

i = i+1

}, movie.name = "hail3D.gif", interval = 0.1, convert = "convert", ani.width = 800, 
ani.height = 800)
