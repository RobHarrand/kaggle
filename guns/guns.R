library('ggplot2')
library('animation')

dt = read.csv('guns.csv')

#dt$Date = sapply(strsplit(dt$Date.Time, split = " "), function(x) x[[1]][1])
dt$Date = as.Date(dt$Date, format = "%m/%d/%Y")
dt = dt[order(dt$Date),]

min_lat <- min(dt$Latitude, na.rm = T)
max_lat <- max(dt$Latitude, na.rm = T)
min_long <- min(dt$Longitude, na.rm = T)
max_long <- max(dt$Longitude, na.rm = T)

l = length(dt$Date)

i = 1

saveGIF(while (i <= l) {
  
  print(m <- ggplot(data=dt[1:i,],aes(Longitude[1:i],Latitude[1:i],size=(Fatalities[1:i]+1)*2,color="red", alpha = 1)) + 
          geom_point() +
          scale_x_continuous(limits=c(min_long, max_long)) +
          scale_y_continuous(limits=c(min_lat, max_lat)) +
          theme(panel.background = element_rect(fill = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
          xlab('Longitute') +
          ylab('Latitude') +
          annotate("text", x =0, y=58, label = dt$Date[i], colour = 'white', size = 8)+
          annotate("text", x =-3, y=60, label = '2016 UK Cycling Collisions', colour = 'white', size = 9)+
          annotate("text", x =0.5, y=50.3, label = 'STATS19 data from data.gov.uk', colour = 'white', size = 7)+
          annotate("text", x =0.5, y=50, label = '(open government licence)', colour = 'white', size = 7))
  
  
  
  i = i+5
  
}, movie.name = "mass_shootings_us.gif", interval = 0.01, convert = "convert", ani.width = 1000, 
ani.height = 1000)
