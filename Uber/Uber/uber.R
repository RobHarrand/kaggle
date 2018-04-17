library('ggplot2')
library('animation')

uber = read.csv('uber-raw-data-sep14.csv', stringsAsFactors = F)

uber$Date = sapply(strsplit(uber$Date.Time, split = " "), function(x) x[[1]][1])
uber$Date = as.Date(uber$Date, format = "%m/%d/%Y")
uber = uber[order(uber$Date),]

min_lat <- min(uber$Lat)
max_lat <- max(uber$Lat)
min_long <- min(uber$Lon)
max_long <- max(uber$Lon)

l = length(uber$Date.Time)

i = 1

saveGIF(while (i <= l) {

    print(m <- ggplot(data=uber[1:i,],aes(Lon[1:i],Lat[1:i])) + 
              geom_point(size=0.06, color="white", alpha = 0.2) +
              scale_x_continuous(limits=c(min_long, max_long)) +
              scale_y_continuous(limits=c(min_lat, max_lat)) +
              theme(panel.background = element_rect(fill = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
              annotate("text", x =-73.0, y=41.2, label = uber$Date[i], colour = 'white', size = 8))

    i = i+25000

}, movie.name = "uber.gif", interval = 0.1, convert = "convert", ani.width = 800, 
ani.height = 800)

