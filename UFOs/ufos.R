library('ggplot2')
library('animation')

ufos = read.csv('scrubbed.csv', stringsAsFactors = F)

ufos$Date = sapply(strsplit(ufos$datetime, split = " "), function(x) x[[1]][1])

ufos$Date = as.Date(ufos$Date, format = "%m/%d/%Y")

ufos = ufos[!is.na(ufos$Date),]

ufos = ufos[order(ufos$Date),]

ufos$latitude = as.numeric(ufos$latitude)
#ufos$Duration.s. = as.numeric(ufos)

min_lat <- min(ufos$latitude, na.rm = T)
max_lat <- max(ufos$latitude, na.rm = T)
min_long <- min(ufos$longitude)
max_long <- max(ufos$longitude)


#Load the map data,
s <- map_data("world")

#Set everywhere to green,
s$colour = "black"

#Change a few obvious ones,
s$colour[s$region == "Antarctica"] = "white"
s$colour[s$region == "Greenland"] = "white"
# s$colour[s$region == "Morocco"] = "orange"
# s$colour[s$region == "Algeria"] = "orange"
# s$colour[s$region == "Tunisia"] = "orange"
# s$colour[s$region == "Libya"] = "orange"
# s$colour[s$region == "Egypt"] = "orange"
# s$colour[s$region == "Saudi Arabia"] = "orange"
# s$colour[s$region == "Australia"] = "orange"
# s$colour[s$region == "Mongolia"] = "orange"

x = densCols(ufos$latitude, ufos$longitude)
ufos$dens <- col2rgb(x)[1,] + 1L
cols = colorRampPalette(c('red', 'yellow', 'blue'))(256)
ufos$col <- cols[ufos$dens]


# Start the clock!
ptm <- proc.time()


#Set a counter,
l = length(ufos$datetime)

i = 1
j = 0

saveGIF(while (i <= l) {
    
    #Set the viewing position from the spline,        
    orient_x = 40
    orient_y = -150
    
    #Let's plot this beast!
    print(
        
        m <- ggplot(data = s, aes(x=long, y=lat, group=group)) + #Set ggplot2
            
            geom_path(data = s, aes(x=long, y=lat), colour="black") + #Plot the Earth
            
            geom_polygon(fill=s$colour, colour="black") + #Colour the countries
            
            theme(panel.grid.major = element_line(colour = "black")) + #Make the grid-lines black
            
            coord_map("ortho", orientation=c(orient_x,orient_y+j,0)) +
        
            geom_point(inherit.aes=FALSE, data = ufos[1:i,], size = 2, colour = ufos$col[1:i], aes(x=longitude[1:i], y=latitude[1:i]), alpha = 0.1) +
        
            annotate("text", x =-100, y=-10, label = ufos$Date[i], colour = 'red', size = 10) + 

            annotate("text", x =-100, y=-20, label = paste('No. of cases: ', i, sep = ''), colour = 'red', size = 8)
    )
    
    i = i+800
    j=j+0.3
    
}, movie.name = "UFO3D.gif", interval = 0.1, convert = "convert", ani.width = 800, 
ani.height = 800)



# Stop the clock
proc.time() - ptm


