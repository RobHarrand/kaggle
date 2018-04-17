library('ggplot2')
library('animation')

terror = read.csv('terror.csv', stringsAsFactors = F)

min_lat <- min(terror$latitude, na.rm = T)
max_lat <- max(terror$latitude, na.rm = T)
min_long <- min(terror$longitude, na.rm = T)
max_long <- max(terror$longitude, na.rm = T)

#Load the map data,
s <- map_data("world")

#Set everywhere to green,
s$colour = "dark green"

#Change a few obvious ones,
s$colour[s$region == "Antarctica"] = "white"
s$colour[s$region == "Greenland"] = "white"
s$colour[s$region == "Morocco"] = "orange"
s$colour[s$region == "Algeria"] = "orange"
s$colour[s$region == "Tunisia"] = "orange"
s$colour[s$region == "Libya"] = "orange"
s$colour[s$region == "Egypt"] = "orange"
s$colour[s$region == "Saudi Arabia"] = "orange"
s$colour[s$region == "Australia"] = "orange"
s$colour[s$region == "Mongolia"] = "orange"


x = densCols(terror$latitude, terror$longitude)

terror$dens <- col2rgb(x)[1,] + 1L

cols = colorRampPalette(c('red', 'yellow', 'blue'))(256)

terror$col <- cols[terror$dens]






# Start the clock!
ptm <- proc.time()


#Set a counter,
l = length(terror$eventid)

i = 1
j = 0

saveGIF(while (i < l) {
    
    #Set the viewing position from the spline,        
    orient_x = 20
    orient_y = -40
    
    #Let's plot this beast!
    print(
        
        m <- ggplot(data = s, aes(x=long, y=lat, group=group)) + #Set ggplot2
            
            geom_path(data = s, aes(x=long, y=lat), colour="black") + #Plot the Earth
            
            geom_polygon(fill=s$colour, colour="black") + #Colour the countries
            
            theme(panel.grid.major = element_line(colour = "black")) + #Make the grid-lines black
            
            coord_map("ortho", orientation=c(orient_x,orient_y+j,0)) +
            
            geom_point(inherit.aes=FALSE, data = terror[1:i,], size = 2, colour = terror$col[1:i], 
                       aes(x=longitude[1:i], y=latitude[1:i]), alpha = 0.2) +
            
            annotate("text", x =-45, y = -10, label = terror$eventid.1[i], colour = 'red', size = 10, fontface =2) + 
            
            annotate("text", x =-45, y = -20, label = paste('No. of incidents: ', i, sep = ''), colour = 'red', size = 8, fontface = 2)
    )
    
    i = i+1500
    j = j+0.2
    
}, movie.name = "Terror3D.gif", interval = 0.1, convert = "convert", ani.width = 800, 
ani.height = 800)


# Stop the clock
proc.time() - ptm
