#To quote wikipedia ... 

# "The first voyage of James Cook was a combined Royal Navy and Royal Society expedition to the 
# south Pacific Ocean aboard HMS Endeavour, from 1768 to 1771. It was the first of three Pacific 
# voyages of which Cook was the commander. The aims of this first expedition were to observe the 
# 1769 transit of Venus across the Sun (3–4 June of that year), and to seek evidence of the 
# postulated Terra Australis Incognita or unknown southern land"

# This script plots the Endeavor's voyage around the world. I've cut the script short and limited 
# it from England to Cape Horn due to the full script taking too long. The full thing can be run 
# by changing the limit of the while loop.  

# The Earth's movement initially followed the ship exactly, but the movement was very jittery. 
# Therefore, the ship's movement is instead fitted with a smoothed spline, and the Earth's movement 
# follows that instead. The ship's movement is as given in the original dataset.  

# I had no idea how to plot a 3D map when I started. The following websites were invaluble,

# http://robinlovelace.net/2014/06/22/great-circles-in-rworldmap-ggplot2.html
# http://docs.ggplot2.org/0.9.3.1/map_data.html



#data = read.csv("../input/CLIWOC15.csv", header = T) #Load the data

#Load data,
data = read.csv("data\\CLIWOC15.csv", header = T) #Load the data

#Load libraries,
library(ggplot2)
require(animation)

Endeavour = data[grep("Endeavour", data$ShipName),] #Create an Endeavour only version
rm(data) #Remove the starting data

exc = (is.na(Endeavour$Lat3) | is.na(Endeavour$Lon3) | is.na(Endeavour$Distance)) #Delete rows with missing long, lat or distance
Endeavour = Endeavour[!exc,]

Endeavour = Endeavour[with(Endeavour, order(UTC)),] #Order by date

#Load the map data,
s <- map_data("world")

#Set everywhere to green,
s$colour = "green"

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

#Set a weather column,
Endeavour$Col = "white"
Endeavour$Col[grep("rain|showers", Endeavour$PrecipitationDescriptor)] = 'blue'
Endeavour$Col[grep("fine|pleasant|fair|clear", Endeavour$Clearness)] = 'light blue'
Endeavour$Col[grep("cloudy|hazy", Endeavour$Clearness)] = 'grey'

#Work out the total distance,
Endeavour$Total_Distance = cumsum(Endeavour$Distance)

#Fit a spline to the coordiates to make the Earth's movement a little less jittery,
ss = smooth.spline(Endeavour$Lon3, Endeavour$Lat3)

#Rearrange to get the starting point to the UK. Add a few points as the spline has fewer than the starting coordiates,
ssc = as.data.frame(cbind(ss$x, ss$y))
ss1 = ssc[1:255,]
ss2 = ssc[256:334,]
ss_add = rbind(ss1[255,],ss1[255,],ss1[255,],ss1[255,],ss1[255,],ss1[255,],ss1[255,])
ss_new = rbind(ss_add, ss_add, ss1[255,],ss2, ss1)

#Quick check of the starting and new coordinates,
#Note, this spline is for the movement of the Earth, NOT the position of the ship,
plot(Endeavour$Lon3, Endeavour$Lat3, col = "red")
points(ss_new)

#Set a counter,
i = 1

#Loop through the rows and save the gif...
saveGIF(while (i < 100){

    #Set the viewing position from the spline,        
    orient_x = ss_new$V2[349-i]
    orient_y = ss_new$V1[349-i]
        
    #Let's plot this beast!
    print(m <- ggplot(s, aes(x=long, y=lat, group=group)) + #Set ggplot2
              
        geom_path(data = s, aes(x=long, y=lat, group=group), colour="black") + #Plot the Earth
            
        geom_polygon(fill=s$colour, colour="black") + #Colour the countries
            
        theme(panel.grid.major = element_line(colour = "black")) + #Make the grid-lines black
            
        coord_map("ortho", orientation=c(orient_x,orient_y,0)) + #Set the viewing position
            
        geom_point(size = Endeavour$Total_Distance[i]/500, colour = 'black', fill = Endeavour$Col[i], 
            aes(x = Endeavour$Lon3[i], y = Endeavour$Lat3[i], pch = 21)) #Plot the Endeavour's position
        
        + scale_shape_identity() #Need this to allow the size and colour to come from their respective variables
    
        + annotate("text", x = Endeavour$Lon3[i]-22, y = Endeavour$Lat3[i], #Add the date
            label = paste(Endeavour$Day[i],"/",Endeavour$Month[i],"/",Endeavour$Year[i])
            ,size = 5, colour = "red", fontface="bold")
        
        + ggtitle(expression(atop("The First Voyage of James Cook - UK to Cape Horn", #Add the title and subtitle
            atop(italic("Spot size = Distance (arb). Colour = Day's weather"), "")))))
        
    i = i+1
        
}, movie.name = "Endeavour3D.gif", interval = 0.2, convert = "convert", ani.width = 800, 
ani.height = 800)