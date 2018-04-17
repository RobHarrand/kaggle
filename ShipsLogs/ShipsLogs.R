#Load libraries,
require(maptools)
require(animation) 

#Load data,
data = read.csv("data\\CLIWOC15.csv", header = T) #Load the data

Endeavour = data[grep("Endeavour", data$ShipName),] #Create an Endeavour only version
rm(data) #Remove the starting data

exc = (is.na(Endeavour$Lat3) | is.na(Endeavour$Lon3) | is.na(Endeavour$Distance)) #Delete rows with missing long, lat or distance
Endeavour = Endeavour[!exc,]

Endeavour = Endeavour[with(Endeavour, order(UTC)),] #Order by date

#Capitalise the ship's logs (currently not using),
# Endeavour$Clearness = as.character(Endeavour$Clearness)
# Endeavour$Clearness = paste0(toupper(substr(Endeavour$Clearness, 1, 1)), substr(Endeavour$Clearness, 2, nchar(Endeavour$Clearness)))

# Endeavour$PrecipitationDescriptor = as.character(Endeavour$PrecipitationDescriptor)
# Endeavour$PrecipitationDescriptor = paste0(toupper(substr(Endeavour$PrecipitationDescriptor, 1, 1)), substr(Endeavour$PrecipitationDescriptor, 2, nchar(Endeavour$PrecipitationDescriptor)))

#Get a world map,
data(wrld_simpl)

par(mar = c(0,0,1,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')

i = 1

#Set a weather column,
Endeavour$Col = "white"
Endeavour$Col[grep("rain|showers", Endeavour$PrecipitationDescriptor)] = 'blue'
Endeavour$Col[grep("fine|pleasant|fair|clear", Endeavour$Clearness)] = 'light blue'
Endeavour$Col[grep("cloudy|hazy", Endeavour$Clearness)] = 'grey'

#Work out the total distance,
Endeavour$Total_Distance = cumsum(Endeavour$Distance)

#Loop through the rows and save the gif...

saveGIF(while (i < length(Endeavour$Lat3+1)) {
    
    plot(wrld_simpl, col='dark green', bg='white', border='black', ann=FALSE, axes = FALSE, main = "The first voyage of James Cook (HMS Endeavour)")
    
        for (x in seq(0:i)) {
            points(Endeavour$Lon3[x],Endeavour$Lat3[x], pch = 20, cex = 1, col=rgb(1, 1, 0.5, 0.5)) #Plot the ship's previous locations
        }
    
        points(Endeavour$Lon3[i],Endeavour$Lat3[i], pch = 21, cex = Endeavour$Total_Distance[i]/5541, col = "black", bg = Endeavour$Col[i]) #Plot the ship location
    
    #Plot some text,
        text(-125,110, "Spot Size = Total distance", col = "black", cex = 0.9, font = 2)
        text(125,110, "Colour = Day's weather", col = "black", cex = 0.9, font = 2)
        
        points(85, 100, pch = 21, cex = 2, col = "black", bg = "grey")
        text(100, 101, " - cloud ", col = "black", cex = 0.8, font = 2)
        
        points(115, 100, pch = 21, cex = 2, col = "black", bg = "blue")
        text(129, 101, " - rain ", col = "black", cex = 0.8, font = 2)
        
        points(85, 92, pch = 21, cex = 2, col = "black", bg = "light blue")
        text(98, 93, " - fine ", col = "black", cex = 0.8, font = 2)
        
        points(115, 92, pch = 21, cex = 2, col = "black")
        text(142, 93, " - not recorded ", col = "black", cex = 0.8, font = 2)
        
        #text(-110,103, "Ship's log: ", col = "black", cex = 1, font = 2)
        text(0, 110, paste("Date: ", Endeavour$Day[i],"/",Endeavour$Month[i],"/",Endeavour$Year[i]), col = "black", cex = 1, font = 2)
        #text(0, 100, Endeavour$Clearness[i], col = "red", cex = 1, font = 2)
        #text(0, 90, Endeavour$AllWindForces[i], col = "red", cex = 1, font = 2)
        #text(0, 90, Endeavour$PrecipitationDescriptor[i], col = "red", cex = 1, font = 2)

    #Wipe the text,
    #rect(-160,85,160,119, col = "light blue", border = NA)
    
    i = i+1
    
    }, movie.name = "Endeavour.gif", interval = 0.25, convert = "convert", ani.width = 800, 
    ani.height = 800)