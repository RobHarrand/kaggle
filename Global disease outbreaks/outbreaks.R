require(plyr)
require(maptools)
require(maps)
require(ggplot2)
require(lubridate)
require(animation)

dt = read.csv("Outbreak_240817.csv")

#plot(dt$longitude, dt$latitude)

dt$reportingDate = as.character(dt$reportingDate)
dt$reportingDate = as.POSIXct(dt$reportingDate, format = "%d/%m/%Y")

dt$Year = year(dt$reportingDate)
dt$Month = month(dt$reportingDate)

#temp = dt[dt$Year == 2016 & dt$Month == 6 & dt$country == 'Niger' & dt$disease == 'Influenza - Avian',]

agg_country = aggregate(sumDeaths ~ disease + country + Year + Month, data = dt, mean)
agg_country = agg_country[order(-agg_country$sumDeaths),]



agg_country = aggregate(sumDeaths ~ disease, data = dt, mean)
agg_country = agg_country[order(-agg_country$sumDeaths),]

par(mar = c(20,4,0.5,4))

barplot(agg_country$sumDeaths, names.arg = agg_country$disease, las = 2)




bird = dt[dt$disease == 'Influenza - Avian',]
bird = bird[order(bird$reportingDate),]

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

plot(wrld_simpl, col='green', bg='white', border='black', ann=FALSE, axes = FALSE, main = "Disease Outbreaks")

points(bird$longitude,bird$latitude, pch = 20, cex = 1, col='blue')


#dev.off()


bbox = cbind(c(179,179,-179,-179,179), c(89,-89,-89,89,89))

par(mar = c(0,0,0,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')
#plot(wrld_simpl, col='grey', bg='white', border=NA, ann=FALSE, axes = FALSE)



i=1

saveGIF(while (i <= length(bird$Id)) {
  
  plot(wrld_simpl, col='darkolivegreen2', bg='white', border='grey', ann=FALSE, axes = FALSE, main = "Birdflu Outbreaks")
  
  points(bird$longitude[1:i],bird$latitude[1:i], pch = 21, cex = 1, bg = 'red', col='black')
  
  text(-156.621094,-38.373819, paste("Date: ", bird$reportingDate[i]))
  
  i = i+60
  
}, movie.name = "birdflu.gif", interval = 0.25, convert = "convert", ani.width = 800, 
ani.height = 800)







