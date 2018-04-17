#The script uses a couple of different indicators as proxies for 'gender equality'
#These plots are based upon average values per country across all available years

#Load libraries,
require("ggplot2")
require(animation) 

#Load data,
indicators = read.csv("data\\Indicators.csv", header = TRUE)

#indicators = read.csv("../input/Indicators.csv", header = T) #Load the data #Load the data (Kaggle)

#Create a 'negative' dataset,
indicators_neg = indicators[grep('(any of five reasons)', indicators$IndicatorName),]
indicators_neg = droplevels(indicators_neg)
indicators_neg$CountryName = as.character(indicators_neg$CountryName)

#Correct some country names,
indicators_neg$CountryName[indicators_neg$CountryName == 'Egypt, Arab Rep.'] = 'Egypt'
indicators_neg$CountryName[indicators_neg$CountryName == 'Congo, Dem. Rep.'] = 'Democratic Republic of the Congo'
indicators_neg$CountryName[indicators_neg$CountryName == 'Gambia, The'] = 'Gambia'
indicators_neg$CountryName[indicators_neg$CountryName == 'Macedonia, FYR'] = 'Macedonia'
indicators_neg$CountryName[indicators_neg$CountryName == 'Yemen, Rep.'] = 'Yemen'

#Create a 'positive' dataset,
indicators_pos = indicators[indicators$IndicatorCode == 'SG.GEN.PARL.ZS',]
indicators_pos = droplevels(indicators_pos)
indicators_pos$CountryName = as.character(indicators_pos$CountryName)

#Correct some country names,
indicators_pos$CountryName[indicators_pos$CountryName == 'Egypt, Arab Rep.'] = 'Egypt'
indicators_pos$CountryName[indicators_pos$CountryName == 'Congo, Dem. Rep.'] = 'Democratic Republic of the Congo'
indicators_pos$CountryName[indicators_pos$CountryName == 'Gambia, The'] = 'Gambia'
indicators_pos$CountryName[indicators_pos$CountryName == 'Macedonia, FYR'] = 'Macedonia'
indicators_pos$CountryName[indicators_pos$CountryName == 'Yemen, Rep.'] = 'Yemen'
indicators_pos$CountryName[indicators_pos$CountryName == 'Bahamas, The'] = 'Bahamas'
indicators_pos$CountryName[indicators_pos$CountryName == 'Venezuela, RB'] = 'Venezuela'
indicators_pos$CountryName[indicators_pos$CountryName == 'Korea, Rep.'] = 'South Korea'
indicators_pos$CountryName[indicators_pos$CountryName == 'Iran, Islamic Rep.'] = 'Iran'

#Load the map data,
s = map_data("world")

#Calculate averages,
means_neg = aggregate(indicators_neg$Value, FUN = mean, by = list(indicators_neg$CountryName))
colnames(means_neg) = c("CountryName", "Mean")
means_neg$CountryName = as.character(means_neg$CountryName)

# #Change colours,
s$colour = 1

i=1

while (i <= length(means_neg$CountryName)) {
    s$colour[s$region == means_neg$CountryName[i]] = means_neg[i,2]
    i=i+1
}


#Load the map data,
s2 = map_data("world")

#Calculate averages,
means_pos = aggregate(indicators_pos$Value, FUN = mean, by = list(indicators_pos$CountryName))
colnames(means_pos) = c("CountryName", "Mean")
means_pos$CountryName = as.character(means_pos$CountryName)

#Change colours,
s2$colour = 1

i=1

while (i <= length(means_pos$CountryName)) {
    s2$colour[s2$region == means_pos$CountryName[i]] = means_pos[i,2]
    i=i+1
}


#Make the plots (wrapped inside the animated GIF),
i=1
y=1
a=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)

saveGIF(while (i <= length(a)){

    if (y == 1){
     
    print(m <- ggplot(s, aes(x=long, y=lat, group=group, fill=colour)) + #Set ggplot2
              
              geom_polygon(alpha=a[i]) + #Set transparency
              
              geom_path(data = s, aes(x=long, y=lat, group=group), colour="black") + #Plot the Earth
              
              scale_fill_gradient(low = "black", high = "firebrick1", guide = "colourbar") + #Set the colours,
              
              theme(plot.title = element_text(size = rel(2))) + #Change the text size,

              ggtitle(expression(atop("Gender Equality: Progress Needed", #Add the title and subtitle
                            atop(italic("Women who believe a husband is justified in beating his wife (%)"), "")))))   
    
        i=i+1
        
        if (i==31) {y=2}
        if (i==31) {i=1}
        
    }
    
    
    if (y == 2) {
    
    print(m <- ggplot(s2, aes(x=long, y=lat, group=group, fill=colour)) + #Set ggplot2
        
        geom_polygon(alpha=a[i]) + #Set transparency
        
        geom_path(data = s2, aes(x=long, y=lat, group=group), colour="black") + #Plot the Earth

        scale_fill_gradient(low = "black", high = "cyan1", guide = "colourbar") + #Set the colours,
            
        theme(plot.title = element_text(size = rel(2))) + #Change the text size,

        ggtitle(expression(atop("Gender Equality: Getting it right", #Add the title and subtitle
                                atop(italic("Proportion of seats held by women in national parliaments (%)"), "")))))
    
    
        i=i+1
        
    }
    
    
}, movie.name = "World-Dev.gif", interval = 0.15, convert = "convert", ani.width = 800, 
ani.height = 600)
