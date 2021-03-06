dt1 = read.csv('Copy of Pets_for_Patriots_Partner_Network.csv')
dt2 = read.csv('Pets.csv')

table(dt1$State.Province)
table(dt2$Location)

dt1$State.Province[dt1$State.Province == 'CA'] = 'California'
dt1$State.Province[dt1$State.Province == 'CO'] = 'Colorado'
dt1$State.Province[dt1$State.Province == 'CT'] = 'Connecticut'
dt1$State.Province[dt1$State.Province == 'DC'] = 'District of Columbia'
dt1$State.Province[dt1$State.Province == 'GA'] = 'Georgia'
dt1$State.Province[dt1$State.Province == 'IL'] = 'Illinois'
dt1$State.Province[dt1$State.Province == 'MO'] = 'Missouri'
dt1$State.Province[dt1$State.Province == 'MT'] = 'Montana'
dt1$State.Province[dt1$State.Province == 'NC'] = 'North Carolina'
dt1$State.Province[dt1$State.Province == 'NY'] = 'New York'
dt1$State.Province[dt1$State.Province == 'OH'] = 'Ohio'
dt1$State.Province[dt1$State.Province == 'TX'] = 'Texas'
dt1$State.Province[dt1$State.Province == 'WA'] = 'Washington'

dt1 = droplevels(dt1)

merged = merge(dt1, dt2, by.x = 'State.Province', by.y = 'Location')


#Plotting

merged$Dog.Population..in.1000. = gsub(",", "", merged$Dog.Population..in.1000.)
merged$Dog.Population..in.1000. = as.character(merged$Dog.Population..in.1000.)
merged$Dog.Population..in.1000. = as.numeric(merged$Dog.Population..in.1000.)

merged$Cat.Population = gsub(",", "", merged$Cat.Population)
merged$Cat.Population = as.character(merged$Cat.Population)
merged$Cat.Population = as.numeric(merged$Cat.Population)


library(scales)
library(plotly)
library(ggplot2)


# col = alpha('dark green', 0.1)
# 
# plot(merged$Latitude, 
#      merged$Longitude, 
#      cex = merged$Dog.Population..in.1000./2000,
#      pch = 16,
#      col = col)
# 
# t <- list(
#   family = "sans serif",
#   size = 18,
#   color = toRGB("grey50")
# )
# 
# plot_ly(x = merged$Latitude, 
#         y = merged$Longitude, 
#         mode = "markers", 
#         showlegend = T, 
#         color = I('dark green'),
#         alpha = 0.15,
#         size = merged$Dog.Population..in.1000./2000)
#           
# 
# s <- map_data("world")
# s$colour = "black"
# 
# min_lat <- min(merged$Latitude, na.rm = T)
# max_lat <- max(merged$Latitude, na.rm = T)
# min_long <- min(merged$Longitude)
# max_long <- max(merged$Longitude)
# orient_x = 40
# orient_y = -120
# 
# p <- ggplot(data = s, aes(x=long, y=lat, group=group)) + #Set ggplot2
#     
#     geom_path(data = s, aes(x=long, y=lat), colour="black") + #Plot the Earth
#     
#     geom_polygon(fill=s$colour, colour="black") + #Colour the countries
#     
#     theme(panel.grid.major = element_line(colour = "black")) + #Make the grid-lines black
#     
#     coord_map("ortho", orientation=c(orient_x,orient_y,0)) +
#     
#     geom_point(inherit.aes=FALSE, 
#                data = merged, 
#                size = merged$Dog.Population..in.1000./2000, 
#                colour = 'green', 
#                aes(x=Longitude, 
#                    y=Latitude), 
#                alpha = 0.2)# +
#     
#     #annotate("text", x =-100, y=-10, label = ufos$Date[i], colour = 'red', size = 10) + 
#     
#     #annotate("text", x =-100, y=-20, label = paste('No. of cases: ', i, sep = ''), colour = 'red', size = 8)
# 
# 
# 
# ggplotly(p)
# 
# 
# 
# 
# p <- plot_ly(lon = merged$Longitude,lat = merged$Latitude, type = "scattergeo",mode = "markers")
# 
# p <- add_trace(long = s$long, lat = s$lat, 
#                mode = "lines", line = list(color=toRGB("black"),width = 0.5),
#                type = "scattergeo",  hoverinfo = "none",showlegend = F)
# 
# p <- layout(p, geo = g)
# 
# 
# 
# 
# 
#           hoverinfo = "text",
#           textfont = t, text = paste("Address = ",  merged$Address))
# 
# %>%
#   layout(p,title ="cPAN Data June 2016", titlefont = t, xaxis = list(title = "Scaled Lipase", titlefont = t),
#          yaxis = list(title = "Scaled Amylase", titlefont = t))
# 
# 
# 
# 
# 
# #Chloropleth
# 
# merged$hover <- with(merged, "no hover")
# # give state boundaries a white border
# l <- list(color = toRGB("white"), width = 2)
# # specify some map projection/options
# g <- list(
#   scope = 'usa',
#   projection = list(type = 'albers usa'),
#   showlakes = TRUE,
#   lakecolor = toRGB('white')
# )
# 
# 
# 
# merged$State.Province = state.abb[match(merged$State.Province,state.name)]
# merged$State.Province = as.factor(merged$State.Province)
# 
# state.abb[!(state.abb %in% merged$State.Province)]



###

# change default color scale title
#m <- list(colorbar = list(title = "% of households with pets"))

# geo styling
g <- list(
  scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("darkolivegreen4"),
  subunitcolor = toRGB("darkolivegreen4"),
  countrycolor = toRGB("black"),
  showcoastlines = TRUE,
  showocean = TRUE,
  oceancolor = 'skyblue4',
  showlakes = TRUE,
  lakecolor = 'skyblue4',
  showrivers = TRUE,
  rivercolor = 'skyblue4',
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 5,
  projection = list(
    type = 'conic conformal',
    rotation = list(lon = -100)
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-140, -55),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(20, 60),
    dtick = 5
  )
)



p <- plot_geo(merged, 
              lat = ~Latitude, 
              lon = ~Longitude, 
              opacity = 0.5,
              marker = list(color = 'red', 
                                     alpha = 0.3,
                   size= ~I(merged$Dog.Population..in.1000.)/200,
                   line = list(color = 'black',
                               width = 1))) %>%
  add_markers(
    text = ~paste("Organisation: ", merged$Organization.Name,
                  "</br>",
                  "</br>",
                  "Address: ", merged$Address, ",", merged$City, ", ", merged$Zip.Postal.Code, ", ", merged$State.Province,
                  "</br>",
                  "Partner Type: ", merged$Partner.Type,
                  "</br>",
                  "State Dog Population (thousands): ", merged$Dog.Population..in.1000.,  " (~ point size)",
                  "</br>",
                  "State Cat Population (thousands): ", merged$Cat.Population,
                  "</br>",
                  "State Proportion of households with pets: ", merged$Percentage.of.households.with.pets, "%", sep=""),
    hoverinfo = "text"
  ) %>%
  layout(title = 'Pets for Patriots Shelter Map & Animal Statistics', 
           geo = g)

p




Sys.setenv("plotly_username"="tentotheminus9")
Sys.setenv("plotly_api_key"="VU1BYmduL9acUMjVYt2W")

api_create(p)



