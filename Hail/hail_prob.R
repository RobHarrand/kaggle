library('animation')
library(ggplot2)

df = read.csv('hail-2015.csv', stringsAsFactors = F)

df$WSR_ID = NULL
df$CELL_ID = NULL
df$RANGE = NULL
df$AZIMUTH = NULL
df$SEVPROB = NULL

ex = (df$PROB == -999)
df = df[!ex,]

df$Month = substr(df$X.ZTIME, 5,6)
df$Day = substr(df$X.ZTIME, 7,8)
df$X.ZTIME = NULL
df$Month = as.numeric(df$Month)
df$Day = as.numeric(df$Day)
df$Month_name = month.abb[df$Month]
df$MAXSIZE = as.factor(df$MAXSIZE)
df$Size_bins[df$MAXSIZE %in% c(0.5,0.75,1,1.25,1.5,1.75)] = 'Small'
df$Size_bins[df$MAXSIZE %in% c(2,2.25,2.5,2.75,3)] = 'Medium'
df$Size_bins[df$MAXSIZE %in% c(3.25,3.5, 3.75,4)] = 'Buy a really good hat'
df$Size_bins = as.factor(df$Size_bins)



m = 4
d = 7

saveGIF(while (m<=12) {
  
  while (d <= length(table(df$Day[df$Month == m]))) { 
    
    t = as.data.frame(table(df$Day[df$Month == m]))
    ds = as.numeric(as.character(t$Var1))
    
    if (length(df$PROB[df$Month == m & df$Day == ds[d]]) > 5) {
      
      temp = df[df$Month == m & df$Day == ds[d],]
      new_empty1 = df[1,]
      new_empty2 = df[1,]
      new_empty1$Size_bins = 'Medium'
      new_empty2$Size_bins = 'Buy a really good hat'
      new_empty1$PROB = 0
      new_empty2$PROB = 0
      new_empty1$Month = m
      new_empty1$Day = d
      new_empty2$Month = m
      new_empty2$Day = d
      temp = rbind(temp, new_empty1, new_empty2)
      temp$Size_bins = factor(temp$Size_bins, levels = c('Small', 'Medium', 'Buy a really good hat'))
      
      print(x <- ggplot(temp, aes(PROB, fill = Size_bins, colour = Size_bins)) +
              geom_density(alpha = 0.1) +
              xlab("Probability of hail") +
              ylab("Density") +
              ylim(0, 0.2) + 
              xlim(0,100) + 
              annotate("text", x =80, y=0.185, 
                       label = paste(df$Day[df$Month == m & df$Day == ds[d]][1],
                                     df$Month_name[df$Month == m & df$Day == ds[d]][1], sep = " "), colour = 'black', size = 8) +
              ggtitle("Probabilities of hail across the year, by hail size") +
              theme(plot.title = element_text(lineheight=.8, face="bold")) + 
              theme(legend.title=element_blank()))
      
    }
    
    d=d+2
    
  }
  
  d=1
  m=m+1
  
}, movie.name = "hail_prob.gif", interval = 0.2, convert = "convert", ani.width = 600, 
ani.height = 600)