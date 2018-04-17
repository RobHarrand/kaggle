library('animation')
library('ggplot2')
library('gridExtra')

data(iris)

dt = as.data.frame(iris$Sepal.Length)

m_dt = round(mean(dt$`iris$Sepal.Length`),3)

set.seed(100)

sum = data.frame(seq(from = 0, to = 10, by = 0.001))
sum$count = 0
colnames(sum) = c('Mean', 'Frequency')
i=1
y=1


no = 10000
plots = seq(from = 0, to = no, by = 100)

saveGIF(while (i<=no) {
    
    s = sample(dt$`iris$Sepal.Length`, 15)
    m = round(mean(s),3)
    sum$Frequency[sum$Mean == m] = sum$Frequency[sum$Mean == m] + 1
    
    if (i %in% plots) {
    
        plot1 <- ggplot(data=dt, aes(dt$`iris$Sepal.Length`)) + geom_histogram(bins = 150) +
                 ggtitle("Histogram of Sepal Lengths") +
                 geom_vline(xintercept = s, colour="red", linetype = "longdash", size = 0.5) + 
                 ylab("Count") + 
                 xlab("Sepal length") +
                 theme_bw() + 
                 geom_blank() +
                 theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
            
        plot2 <- ggplot(data=sum, aes(sum$Mean, sum$Frequency)) + geom_point() + 
                 scale_y_continuous(limits = c(0, 160)) +
                 scale_x_continuous(limits = c(4, 8)) +
                 ggtitle("Sample Means of Sepal Lengths") +
                 geom_vline(xintercept = m_dt, colour="red", linetype = "longdash", size = 0.5) +
                 geom_path() +
                 annotate("text", x = 4.7, y = 140, label = paste("No. of samples: ",i)) +
                 annotate("text", x = 7.2, y = 130, label = paste("Population mean: ",m_dt)) +
                 annotate("text", x = 7.2, y = 120, label = "Sample size: 15") +
                 ylab("Count") + 
                 xlab("Mean") + 
                 theme_bw() + 
                 geom_blank() +
                 theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
                 
             grid.arrange(plot1, plot2, ncol=2)
        
    }

    i=i+1
    
}, movie.name = "Sepal_ani.gif", interval = 0.01, convert = "convert", ani.width = 800, 
ani.height = 600)
