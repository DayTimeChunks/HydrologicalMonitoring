
require(ggplot2)
df <- data.frame(a = seq(0, 90, 10), b = seq(10, 100, 10))
df.plot <- ggplot(data = df, aes(x = seq(1, 100, 10))) + 
  geom_line(aes(y = a), colour = 'red') +
  geom_line(aes(y = b), colour = 'blue') +
  scale_x_continuous(breaks = seq(0,100,10))

# make dummy labels for the table content
df$lab <- month.abb[ceiling((df$a+1)/10)]

df.table <- ggplot(df, aes(x = a, y = 0,
                           label = lab, colour = b)) +
  geom_text(size = 3.5) + 
  theme_minimal() + 
  scale_y_continuous(breaks=NULL)+
  theme(panel.grid.major = element_blank(), legend.position = "none",
        panel.border = element_blank(), axis.text.x =  element_blank(),
        axis.ticks =  element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

gA <- ggplotGrob(df.plot)
gB <- ggplotGrob(df.table)[6,]
gB$heights <- unit(1,"line")


require(gridExtra)
gAB <- rbind(gA, gB)
grid.newpage()
grid.draw(gAB)