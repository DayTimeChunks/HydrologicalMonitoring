

library(vegan)
library(ggplot2)
library(grid)
data(dune)

# calculate distance for NMDS
NMDS.log<-log(dune+1)
sol <- metaMDS(NMDS.log)

# Create meta data for grouping
MyMeta = data.frame(
  sites = c(2,13,4,16,6,1,8,5,17,15,10,11,9,18,3,20,14,19,12,7),
  amt = c("hi", "hi", "hi", "md", "lo", "hi", "hi", "lo", "md", "md", "lo", 
          "lo", "hi", "lo", "hi", "md", "md", "lo", "hi", "lo"),
  row.names = "sites")

# plot NMDS using basic plot function and color points by "amt" from MyMeta
plot(sol$points, col = MyMeta$amt)

# same in ggplot2
NMDS = data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2])
ggplot(data = NMDS, aes(MDS1, MDS2)) + 
  geom_point(aes(data = MyMeta, color = MyMeta$amt))

# Add species loadings and save them as data frame. 
# Directions of arrows cosines are stored in list vectors and matrix arrows. 
# To get coordinates of the arrows those direction values should be multiplied
# by square root of r2 values that are stored in vectors$r. 
# More straight forward way is to use function scores() as provided in answer of 
# @Gavin Simpson. Then add new column containing species names.

vec.sp<-envfit(sol$points, NMDS.log, perm=1000)
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species<-rownames(vec.sp.df)

# plot(vec.sp, p.max=0.1, col="blue")

ggplot(data = NMDS, aes(MDS1, MDS2)) + 
  geom_point(aes(data = MyMeta, color = MyMeta$amt))+
  geom_segment(data=vec.sp.df,aes(x=0,xend=MDS1,y=0,yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")),colour="grey",inherit_aes=FALSE) + 
  geom_text(data=vec.sp.df,aes(x=MDS1,y=MDS2,label=species),size=5)+
  coord_fixed()