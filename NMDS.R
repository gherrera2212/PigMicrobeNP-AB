# https://www.youtube.com/watch?v=QljEeBei-JA
library(vegan)

# Import dataset as .csv the file needs Col1: ID; Col2:metadata; Cols: genera

data_1 <- endo_MD[,5:107]        # take just the genera part of the matrix

data_2 <- endo_MD[,1:4] 


#ordination by NMDS
NMDS <- metaMDS(data_1, distance = "bray", k = 2)
NMDS <- metaMDS(data_1, distance = "euclidean", k = 2)

#########################
#Data visualisation (THIS IS AN UPDATED VERSION OF THE SCRIPT, NOW USING GGPLOT)
install.packages("ggplot2")
library("ggplot2")

#Extract the axes scores
datascores <- as.data.frame(scores(NMDS))  #extract the site scores

#Add/calculate spider diagram
scores <- cbind(as.data.frame(datascores), Location = data_2$Location, Media = data_2$Media,Fungi = data_2$Fungi )
centroids <- aggregate(cbind(NMDS1, NMDS2) ~ Location + Media + Fungi, data = scores, FUN = mean)
seg <- merge(scores, setNames(centroids, c('Location', 'Media', 'Fungi','oNMDS1','oNMDS2')),
             by = 'Location', sort = FALSE)

#plot
ggplot(scores, aes(x = NMDS1, y = NMDS2,colour = Fungi, shape = Location)) +
  #geom_segment(data = seg,
  #    mapping = aes(xend = oNMDS1, yend = oNMDS2)) + # add spiders
  # add centroids
  geom_point(aes(size = 0.8)) +  
  gghighlight::gghighlight() + 
  facet_wrap(vars(Media)) +
  coord_fixed()+                                              
  theme_bw()+ 
  theme(legend.position="right",legend.text=element_text(size=10),legend.direction='vertical')

ggplot(scores, aes(x = NMDS1, y = NMDS2,colour = Fungi, shape = Location)) +
  #geom_segment(data = seg,
  #    mapping = aes(xend = oNMDS1, yend = oNMDS2)) + # add spiders
  # add centroids
  geom_point(aes(size = 0.8)) +  
  coord_fixed()+                                              
  theme_bw()+ 
  theme(legend.position="bottom",legend.text=element_text(size=10),legend.direction='horizontal')

##Evaluate NMDS mapping
stressplot(NMDS)


#Evaluate Goodness, he larger circles are representing plots that don't have as strong as a fit with the original dissimilarity matrix.
gof <- goodness(NMDS)
plot(NMDS, type="t", main = "goodness of fit")
points(NMDS, display="sites", cex=gof*100)


#Compare the effect of different methods for the distances

pro <- procrustes(NMDS, NMDS2) # visually compares ordinations

plot(pro, cex=1.5)

#####################
#Bootstrapping and testing for differences between the groups
fit <- adonis2(data_1 ~ Fungi*Location*Media, data=data_2, permutations=2, method="euclidean")
fit


#####################
#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(data_1)
anova(betadisper(distances_data, data_2$ID))
