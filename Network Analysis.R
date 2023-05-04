https://www.youtube.com/watch?v=aw--_pR-WhA

https://kateto.net/netscix2016.html

install.packages(c("igraph", "readr", "tidyr", "RColorBrewer"))
#library for network exploration
library(igraph)
library(readr)
library(tidyr)
simpleNetwork(Network)


#create a graph g based on the dataframe Edgelist: the whole sets of interactions and Vertices: the different actors in the interaction 
g1<-graph_from_data_frame(d = edge_resistance_24.48.72 , vertices = vertices_resistance_24.48.72, directed = FALSE)
plot(g1)

g1<-graph_from_data_frame(d = edge_resistance , vertices = vertices_ARG, directed = FALSE)
plot(g1)

#check the number of edges that each ID has
E(g1)[[inc('C24')]]
g1

#change the layout
m1 <- layout_nicely(g1)

a1 <- layout.auto(g1)

k1 <- layout.kamada.kawai(g1)

#plot based on the different layout
plot(g1, vertex.label.color = "black", layout = k1)

# Create a vector of weights based on the number of hours each pair spend together
w1 <- E(g1)$Size

# Plot the network varying edges by weights

plot(g1, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = w1,
     layout = m1)



# Create a new igraph object by deleting edges that are less than 1 
g2 <- delete_edges(g1, E(g1)[w1 < 5])
g3 <- delete_vertices(g2, E(g2)[w1 < 5])
# Plot layouts 
w2 <- E(g2)$Size
m2 <- layout_nicely(g2)
s1 <- layout.reingold.tilford(g1)


#1. Network Density
edge_density(g2) # Global density

library(RColorBrewer) # This is the color library

#==================================================================#
# In this section, you will measure the centrality of the igraph   #
# object, "Stucont". You will be able to see how the theoretical   #
# concept of each centrality such as degree, eigenvector, and      #
# betweenness centrality is measured by the igraph.                #
#==================================================================#

#1. Degree centrality
g1_deg<-degree(g1,mode=c("All"))
V(g1)$degree<-g1_deg
V(g1)$degree
which.max(g1_deg)  #will give back the max degree of interaction
DF <- as_long_data_frame(g1)

#================== Measuring Network Structure ===================#
#==================================================================#
# In this section, you will measure the indicators of the network  #
# structure such as network density, assortativity.                #
#==================================================================#

E(g1)
#2. Assortativity
values <- as.numeric(factor(V(g1)$name))
assortativity_nominal(g1, types=values)

#2.1. Calculate the observed assortativity
observed.assortativity <- assortativity_nominal(g1, types=values)
results <- vector('list', 1000)
for(i in 1:1000){results[[i]] <- assortativity_nominal(g1, sample(values))}

#2.2.  Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results), xlim = c(-0.5,0.1))
abline(v = observed.assortativity,col = "red", lty = 3, lwd=2)                                                             

library(RColorBrewer) # This is the color library
t1 <- E(g1)$Sample
t2 <- E(g2)$Sample
#color edges based on the interaction type
edge_color<- unlist(lapply(t1 ,function(x){
  if(grepl("C24",x)) '#acabab'
  else if(grepl('E24',x)) '#fb8081'
  else if(grepl('P24',x)) '#7cc1f9'
  else if(grepl('L24',x)) '#59fb55'
  else if(grepl('PE24',x)) '#ea71fc'
  else if(grepl('LE24',x)) '#fba959'
  else if(grepl('C72',x)) '#050505'
  else if(grepl('E72',x)) '#fc0305'
  else if(grepl('P72',x)) '#03477f'
  else if(grepl('L72',x)) '#058802'
  else if(grepl('PE72',x)) '#750486'
  else if(grepl('LE72',x)) '#fc7f04'
  else if(grepl('C48',x)) '#5e5e5e'
  else if(grepl('E48',x)) '#f84547'
  else if(grepl('P48',x)) '#068dfb'
  else if(grepl('L48',x)) '#0dfb06'
  else if(grepl('PE48',x)) '#da05fa'
  else if(grepl('LE48',x)) '#fa9837'
}))


#color vertix based on the interaction Location
vert_color<- unlist(lapply(t1 ,function(x){
  if(grepl("24",x)) '#FF0101'
  else if(grepl('72',x)) '#01FF1B' 
}))

plot(g1,edge.color = edge_color,vertex.label.cex =0.6, 
     vertex.size= vertices_resistance_24.48.72$Size,
     edge.color = edge_color,
     edge.width = w1,
     vert_color= 'black',
     layout = m1)

#cluster based on the graph of interaction
ceb <- cluster_walktrap(g1) 
cs <- cluster_spinglass(g1)
ct <- cluster_edge_betweenness(g1) 

#check the clusering
dendPlot(ceb, mode="hclust")

# change the position/color/size of nodes and edges manually
tkid <- tkplot(g1) #tkid is the id of the tkplot that will open, after you move the different nodes you can store them

tkid <- tkplot.getcoords(tkid) # grab the coordinates from tkplot

tk_close(tkid, window.close = T)


remove(tkid)
plot(g1,edge.color = edge_color,vertex.label.cex =0.6, 
     vertex.size= vertices_resistance_24.48.72$Size/9,
     edge.color = edge_color,
     edge.width = w1,
     vert_color= 'black',
     layout = tkid) 

##correct one!!
plot(ceb, g1,edge.color = edge_color,
     edge.width=w1/5,
     vertex.size= vertices_resistance_24.48.72$Size/2,
     vertex.label.color="black",
     vertex.label.cex =0.9,
     vertex.label.dist=1.2,
     layout = tkid) 

#add legend to the last graph
legend("topright", inset = .02, title = "Edge color: Condition", ncol = 3,
       c("Control24", "E24", "P24", "L24", "PE24", "LE24","Control48", "E48", "P48", "L48", "PE48", "LE48", "Control72", "E72", "P72", "L72", "PE72", "LE72"), 
       fill = c('#acabab', '#fb8081', '#7cc1f9', '#59fb55', '#ea71fc', '#fba959', '#5e5e5e', '#f84547', '#068dfb', '#0dfb06', '#da05fa', '#fa9837', '#050505', '#fc0305', '#03477f', '#058802', '#750486', '#fc7f04'), cex=0.8)


#examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
membership(ceb) # community membership for each node
modularity(ceb) # how modular the graph partitioning is


clp <- cluster_label_prop(g1)

plot(clp, g1,edge.color = edge_color,
     vertex.label.cex =0.6,
     vertex.size=V(g1)$degree,
     layout = tk1) 



plot(g1,
     vertex.label.cex =0.6,
          layout = tk)

plot(g1,edge.color = edge_color,
     #edge.width=edge_ARG$Alignments,
     vertex.color = 'white',
     vertex.label.cex =0.7,
     vertex.size=V(g1)$degree,
     layout=tkid) 
#add legend to the last graph
legend("topright", inset = .02, title = "Time",
       c("24h", "72"), fill = c('#56b1f7', '#132b43'), cex=0.8)

