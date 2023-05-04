install.packages("gplots")
install.packages("heatmap.plus")
install.packages("RColorBrewer")
install.packages("viridis")
install.packages("heatmap3")

library("gplots")
library("heatmap.plus")
library("RColorBrewer")
library(heatmap3)
library(viridis)
data_1 <- ARG_genes...Copy[,5:26]        # take just the genera part of the matrix

data_2 <- ARG_genes...Copy[,1:4] 


heatmap_log <-log((data_1+1))
write.table(heatmap_log, "ARG_genes_log.tsv", sep="\t", row.names=TRUE, col.names=NA, quote=FALSE)

heatmap_log <- data_1
Antibiotic <-c("Enro", "no_Antib")
Antibiotic_color <-c(brewer.pal(2, "Set1"))
Nanoparticles <-c("Lignin", "PLGA", "none")
Nanoparticles_color <-c(brewer.pal(3, "Set2"))
Time <-c("24", "72")
Time_color <-c(brewer.pal(2, "Set3"))

Antibiotic_color<- unlist(lapply(rownames(heatmap_log),function(x){
  if(grepl("Enro",x)) '#E41A1C' #pink
  else if(grepl('no_antibiotic',x)) '#377EB8' 
}))

Nanoparticles_color<- unlist(lapply(rownames(heatmap_log),function(x){
  if(grepl("Lignin",x)) '#66C2A5'
  else if(grepl('PLGA',x)) '#FC8D62' 
  else if(grepl('no_particle',x)) '#8DA0CB'
}))

Time_color<- unlist(lapply(rownames(heatmap_log),function(x){
  if(grepl("24",x)) '#8DD3C7'
  else if(grepl('72',x)) '#BEBADA'
}))

myCols <- cbind(Antibiotic_color,Nanoparticles_color,Time_color)
colnames(myCols)[1] <- "Antibiotic"
colnames(myCols)[2] <- "Nanoparticle"
colnames(myCols)[3] <- "Time"


heatmap3(heatmap_log, Colv = NULL, Rowv = NA,  col=inferno(8),cexRow=1,cexCol=0.9, main="Abundance KEGG Orthhologs Pathways", margins = c(16,0), 
             RowSideColors=myCols, distfunR = function(heatmap_log) dist(heatmap_log, method="euclidean"))

legend(0.8,0.794,legend=c("Enro", "Empty"), fill=c('#E41A1C', '#377EB8'),cex=0.8)
legend(0.8,0.67,legend=c("Lignin", "PLGA", "None"),fill=c('#66C2A5', '#FC8D62', '#8DA0CB'),cex=0.8,)
legend(0.8,0.502,legend=c("24", "48", "72"),fill=c('#8DD3C7', '#FFFFB3', '#BEBADA'),cex=0.8,)

#Time-specific 
T24 <-as.data.frame(`heatmap_log`[grep("24", row.names(`heatmap_log`)),])
T48 <-as.data.frame(`heatmap_log`[grep("48", row.names(`heatmap_log`)),])
T72 <-as.data.frame(`heatmap_log`[grep("72", row.names(`heatmap_log`)),])

heatmap3(T24, Colv = NA, Rowv = NULL,  col=inferno(20),cexRow=1,cexCol=1, margins = c(15,15), main="Abundance KEGG Orthhologs Pathways",
         RowSideColors=myCols, distfunR = function(T24) dist(T24, method="euclidean"))

heatmap3(T72, Colv = NA, Rowv = NULL,  col=inferno(20),cexRow=1,cexCol=1, margins = c(15,15), main="Abundance KEGG Orthhologs Pathways",
         distfunR = function(Exo) dist(Exo, method="euclidean"))

#Exo-specific 
heatmap_log <-log((heatmap_Exo.specific_pathways+1))

Location_color<- unlist(lapply(rownames(heatmap_log),function(x){
  if(grepl("T1",x)) '#E41A1C' #pink
  else if(grepl('T2',x)) '#377EB8' 
  else if(grepl('T3',x)) '#4DAF4A'
  else if(grepl('B1',x)) '#984EA3'
  else if(grepl('B2',x)) '#FF7F00' 
  else if(grepl('B3',x)) '#FFFF33'
  
}))

Media_color<- unlist(lapply(rownames(heatmap_log),function(x){
  if(grepl("Corn",x)) '#66C2A5'
  else if(grepl('Oat',x)) '#FC8D62' 
  else if(grepl('grain',x)) '#8DA0CB'
  else if(grepl('Pot',x)) '#E78AC3'
  
  
}))

myCols <- cbind(Media_color,Location_color)

heatmap3(heatmap_log, Colv = NULL, Rowv = NULL,  col=inferno(20),cexRow=1,cexCol=1, margins = c(15,15), main="Abundance KEGG Orthhologs Pathways",
         RowSideColors=myCols, distfunR = function(heatmap_log) dist(heatmap_log, method="euclidean"))

legend(0.8,0.58,legend=c("T1=Orange tree", "T2= Persimmon tree", "T3= Cycas", "B1= Yaupon", "B2= Wax myrtle", "B3= Boxwood"), fill=c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00' ,'#FFFF33'),cex=0.8)
legend(0.8,0.30,legend=c("Cornmeal", "Oatmeal", "Sorghum grain", "Potato-carrot"),fill=c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3'),cex=0.8,)
legend(0.8,0.10,legend=c("Exo-bacteria", "Endo-bacteria"),fill=c('#fbff0b', '#0b0bff'),cex=0.8,)


