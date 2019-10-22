# ETL Process

# Importing libraries
install.packages("visNetwork")
install.packages("igraph")
install.packages("stringr")
library(visNetwork)
library(igraph)
library(stringr)

# Importing dataset
library(readxl)
ETL_Process <- read_excel("ETL_Process.xlsx")
ETL_Process <- subset.data.frame(ETL_Process, select = c(2:5, 13, 14,16, 17, 18, 20))
ETL_Process$Source_TableName = ifelse(is.na(ETL_Process$Source_TableName), "No Source Table",
                                      ETL_Process$Source_TableName)
ETL_Process$Destination_TableName = ifelse(is.na(ETL_Process$Destination_TableName), "No Source Table",
                                           ETL_Process$Destination_TableName)

#Edges
# install.packages("data.table")
library(data.table)

Solution <-data.frame(Node = ETL_Process$Solution, Type = c("Solution")) 
BN <-data.frame(Node = ETL_Process$BusinessName, Type = c("Business Name"))
SBN <-data.frame(Node = ETL_Process$Source_BusinessName, Type = c("Source Business Name"))
STN <-data.frame(Node = ETL_Process$Source_TableName, Type = c("Source Table Name"))
DBN <-data.frame(Node = ETL_Process$Destination_BusinessName, Type = c("Destination Business Name"))
DTN <-data.frame(Node = ETL_Process$Destination_TableName, Type = c("Destination Table Name"))
DTN2 <-data.frame(Node = ETL_Process$Destination_TableName, Type = c("Destination Table Name"))
Edges <- rbind(Solution,
               rbind(BN,
                     rbind(SBN,
                           rbind(STN, 
                                 rbind(DBN, 
                                       rbind(DTN, DTN2))))))

Edges <- unique(Edges)
Edges <- data.frame(Edges)
Edges$Node <- as.character(Edges$Node)
Edges$Type <- as.character(Edges$Type)

#Nodes
a <- subset.data.frame(ETL_Process, select = c(1, 2))
colnames(a) <- c("from", "to")
b <- subset.data.frame(ETL_Process, select = c(2, 5))
colnames(b) <- c("from", "to")
c <-subset.data.frame(ETL_Process, select = c(5, 7))
colnames(c) <- c("from", "to")
d <-subset.data.frame(ETL_Process, select = c(7, 8))
colnames(d) <- c("from", "to")
e <-subset.data.frame(ETL_Process, select = c(8, 10))
colnames(e) <- c("from", "to")
ETL <- rbind(a, b, by = c("from", "to"))
ETL <- rbind(ETL, c, by = c("from", "to"))  
ETL <- rbind(ETL, d, by = c("from", "to")) 
ETL <- rbind(ETL, e, by = c("from", "to")) 
ETL <- as.data.frame(ETL)
install.packages("dplyr")
library(dplyr)
ETL <- inner_join(ETL, Edges, by = c('from' = "Node"), all = FALSE)
ETL$Type <- as.character(ETL$Type)

#

graph <- graph_from_data_frame(ETL)
par(mar=c(0,0,0,0))
V(graph)$label.cex = 0.6
test.layout <- layout_(graph,with_dh(weight.edge.lengths = edge_density(graph)/10000))
vcol <- rep("grey40", vcount(graph))
vcol[E(graph)$Type=="Solution"] <- "#e62f22"
vcol[E(graph)$Type=="Business Name"] <-  "#e8922a"
vcol[E(graph)$Type=="Source Business Name"] <- "#edd93e"
vcol[E(graph)$Type=="Source Table Name"] <- "#aff037"
vcol[E(graph)$Type=="Destination Business Name"] <- "#37f07b"
vcol[E(graph)$Type=="Destination Table Name"] <- "#325ed9"
plot(graph, layout = test.layout, vertex.size=15, vertex.shape="circle", vertex.color = vcol,
     edge.width=1, edge.color = "#000000",
     edge.arrow.size=1, arrow.size = 1, 
     width = 1)


x <- ifelse(graph$Type == "Solution", "tomato",
            ifelse(graph$edges == "Business Name", "green",
                   ifelse(graph$edges== "Source Business Name", "brown", 
                          ifelse(graph$edges == "Source Table Name", "red",
                                 ifelse(graph$edges == "Destination Business Name", "blue","grey")))))

vn$nodes$color  <- heat.colors(57, alpha=1)