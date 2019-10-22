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

plot.igraph(graph, layout=layout_with_dh, width = 100,vertex.color="green")
comps <- components(graph)$Type
colbar <- rainbow(max(comps)+1)
V(graph)$color <- colbar[comps+1]
plot(graph, layout=layout_with_fr, vertex.size=30, label.cex=2, arrow.size = 44, arrow.width = 45, width = 1, edge.w)

plot(graph, edge.arrow.size=.1, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(graph)$from, vertex.label.color="black", vertex.s)

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold", "green", "blue", "red")
V(graph)$color <- colrs[V(graph)$Type]
# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6
# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA
# Set edge width based on weight:
E(net)$width <- E(net)$weight/6
#change arrow size and edge color:
E(graph)$arrow.size <- .2
E(graph)$width <- 13
E(graph)$edge.color <- "gray80"
E(graph)$width <- 1+E(graph)$weight/12
plot(graph)


vn <- toVisNetworkData(graph)


vn$edges$color <- ifelse(vn$edges$Type == "Solution", "tomato",
                         ifelse(vn$edges$Type == "Business Name", "green",
                                ifelse(vn$edges$Type == "Source Business Name", "brown", 
                                       ifelse(vn$edges$Type == "Source Table Name", "red",
                                              ifelse(vn$edges$Type == "Destination Business Name", "blue","grey")))))

vn$nodes$color  <- heat.colors(57, alpha=1)

network <- visNetwork(nodes = vn$nodes, edges = vn$edges, height = "1000px", width = "100%", background ="#cff0ff") %>%
  visOptions(highlightNearest = TRUE) %>%
  visIgraphLayout(layout = "layout_with_dh") %>%
  visNodes(shadow = "#f00e0e",borderWidth = 6) %>%
  visEdges(arrows = "to", arrowStrikethrough = F) 
k <-  visSave(network, file = "network.html")