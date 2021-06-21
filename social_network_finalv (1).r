#BANA 277 Assignment: Social Network
#Members: Song Han, Poonam Ahuja, Isha Chaudhari, Sujaya Darke, Sammi Chang

# Install and Load Package
install.packages("igraph")
install.packages("sandwich")
install.packages("msm")
library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(plyr)
library(readr)
require(sandwich)
require(msm)

# Import Data
copurchase <- read_csv("copurchase.csv")
products <- read_csv("products.csv")

# Question 1
#filter products to only books that have a salesrank of <= 150000 and salesrank !=-1
new_product <- subset(products, products$group == 'Book' & products$salesrank <= 150000 & products$salesrank != -1)
book_id <- unique(new_product$id)
new_copurchase <- subset(copurchase, copurchase$Source %in% book_id & copurchase$Target %in% book_id)

# Question 2 & 3
graph1 <- graph.data.frame(new_copurchase, directed = T)
in_degree <- degree(graph1, mode='in')
out_degree <- degree(graph1, mode='out')

copurchase_list <- c(rbind(new_copurchase$Source, new_copurchase$Target))
graph3 <- graph(copurchase_list)
in_degree3 <- degree(graph3, mode='in')
out_degree3 <- degree(graph3, mode='out')

# Question 4
#find the node that has the max degree
all_degree <- degree(graph1, mode = 'all')
max_degree <- max(all_degree)
focal_id <- which(all_degree == max(all_degree))
subcomp_new <- subcomponent(graph1, '33', mode = 'all')
subcomo_product <- subset(new_product, new_product$id %in% subcomp_new)
subcomp_new

# Question 5
#plot the subgraph that shows all nodes connected to node "33"
subgraph1 <- subgraph(graph1, subcomp_new)

# Degree
in_degree1 <- degree(subgraph1, mode = "in")
out_degree1 <- degree(subgraph1, mode = 'out')

#### PLOT SUBGRAPH.

#sub_graph
#color nodes along diameter, and plot the subgraph in its entirety
diam <- get_diameter(subgraph1, directed=T)
vcol <- rep("gray40", vcount(subgraph1))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(subgraph1))
# E(net, path=diam) finds edges along a path and colors the edges orange, here 'diam'
ecol[E(subgraph1, path=diam)] <- "orange" 
# E(net, path=diam) finds edges along a path, here 'diam'
#with the node names are too messy, so i removed them
igraph.options(vertex.label = NA)
plot(subgraph1, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0, vertex.color='yellow', 
     vertex.label.cex = 0.8, vertex.size = (degree(subgraph1))*.2, layout= layout.kamada.kawai)
title("Subcomponent")

# Question 6
# Density
edge_density(subgraph1, loops=F)
# Closeness
close <- closeness(subgraph1, mode='all', weights=NA)
close
#Betweenness
between <- betweenness(subgraph1, directed='T', weights=NA)
between
V(subgraph1)$degree <- degree(subgraph1)
V(subgraph1)$label <- V(subgraph1)$name
#centrality
centr_degree(subgraph1)

#diameter
diam_length <- diameter(subgraph1, directed=T)


#Hub
hub <-hub.score(subgraph1)$vector
hub
#Authority
auth <- authority.score(subgraph1)$vector
auth

#histogram for degree distribution
par(mfrow = c(1,2))
degree.subgraph = degree(subgraph1)
hist(degree.subgraph, 100, col = "orange", xlab = "Degree", xlim = c(0, 38),
     ylab = "Frequency", main = "Deg. Dist. of Copurchase Subcomponent");


# Question 7
innerjoin <- inner_join(new_copurchase, new_product, by = c("Source"="id"))
aggregate1 <- aggregate(innerjoin, list(innerjoin$Target), mean)
aggregate2 <- aggregate1[c(-1, -2, -4, -5, -8)]
colnames(aggregate2) <- c('id', 'nghb_mn_salesrank', 'nghb_mn_review_cnt', 'nghb_mn_rating')

# Optional
#groupby <- group_by(innerjoin, Target) %>% 
        #summarize_each(funs(mean))
#groupby_new <- groupby[c( -2,-3, -4, -7)]
#colnames(groupby_new) <- c('id', 'nghb_mn_salesrank', 'nghb_mn_review_cnt', 'nghb_mn_rating')

# Neighbors’ mean rating (nghb_mn_rating)
nghb_mn_rating <- aggregate2$nghb_mn_rating
nghb_mn_rating

# Neighbors’ mean salesrank (nghb_mn_salesrank)
nghb_mn_salesrank <- aggregate2$nghb_mn_salesrank
nghb_mn_salesrank

# Neighbors’ mean number of reviews (nghb_mn_review_cnt)
nghb_mn_review_cnt <- aggregate2$nghb_mn_review_cnt
nghb_mn_review_cnt

# Question 8
close1 <- as.data.frame(close)
close1 <- cbind(id = rownames(close1), close1)

between1 <- as.data.frame(between)
between1<- cbind(id = rownames(between1), between1)

in_degree2 <- as.data.frame(in_degree1)
in_degree2 <- cbind(id = rownames(in_degree2), in_degree2)

out_degree2 <- as.data.frame(out_degree1)
out_degree2 <- cbind(id = rownames(out_degree2), out_degree2)

hub1 <- as.data.frame(hub)
hub1 <- cbind(id = rownames(hub1), hub1)

auth1 <- as.data.frame(auth)
auth1 <- cbind(id = rownames(auth1), auth1)

final_dataset <- cbind(hub1$id, hub1$hub, between1$between,auth1$auth, close1$close,in_degree2$in_degree1, out_degree2$out_degree1)
colnames(final_dataset) <- c('ID', 'hub', 'betweenness', 'authority', 'closeness', 'in_degree', 'out_degree')

final_dataset1 <- as.data.frame(final_dataset)
aggregate2$id <- as.character(aggregate2$id)

final_dataset2 <- inner_join(final_dataset1, aggregate2, by = c('ID' = 'id'))
new_product$id <- as.character(new_product$id)

final_dataset3 <- inner_join(new_product, final_dataset2, by = c('id' = 'ID'))
final_dataset4 <- final_dataset3[c(-2, -3)]
dim(final_dataset4)

final_dataset4$hub <- as.numeric(final_dataset4$hub)
final_dataset4$betweenness <- as.numeric(final_dataset4$betweenness)
final_dataset4$authority <- as.numeric(final_dataset4$authority)
final_dataset4$closeness <- as.numeric(final_dataset4$closeness)
final_dataset4$in_degree <- as.numeric(final_dataset4$in_degree)
final_dataset4$out_degree <- as.numeric(final_dataset4$out_degree)
final_dataset4$downloads <- as.numeric(final_dataset4$downloads)

summary(predict_salesrank <- glm(salesrank ~ review_cnt + rating + hub + betweenness + downloads +
                                         authority + closeness + in_degree + out_degree + nghb_mn_salesrank + nghb_mn_review_cnt + nghb_mn_rating, family="poisson", data = final_dataset4))






