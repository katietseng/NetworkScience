---
title: "Intro to igraph"
author: "Katie Tseng"
course: "CPTS 592-Sp21-Elements of Network Science"
date: "2/5/2021"
---
#Network file formats: edgelist, pajek, graphml, gml, ncol, lgl, dimacs, graphdb
#<url> http://igraph.org/r/doc/read_graph.html

#setup
install.packages("igraph")
library(igraph)
getwd()
setwd("/Users/katietseng/Desktop/Classes/Network Science/NetworkScience")

#load data
load("/Users/katietseng/Desktop/Classes/Network Science/Lecture/termDocMatrix.rdata")
termDocMatrix[5:10, 1:20]

#make matrix boolean
termDocMatrix[termDocMatrix>=1] <- 1

#build term-term matrix (by multiplying term document matrix)
termMatrix <- termDocMatrix %*% t(termDocMatrix)
termMatrix[5:10, 5:10]

#build graph from termMatrix (how connected are these terms)
g <- graph.adjacency(termMatrix, weighted=T, mode='undirected')
print(g) #21 nodes, 151 edges

#import graph
g = read.graph("TermNetwork.gml","gml")

#display vertex and edge attributes
vertex_attr(g) #nodes
edge_attr(g) 

#set label and degree (access attributes)
V(g)$name
E(g)$weight

#plot graph
plot(g)
#remove self loops
g <- simplify(g)
plot(g)

#set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
V(g)$label
V(g)$degree

###CHANGE LAYOUT###

#make fruchterman reingold layout
layout1 <- layout.fruchterman.reingold(g)
#make kamada kawai layout
layout2 <- layout.kamada.kawai(g)
#make circle layout
layout3 <- layout.circle(g)
#make random layout
layout4 <- layout.random(g)

#plot layouts 1-4
#set seed to make the layout reproducible
set.seed(3014)
plot(g, layout1=layout1)
plot(g, layout2=layout2)
plot(g, layout3=layout3)
plot(g, layout4=layout4)

#set vertex label size (label.cex) proportional to degree
V(g)$label.cex <- 2*V(g)$degree/max(V(g)$degree)+.2
#change label color using rgb()
V(g)$label.color <- rgb(0,0,.2)
#change vertex color using rgb()
V(g)$color <- rgb(.5,.7,.9)
#remove outlines (frame.color)
V(g)$frame.color <- NA

#given, calculate edge thicknesses based on weight
egam <- (log(E(g)$weight)+.4)/max(log(E(g)$weight)+.4)
#set edge color
E(g)$color <- rgb(.2,.5,1)
#set edge widths to calculated values
E(g)$width <- egam

#plot the graph in layout1, note: tkplot lets you drag the nodes, handy
tkplot(g, layout=layout1)
plot(g, layout=layout1)

###STRUCTURAL PROPERTIES###

#find if the network is connected
is_connected(g)
#get the network diameter
diameter(g)
#get the maximum degree
max(degree(g))
#plot the network's (cumulative) degree distribution
plot(degree_distribution(g, cumulative=TRUE))