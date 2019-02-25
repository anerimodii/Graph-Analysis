data <- read.table("C:/Users/aneri/Downloads/roadNet-CA.txt",header=F) #reads the files from the destination
mat_data <- as.matrix(data) #mat_data stores the data as matrix
v1 <- mat_data[,1] 
v2 <- mat_data[,2] 
relations<- data.frame(from=v1,to=v2) #relations has the dataframe from vertices 1 to 2
g<-graph.data.frame(relations,directed=TRUE) #converts the data frame into the graph
options(max.print=100000000) #for printing maximum
degree1 <- V(g)[degree(g, mode = 'out')==1 & degree(g, mode = 'in')==1] #stores the in and out degrees which are 1
g1 <- delete_vertices(g,degree1) #deletes the degree 1 vertices
degree2 <- V(g1)[degree(g1, mode = 'out')<3 & degree(g1, mode = 'in')<3]  #stores the in and out degrees which are less than 3
g2 <- delete_vertices(g1,degree2)  #deletes the  vertices less than 3
degree3 <- V(g2)[degree(g2, mode = 'out')<4 & degree(g2, mode = 'in')<4] #stores the in and out degrees which are less than 4
g3 <- delete_vertices(g2,degree3)  #deletes the  vertices less than 4
vcount(g3)
is.directed(g3) #checks whether the graph is directed
is.connected(g3) #checks whether the graph is connected
edge_density(g3,loops=TRUE) #checks the edge density
neighbors(g3,v,mode=c("out","in","all","total")) #checks the neighbors
ego_size(g3, order = 1, nodes = V(g3), mode = c("all", "out", "in"),mindist = 0) #checks the ego_size
mst(g3, weights = NULL, algorithm = NULL) #checks the minimum spannig tree
eccentricity(g3,vids=V(g3),mode=c("all","out","in","total")) #checks the eccentricity
is.loop(g3) #checks whether graph has loops
is.simple(g3) #checks whether the graph is simple
is.connected(g3) # checks whether the graph is connected or not
matrix.df <- as.data.frame(mat_data) #matrix dataframe
eigen_centrality(g3,directed = TRUE, scale =TRUE, weights=NULL, options = arpack_defaults) #checks the eigen_centrality
edge.disjoint.paths(g3,4,10) #checks the disjoint path
centperson <- function(g3) {
  a1 <- degree(g3) #it stores the degree of all vertices in g3
  max <- a1[1]   #a1[1] has the degree of node 1
  for(i in 2:vcount(g3)) #we will iterate from 2 to the vcount(g3) which will the last count
  {
    if (max<a1[i]) #this will help us in finding the maximum of the degree node
      max <-  a1[i] #this will substitute the value of it
}  
  print(max) #prints the max value
}
V(g3)$name[degree(g3)==max(degree(g3))] 
page_rank(g3)
pagerank <- page_rank(g3)
head(pagerank)
va <- vertex_attr(g3)
head(va) 
alpha_centrality(g3,alpha=0.9) #checks the alpha_centrality
clique_num(g3) #checks the largest clique
diameter(g3,directed=TRUE,weights=NA) #checks the largest length
get_diameter(g3,directed=TRUE,weights=NA) #lists the paths with largest length

