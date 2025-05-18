# THIS CODE IS MY OWN WORK, IT WAS WRITTEN WITH CONSULTING GOOGLE AND FOLLOWING OTHERS SOLUTIONS

# STUDENT NAME: Jouni Kettunen

# After going into Moodle I found out that this weeks solutions were already there, not nice.
# Keeping my pride, I decided to return this flawed and hard googled work despite having an opportunity to do everything right by following the examples
# It's divided into original part and after part, the latter having solutions done in the class and me implementing snippets I found from internet.

# Set the working directory
setwd("~/BioR21/Week8")

#### TASK 1 #####


# load the igraph for visualization

library(igraph)

# make the yeast.graphml to an igraph object
yeastgraph <- read_graph("yeast.graphml", format = "graphml")

#### TASK 2 #####

# create the Global Efficiency function
global.efficiency <- function(graph) {
  
  # variable n to hold the number of vertices
  N <- vcount (graph)
 
  # dij to hold the shortest paths value
  dij <- shortest.paths(graph)
  
  #replaces zeros as infinite values
  dij[dij == 0] <- Inf
  
  # calculation part
  out <- (1/(N*(N-1))) * sum(1/dij)
  return(out)
}

global.efficiency(yeastgraph)

#### TASK 3 & 4 (original) #####

# my skills are far far far faaaaaaaar away to do any own implementations
# so here's the thing you advised; "emulation"
dummy.vulnerability <- function () {
  return(rnorm(1))
}

dummy.vulnerability()

# I didn't anymore know what to do, so I really got frustrated

# I'm sorry for the hard words but I left them to show my frustration

# I do understand that this is an university level course not a kindergarten, but still
# PLEASE: give more instructions when things are this hard. I really found myself hanging loose and got really frustrated
# This after all is a matter of continuing or not. If this is so hard and no help or no explanations, why to continue why to try at all?

# no idea how to use the function, but I did it like this
V(yeastgraph)$vul <- rnorm(vcount(yeastgraph)) 

# hopefully values have changed
V(yeastgraph)$vul

#### TASK 5 (original) ####

# is this the plotting function?
plot(yeastgraph)

V(yeastgraph)$color <- "pink"
# i understand that you can do a lot of coloring, but how. Why there wasn't any proper examples?!

#### TASK 3-5 (after) ####

# I feel that the function part is a mindless copy of others solution, I wish you would've explained this more thoroughly

# Task 3

vulnerability <- function(graph) {
 # variable E counts the efficiency and holds its value
  E <- global.efficiency(graph)
  
  # an empty vector
  result <- c()
  
  # start from the first vertex and travel to the last
  for(i in 1:vcount(graph)) {
    
    # Atte or Antti had this nice printing option to show where we are in every 100th vertice
    if(i %% 100 == 0){
      print(paste("prosessing vertex no ", i))
    }
    
    # I'm not sure what this does, but it was needed
    # are we saving instead of deleting things?
    ver.del <- delete.vertices(graph, V(graph)[i])
    
    # count the efficiency for the altered 
    E.i <- global.efficiency(ver.del)
    V <- (E-E.i)/E
    result <- c (result,V)
  }
  return(result)
}

# Task 4
#assign the vulnerability as a node attribute
V(yeastgraph)$vul <- vulnerability(yeastgraph)

# Task 5

# This is what I got after hours of googling and my own try-outs

# load the ready made colorlibrary
library(rcartocolor)

# find appropriate colorscale from colors that suit also to colorblind
display_carto_all(type = "all", colorblind_friendly = T)

# attach the colorscale to a colorRampPalette function that is used in the coloring (found this from Stackoverflow)
rbPal <- colorRampPalette(carto_pal(7, "SunsetDark"))

# use the 7 colors for the nodes
# is 7 enough?
V(yeastgraph)$Col <- rbPal(7)[as.numeric(cut(V(yeastgraph)$vul,breaks = 7))]

# inspect that the colors really are there
unique(V(yeastgraph)$Col)

# trying to make a reasonable graph with different layout options, found this and the latter part from "Network Visualization Cookbook"
coords <- layout_with_fr(yeastgraph, niter = 10000)
coords2 <- layout_with_graphopt(yeastgraph, niter = 10000)

# plot the network with adjusted layout and node options to get a better view
# aspect size asp = 0 to get more area covered, reduce vertex size from default 15 to 6, color edge to white and color vertices by their vulnerability
plot.igraph(yeastgraph, layout = coords, asp = 0, vertex.color = V(yeastgraph)$Col, vertex.frame.color = "white", vertex.size = 6, vertex.size2 = NA)
plot.igraph(yeastgraph, layout = coords2, asp = 0, vertex.color = V(yeastgraph)$Col, vertex.frame.color = "white", vertex.size = 6, vertex.size2 = NA)

# Neither of these look good, they are just one messy pile of labels.
# The good thing is that when I used the pseudofunction to create colors for the "vulnerability", the vertix colors actually change accordingly!

# Figured out that tkplot can also be used with same attributes as plot.igraph, bad side is that it seems to take a long time to finish. 
# Good side is that it shows the colors even better than plain plot.

tkplot(yeastgraph, layout = coords, asp = 0, vertex.color = V(yeastgraph)$Col, vertex.frame.color = "white", vertex.size = 6, vertex.size2 = NA)