## Template to produce figures in Bohr & Dunlap, "Key topics in environmental sociology, 1990–2014: results from a computational text analysis" (2018)

library(stm)
library(igraph)


## Run your stm model after loading/cleaning data
stm.output <- stm()


## Figure 1 (customize topic labels as needed)
plot(stm.output, type="summary", n=5, main="",
     topic.names = c("GOVERNANCE: ", "VALUES: ", "RISK & RISK PERCEPTION: ", "SOCIAL THEORY: ", "NATURAL RESOURCES: ", 
                     "AGRICULTURE: ", "CLIMATE & SOCIETY: ", "CULTURE: ", "EMISSIONS: ", "HEALTH & WELLBEING: ",
                     "DEVELOPMENT: ", "ENVIRONMENTAL INEQUALITY: ", "ENVIRONMENT & SOCIETY: ", "LOCAL ENVIRONMENT: ", "ENVIRONMENTAL CONCERN: ", 
                     "POLITICAL ECONOMY: ", "POLICY & PLANNING: ", "GENDER: ", "DEMOGRAPHY: ", "SOCIAL MOVEMENTS: ",
                     "POLITICAL ECOLOGY : ", "SUSTAINABILITY : ", "POLITICS: ", "ATTITUDES & BEHAVIOR: ", "TOURISM: "),
     text.cex=1.2,
     xlab="")
mtext("Expected Topic Proportion", side=1, line=3, cex=1.2) #adds custom Y-axis label


## Figures 2, 3, 7:9
## Per my email, I recommend you render such visualizations using the tidystm package: https://github.com/mikajoh/tidystm


## Figures 4:6

# create objects representing topic scores & correlation matrix
theta <- stm.output$theta
cor_theta <- cor(theta)

# create object to represent nodesize proportional to topic prevalence
nodesize <- colMeans(stm.output$theta)   #since topic proportions are all < 1, have to scale nodesize when plotting to get it to show up

# create igraph object
g <- graph.adjacency(cor_theta, weighted=TRUE, mode="lower")
g <- delete.edges(g, E(g)[weight < 0.1 ])  ## adjust this threshold as needed
g <- simplify(g, remove.multiple = T, remove.loops = T,
              edge.attr.comb=c(weight="sum", type="ignore") )
l <- layout_with_fr(g) #Fruchterman-Reingold
V(g)$color <- "lightgrey"

# igraph plot; adjust topic labels as needed, as well as optional 'mark.groups' and 'mark.col' arguments (below was used to render Figure 5)
plot(g, layout=l, #main="Graphical Display of 25 Topic Correlations (Tie Strength > .05)",
     vertex.label = c("GOVERNANCE", "VALUES", "RISK &\nRISK PERCEPTION", "SOCIAL THEORY\n", "NATURAL\nRESOURCES", 
                      "AGRICULTURE", "CLIMATE &\nSOCIETY", "CULTURE", "EMISSIONS", "HEALTH &\nWELLBEING",
                      "DEVELOPMENT", "ENVIRONMENTAL\nINEQUALITY", "ENVIRONMENT\n& SOCIETY", "LOCAL\nENVIRONMENT", "ENVIRONMENTAL\nCONCERN\n", 
                      "POLITICAL\nECONOMY", "POLICY &\nPLANNING", "GENDER", "DEMOGRAPHY", "SOCIAL\nMOVEMENTS",
                      "POLITICAL\nECOLOGY ", "SUSTAINABILITY ", "POLITICS", "\nATTITUDES &\nBEHAVIOR", "TOURISM "),
     vertex.color = V(g)$color, vertex.label.cex = 1, vertex.size=nodesize*500, 
     vertex.label.color="black",edge.color="grey", edge.lty="solid",
     mark.groups=list(c(2,4,5,10,12:15,17:19,24),
                      c(6,22),
                      c(7,25),
                      c(9,23)), # draws polygon around nodes
     mark.col=c("green","skyblue","pink","gold"), mark.border="black")

