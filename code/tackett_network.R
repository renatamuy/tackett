# Tackett
# This repo is a supplement to the manuscript:
# Muylaert et al., in prep. Connections in the Dark: Network Science and 
# Social-Ecological Networks as Tools for Bat Conservation and Public Health.
# Global Union of Bat Diversity Networks (GBatNet).
# See README for further info: https://github.com/renatamuy/tackett


if(!require(here)){
  install.packages("here")
  library(here)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(echarts4r)){
  install.packages("echarts4r")
  library(echarts4r)
}

if(!require(emln)){
  install.packages("emln")
  library(emln)
}

if(!require(ggalluvial)){
  install.packages("ggalluvial")
  library(ggalluvial)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(igraph)){
  install.packages("igraph")
  library(igraph)
}

if(!require(networkD3)){
  install.packages("networkD3")
  library(networkD3)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(xlsx)){
  install.packages("xlsx")
  library(xlsx)
}

devtools::install_github('Ecological-Complexity-Lab/emln', force=T)
library(emln)


################################################################################


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list= ls())

setwd("../data")
getwd()
list.files()


################################################################################


df <- read.xlsx('Supplementary Materials-Table S3.xlsx', sheetIndex = 1, startRow=2)
str(df)
colnames(df)

# Layers to explore 

df[c('species', 'body.part', 'cures'  )]

unique(df$IUCN.Region)
unique(df$species)
unique(df$body.part)
unique(df$cures)

# Prep

df <- df %>%
  filter(species != "unknown")

df <- df %>%
  mutate(across(everything(), ~ gsub("^unknown\\s*$", "unknown", .)))

df <- df %>%
  mutate(across(everything(), ~ gsub("^unknown\\s*$", "unknown", .)))

links <- df %>%
  count(species, body.part, cures) %>%
  rename(value = n) %>%
  pivot_longer(cols = c(body.part, cures), names_to = "target_type", values_to = "target") %>%
  select(source = species, target, value)

nodes <- unique(c(links$source, links$target))
nodes <- data.frame(name = nodes)

head(nodes)
head(links)

g1 <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)
g1
vertex_attr(g1)
edge_attr(g1)


################################ SANKEY ########################################


links %>%
  e_charts() %>%
  e_sankey(links, source = source, target = target, value = value) %>%
  e_theme("caravan")


################################ MULTILAYER ####################################


ggplot(data = df,
       aes(axis1 = species, axis2 = body.part, axis3 = cures)) +
  geom_alluvium(aes(fill = species), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("species", "body.part", "cures"), expand = c(0.15, 0.15)) +
  labs(title = "Alluvial Plot of Species, Body Parts, and Cures",
       y = "Frequency") +
  theme_minimal()


################################ TRIPARTITE ####################################


setwd("../code")
getwd()
list.files()


### Testing solutions to draw the network as tripartite

# Functions to check if a graph is tripartite
source("../code/is_tripartite_edges.R")
source("../code/is_tripartite_coloring.R")


# Custom layout for tripartite graph
source("../code/layout_tripartite_type.R")
source("../code/layout_tripartite_level.R")


# Plot the network as tripartite

V(g1)$type
V(g1)$level
E(g1)$weight
E(g1)$width
degree(g1)
which(degree(g1) == 0)

edge_list_g1 <- as_edgelist(g1)
edge_list_g1

is_tripartite_coloring(g1)

png(file = "../figures/kadambari_network_tripartite.png", 
    width = 2000, 
    height = 2000, 
    unit='px', 
    res = 300, 
    bg = "white")

par(mar = c(3, 3, 3, 3)) 

plot(
  g1,
  vertex.label.family = "Arial", 
  vertex.label.cex = 0.5,   
  layout = layout_tripartite_level, 
  vertex.size = 22,  
  vertex.label.cex = 1.5,  
  vertex.label.color = "gray30", 
  #vertex.label.dist=1.9,
  vertex.frame.color = "white",  
  edge.width = E(g1)$width,
  #edge.color = "gray63",
  edge.color = E(g1)$color, 
  main = "Fruit bat costs and benefits per state"
)

dev.off()


################################################################################