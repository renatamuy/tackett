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

if(!require(ggalluvial)){
  install.packages("ggalluvial")
  library(ggalluvial)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(ggraph)){
  install.packages("ggraph")
  library(ggraph)
}

if(!require(ggrepel)){
  install.packages("ggrepel")
  library(ggrepel)
}

if(!require(htmlwidgets)){
  install.packages("htmlwidgets")
  library(htmlwidgets)
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

if(!require(tidygraph)){
  install.packages("tidygraph")
  library(tidygraph)
}

if(!require(webshot)){
  install.packages("webshot")
  library(webshot)
}

if(!require(xlsx)){
  install.packages("xlsx")
  library(xlsx)
}

if(!require(emln)){
  devtools::install_github('Ecological-Complexity-Lab/emln', force=T)
  library(emln)
}
  

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
head(df)
colnames(df)


# Layers to explore 

df[c('species', 'body.part', 'cures'  )]

unique(df$IUCN.Region)
unique(df$species)
unique(df$body.part)
unique(df$cures)


# Network version 1

df <- df %>%
  filter(species != "unknown")

df <- df %>%
  mutate(across(everything(), ~ gsub("^unknown\\s*$", "unknown", .)))

df <- df %>%
  mutate(across(everything(), ~ gsub("^unknown\\s*$", "unknown", .)))

str(df)
head(df)
colnames(df)

links <- df %>%
  count(species, body.part, cures) %>%
  rename(value = n) %>%
  pivot_longer(cols = c(body.part, cures), names_to = "target_type", values_to = "target") %>%
  select(source = species, target, value)

nodes <- unique(c(links$source, links$target))
nodes <- data.frame(name = nodes)

str(nodes)
head(nodes)
tail(nodes)

str(links)
head(links)
tail(links)

g1 <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)
g1
vertex_attr(g1)
edge_attr(g1)
plot(g1)


# Network version 2

df2 <- tibble(
  species = df$species,
  body.part = df$body.part,
  cures = df$cures)


df2

# Check the unique values in each column
unique_species <- unique(df2$species)
unique_body_part <- unique(df2$body.part)
unique_cures <- unique(df2$cures)

# Verify none of these are empty
if (length(unique_species) == 0 || length(unique_body_part) == 0 || length(unique_cures) == 0) {
  stop("One or more columns in df2 are empty. Please check the input data.")
}

# Combine all nodes
all_vertices <- c(unique_species, unique_body_part, unique_cures)
head(all_vertices)
tail(all_vertices)

# Create the node types
vertex_types <- c(
  rep("species", length(unique_species)),
  rep("body.part", length(unique_body_part)),
  rep("cures", length(unique_cures))
)

# Check that lengths match
if (length(all_vertices) != length(vertex_types)) {
  stop("Mismatch in lengths of vertices and their types. Check the input data.")
}

# Construct the node data frame
vertex_list <- data.frame(
  name = all_vertices,
  type = vertex_types
)

# View the node list
nodes2 <- vertex_list
head(nodes2)
tail(nodes2)

duplicates <- duplicated(nodes2$name)
duplicates
nodes2$name == "unknown"
nodes2$name[duplicates] <- paste0(nodes2$name[duplicates], "_", seq_along(nodes2$name[duplicates]))
nodes2

links2 <- df2 %>%
  count(species, body.part, cures) %>%
  rename(value = n) %>%
  pivot_longer(cols = c(body.part, cures), names_to = "target_type", values_to = "target") %>%
  select(source = species, target, value)
links2

g2 <- graph_from_data_frame(d = links2, vertices = nodes2, directed = TRUE)
g2
vertex_attr(g2)
edge_attr(g2)
plot(g2)


# Network version 3

colnames(df)

edges_set1_set2 <- data.frame(from = df$species, to = df$body.part)
edges_set2_set3 <- data.frame(from = df$body.part, to = df$cures)
edges3 <- rbind(edges_set1_set2, edges_set2_set3)
head(edges3)

# Create graph
g3 <- graph_from_data_frame(edges3, directed = FALSE)
g3

# Assign node groups
V(g3)$group <- ifelse(V(g3)$name %in% df$species, 1,
                      ifelse(V(g3)$name %in% df$body.part, 2, 3))
V(g3)$group
V(g3)$level <- V(g3)$group

vertex_attr(g3)
edge_attr(g3)


# Set node and link properties

V(g3)$type <- ifelse(V(g3)$group == 1, "species", "variable")
V(g3)$type <- ifelse(V(g3)$group == 2, "body.part", V(g3)$type)
V(g3)$type <- ifelse(V(g3)$group == 3, "cures", V(g3)$type)

V(g3)$color <- ifelse(V(g3)$type == "species", "#DCC949", "#CD8862")
V(g3)$color <- ifelse(V(g3)$type == "body.part", "#7D9D33", V(g3)$color)

vertex_attr(g3)

E(g3)$weight <- 1
E(g3)$width <- E(g3)$weight / max(E(g3)$weight) * 2  # Scale link widths
E(g3)$curved <- 0.2 
E(g3)$color <- V(g3)$color[ends(g3, es = E(g3), names = FALSE)[, 2]] #Fixed!

edge_attr(g3)

plot(g3)


################################ SANKEY ########################################


sankey <- links %>%
  e_charts() %>%
  e_sankey(links, source = source, target = target, value = value) %>%
  e_theme("caravan")
sankey

# Save chart as HTML
html_file <- "../figures/sankey.html"
saveWidget(sankey, html_file, selfcontained = TRUE)

# Convert HTML to PNG
png_file <- "../figures/tackett_network_sankey.png"
webshot(html_file, png_file, vwidth = 3000, vheight = 2000)


################################ ALLUVIAL ######################################

# This is my favorite so far, but I still want to improve its visualization,
# especially regarding node label overlap. We could also change the grey tone
# of the strata.

png(file = "../figures/tackett_network_alluvial.png", 
    width = 6000, 
    height = 2000, 
    unit='px', 
    res = 300, 
    bg = "transparent")

ggplot(data = df,
       aes(axis1 = species, axis2 = body.part, axis3 = cures)) +
  geom_alluvium(aes(fill = species), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)),
            size = 2) +
  scale_x_discrete(limits = c("species", "body.part", "cures"), expand = c(0.15, 0.15)) +
  labs(title = "Alluvial Plot of Species, Body Parts, and Cures",
       y = "Frequency") +
  theme_minimal()

dev.off()


################################ TRIPARTITE ####################################


setwd("../code")
getwd()
list.files()


# Functions to check if a graph is tripartite

source("../code/is_tripartite_edges.R")
source("../code/is_tripartite_coloring.R")


# Custom layout for tripartite graph

source("../code/layout_tripartite_type.R")
source("../code/layout_tripartite_level.R")


# Tripartite graph

png(file = "../figures/tackett_network_tripartite.png", 
    width = 2000, 
    height = 2000, 
    unit='px', 
    res = 300, 
    bg = "white")

par(mar = c(3, 3, 3, 3)) 

plot(
  g3,
  vertex.label.family = "Arial", 
  vertex.label.cex = 0.5,   
  layout = layout_tripartite_level, 
  vertex.size = 10,  
  vertex.label.cex = 1.5,  
  vertex.label.color = "gray30", 
  vertex.frame.color = "white",  
  edge.width = E(g3)$weight,
  edge.color = E(g3)$color,
  edge.arrow.size = 0,
  main = "Fruit bat costs and benefits per state"
)

dev.off()


# Circular graph

png(file = "../figures/tackett_network_circular.png", 
    width = 2000, 
    height = 2000, 
    unit='px', 
    res = 300, 
    bg = "transparent")

par(mar = c(3, 3, 3, 3)) 

plot(
  g3,
  vertex.label.family = "Arial", 
  vertex.label.cex = 0.5,   
  layout = layout_in_circle, 
  vertex.size = 10,  
  vertex.label.cex = 1.5,  
  vertex.label.color = "gray30", 
  vertex.frame.color = "white",  
  edge.width = E(g3)$weight,
  edge.color = E(g3)$color,
  edge.arrow.size = 0,
  main = "Fruit bat costs and benefits per state"
)

dev.off()


################################################################################