# This script is a supplement to the manuscript:
# Muylaert et al., in prep.
# See README for further info: https://github.com/renatamuy/tackett
# Original data source: Tacket et al. (2022)  - https://doi.org/10.3390/d14030179

#Load or install the required packages
packages <- c("here", "echarts4r", "ggalluvial",
              "ggplot2", "ggraph", "ggrepel", "htmlwidgets",
              "igraph", "networkD3", "tidyverse", "tidygraph",
              "webshot", "xlsx", "devtools", "rJava")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


if (!requireNamespace("emln", quietly = TRUE)) {
  message("'emln' package not found. Installing from GitHub...")
  devtools::install_github("Ecological-Complexity-Lab/emln", force = TRUE)
} else {
  message("'emln' package is already installed.")
}
library(emln)
message("'emln' package loaded successfully.")


# Set the stage
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list= ls())

setwd("../data")
getwd()
list.files()


# Get the data
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


# Initial network setup
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


# Additional network setup
colnames(df)

edges_set1_set2 <- data.frame(from = df$species, to = df$body.part)
edges_set2_set3 <- data.frame(from = df$body.part, to = df$cures)
edges3 <- rbind(edges_set1_set2, edges_set2_set3)
head(edges3)


# Create graph
g2 <- graph_from_data_frame(edges3, directed = FALSE)
g2


# Assign node groups
V(g2)$group <- ifelse(V(g2)$name %in% df$species, 1,
                      ifelse(V(g2)$name %in% df$body.part, 2, 3))
V(g2)$group
V(g2)$level <- V(g2)$group

vertex_attr(g2)
edge_attr(g2)


# Set vertex and edge properties
V(g2)$type <- ifelse(V(g2)$group == 1, "species", "variable")
V(g2)$type <- ifelse(V(g2)$group == 2, "body.part", V(g2)$type)
V(g2)$type <- ifelse(V(g2)$group == 3, "cures", V(g2)$type)

V(g2)$color <- ifelse(V(g2)$type == "species", "#DCC949", "#CD8862")
V(g2)$color <- ifelse(V(g2)$type == "body.part", "#7D9D33", V(g2)$color)

vertex_attr(g2)

E(g2)$weight <- 1
E(g2)$width <- E(g2)$weight / max(E(g2)$weight) * 2  # Scale link widths
E(g2)$curved <- 0.2 
E(g2)$color <- V(g2)$color[ends(g2, es = E(g2), names = FALSE)[, 2]] #Fixed!

edge_attr(g2)


# Set the tripartite network
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
png(file = "../figures/tackett_network.png", 
    width = 4000, 
    height = 3000, 
    unit='px', 
    res = 300, 
    bg = "white")

par(mar = c(3, 3, 3, 3)) 

plot(
  g2,
  vertex.label.family = "Arial", 
  vertex.label.cex = 0.5,   
  layout = layout_tripartite_level, 
  vertex.size = 10,  
  vertex.label.cex = 2.5,  
  vertex.label.color = "gray30", 
  vertex.frame.color = "white",  
  edge.width = E(g2)$weight,
  edge.color = E(g2)$color,
  edge.arrow.size = 0,
  main = "Medicinal use of bats"
)

dev.off()
