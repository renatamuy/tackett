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

if(!require(xlsx)){
  install.packages("xlsx")
  library(xlsx)
}

devtools::install_github('Ecological-Complexity-Lab/emln', force=T)
library(emln)


setwd(here())
setwd('data')

df <- read.xlsx('Supplementary Materials-Table S3.xlsx', sheetIndex = 1, startRow=2)

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

links

nodes <- unique(c(links$source, links$target))
nodes <- data.frame(name = nodes)

head(nodes)
head(links)

# Plot a cool sankey graph 

links %>%
  e_charts() %>%
  e_sankey(links, source = source, target = target, value = value) %>%
  e_theme("macarons")


# Multilayer attempts -----------------

ggplot(data = df,
       aes(axis1 = species, axis2 = body.part, axis3 = cures)) +
  geom_alluvium(aes(fill = species), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("species", "body.part", "cures"), expand = c(0.15, 0.15)) +
  labs(title = "Alluvial Plot of Species, Body Parts, and Cures",
       y = "Frequency") +
  theme_minimal()


#-------------------------------------------------------------------------------------------------------------