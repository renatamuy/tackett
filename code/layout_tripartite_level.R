layout_tripartite_level <- function(graph) {
  level <- V(graph)$level
  x <- level
  y <- unlist(lapply(split(seq_along(level), level), function(indices) seq_along(indices)))
  cbind(x, y)
}