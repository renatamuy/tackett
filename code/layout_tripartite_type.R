layout_tripartite_type <- function(graph) {
  type <- V(graph)$type
  x <- type
  y <- unlist(lapply(split(seq_along(type), type), function(indices) seq_along(indices)))
  cbind(x, y)
}