is_tripartite_edges <- function(graph) {
  edge_list <- as_edgelist(graph)
  for (edge in seq_len(nrow(edge_list))) {
    v1 <- edge_list[edge, 1]
    v2 <- edge_list[edge, 2]
    if (V(graph)$type[v1] == V(graph)$type[v2]) {
      return(FALSE)  # Found an edge within the same set
    }
  }
  return(TRUE)  # All edges respect the tripartite property
}