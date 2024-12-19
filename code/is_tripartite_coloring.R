is_tripartite_coloring <- function(graph) {
  n <- vcount(graph)            # Number of vertices
  colors <- rep(0, n)           # 0: uncolored, 1: color 1, 2: color 2, 3: color 3
  
  # BFS to color the graph
  for (start in V(graph)) {
    if (colors[start] == 0) {   # Start a new BFS if the vertex is uncolored
      colors[start] <- 1        # Assign the first color
      queue <- c(start)         # Initialize queue for BFS
      
      while (length(queue) > 0) {
        current <- queue[1]     # Get the first element in the queue
        queue <- queue[-1]      # Remove it from the queue
        
        for (neighbor in neighbors(graph, current)) {
          if (colors[neighbor] == 0) {
            # Assign the next color in a cycle of 3 colors
            colors[neighbor] <- (colors[current] %% 3) + 1
            queue <- c(queue, neighbor) # Add to the queue
          } else if (colors[neighbor] == colors[current]) {
            # If a neighbor has the same color, the graph is not tripartite
            return(FALSE)
          }
        }
      }
    }
  }
  return(TRUE)  # All vertices were colored successfully
}
