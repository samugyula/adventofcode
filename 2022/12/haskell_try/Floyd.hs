module Floyd (floyd_warshall) where

floyd_warshall start end graph = (dist, [start] ++ route ++ [end])
  where dist  = shortest (start, end, length graph) graph
        route = path (start, end, length graph) graph
        
-- Calculates the value of shortest route
shortest (i,j,0) g = g !! (i-1) !! (j-1) -- Transition value from graph
shortest (i,j,k) g = min (shortest (i,j,k-1) g) $
                         (shortest (i,k,k-1) g) + (shortest (k,j,k-1) g)

-- Reconstructs the shortest path
path (i,j,0) _ = []
path (i,j,k) g
  | direct < step =  path(i,j,k-1) g
  | otherwise     = (path(i,k,k-1) g) ++ [k] ++ (path(k,j,k-1) g)
  where direct =  shortest (i,j,k-1) g
        step   = (shortest (i,k,k-1) g) + (shortest (k,j,k-1) g)
