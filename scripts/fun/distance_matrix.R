## CREATE A MATRIX CALCULATING THE EUCLIDEAN DISTANCE BETWEEN ALL COCONUTS
## matrix will be used 

# cache the distance data between all items into a matrix
distance_matrix <- function(df) {
  # calculate the number of obj in the df
  no_of_obj <- nrow(df)  
  
  # create matrix
  obj_matrix <- matrix(
    data = 0, nrow = no_of_obj, ncol = no_of_obj,
    dimnames = list(
      c(seq.int(1, no_of_obj, 1)),
      c(seq.int(1, no_of_obj, 1))
    )
  )
  
  # xy location vectors to be reused
  location_i <- vector("numeric", length = 2)
  location_j <- vector("numeric", length = 2)
  
  # for each row, col combination compare to df and grab x and y and calc euclid 
  # distance, iterate row by row until all combinations are calculated
  for(i in seq_along(obj_matrix[1,])) {
    # loop through cols
    for (j in seq_along(obj_matrix[,1])) {
      # grab locations and cache for use in distance formula
      location_i <- c(df$x[[i]], df$y[[i]])
      location_j <- c(df$x[[j]], df$y[[j]])
      
      # calc euclid
      euclid_dist <- sqrt(
        (location_i[[1]] - location_j[[1]])^2 + (location_i[[2]] - location_j[[2]])^2
      )
      
      # store into matrix
      obj_matrix[i, j] <- euclid_dist
    }
  }
  
  return(obj_matrix)
}