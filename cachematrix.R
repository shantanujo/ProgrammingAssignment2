## This fucntion creates an inverse of a matrix solves it and caches the result, when this operation is done second time with the same matrix cached inverse values are returned without calculating them if the matrix is unchanged.


## makeCacheMatrix creates inverse of the matrix and creates a cache of it
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setin <- function(inverse) i <<- inverse
     getin <- function() i
     list(set = set, get = get,
          setin = setin,
          getin = getin)
}

## cacheSolve checks if the inverse of the matrix is solved if yes, then displays the result with "getting cached data";
## otherwise the matrix is solved. 

cacheSolve <-  function(x, ...) {
     i <- x$getin()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setin(i)
     i
}
