## These functions create and invert a matrix
## The second function chekcks to see if the inverse is already cached
## if already cached, it pulls the cached value, instead of recalculating

## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     
     #set variables
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
          get <- function() x
     setInverseMatrix <- function(solve) m <<- solve
     getInverseMatrix <- function() m
     
     #output list object for next function
     list(set = set, get = get,
          setInverseMatrix = setInverseMatrix,
          getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {

     m <- x$getInverseMatrix()
     #check to see if inverse matrix is already cached
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     #otherwise, get matrix and calculate inverse
     data <- x$get()
     m <- solve(data, ...)
     x$setInverseMatrix(m)
     #output inverse matrix, m
     m
}
