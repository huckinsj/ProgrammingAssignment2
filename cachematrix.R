## cachematrix.r
## 11-11-2014 canhuck
##
## Version 1 - Rough draft complete, using example code as sample. Appears to work, need to
## dive into 'why' it works, IE: what does the list() do in the makeCacheMatrix() function?
## Can I remove it?


## This function creates a matrix object x that can also cache its own inverse 
## This function takes in a single matrix as an argument, and caches a copy of it using
## the setMatrix function.
##
## 
##
makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL

  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }

  get <- function() x
  
  setMatrix <- function(setMatrix) matrix <<- setMatrix
  
  getMatrix <- function() matrix
  
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  
  ## Set the "local" matrix variable equal to the value returned by the getMatrix function
  ## within the special matrix object passed in. If it is not null, then it is cached, and
  ## we can pull directly from the cache and display a message indicating such.
  ##
  ## if it is null, which is the default set on line 16 of the code, then we know we need to
  ## compute it from scratch and can skip this block
  matrix <- x$getMatrix()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  ## Call the get function contained within the special matrix object passed in to the function
  ## in order to get the data
  data <- x$get()
  
  ## Return a matrix that is the inverse of 'x'
  matrix <- solve(data)
  
  ## Set the cached version of the matrix so that the next time we call cacheSolve() we don't
  ## need to compute it
  x$setMatrix(matrix)
  matrix
}
