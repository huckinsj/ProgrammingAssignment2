#################################
## cachematrix.r
## 11-11-2014 canhuck
##
## Version 1 - Rough draft complete, using example code as sample. Appears to work, need to
## dive into 'why' it works, IE: what does the list() do in the makeCacheMatrix() function?
## Can I remove it?
##
##
## Version 2 - Final draft complete. Determined that list() command at the end was required
## to return a list of functions to be used in the cacheSolve() function.
#################################


## This function creates a matrix object x that can also cache its own inverse 
## This function takes in a single matrix as an argument, and caches a copy of it using
## the setMatrix function.
makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    
    #Set the value of the original matrix passed in to the function to y
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    
    get <- function() x
    
    setMatrix <- function(setMatrix) matrix <<- setMatrix
    
    getMatrix <- function() matrix
    
    ## Return a list of 4 functions that belong to this matrix
    ## Get and set will get the matrix "normally", while setMatrix and getMatrix
    ## will get and set the value of the cached copy
    list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## Set the "local" matrix variable equal to the value returned by the getMatrix function
## within the special matrix object passed in. If it is not null, then it is cached, and
## we can pull directly from the cache and display a message indicating such.
##
## if it is null, which is the default set on line 16 of the code, then we know we need to
## compute it from scratch and can skip this block
cacheSolve <- function(x, ...) {    
   
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
