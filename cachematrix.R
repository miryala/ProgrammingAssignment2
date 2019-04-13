## We have two functions here makeCacheMatrix and cacheSolve
## makeCacheMatrix creates our own data type which contains original matrix
## and its inverse matrix in a list
## cacheSolve tries to get inverse matrix from our cacheMatrix data type, if it finds
## it returns directly from cache, else it solves inverse and stores in cache and returns result

## We create four functions set,get,setInverse,getInverse
## we can set matrix and its inverse using set and setInverse functions
## we can retrieve corresponding data using get functions

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  setMatrix <- function(mat){
    x<<- mat
    inverseM <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) inverseM <<- inv
  getInverse <- function() inverseM
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Get inverse from cacheMatrix, if exists return result direclty
## if not, calculate inverse, store it in cahceMactrix and return resukt

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverse()
  if(!is.null(inverseM)){
    print("getting from cache!!")
    return(inverseM)
  } 
  mat <- x$getMatrix()
  inverseM <- solve(mat, ...)
  x$setInverse(inverseM)
  inverseM
}
