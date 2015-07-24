## The below two functions create a matrix object which 
## caches inverse of the associated matrix to reduce 
## computation time.

## Creates and returns a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                       ## initiates inverse of the matrix as null
  set <- function(y) {            ## sets y as the matrix associated with the object
    x <<- y
    m <<- NULL
  }
  get <- function() x             ## returns matrix associated with the object
  setinverse <- function(inverse) ## sets inverse of the matrix to input argument
  {
    m <<- inverse
  }
  getinverse <- function() m      ## returns stored matrix inverse 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns inverse of matrix associated with object by accessing cache if 
## inverse has been already computed
cacheSolve <- function(x, ...) {
  m <- x$getinverse()       ## return cached inverse in m
  if(!is.null(m)) {         ## inverse already computed, accessing cache data
    message("getting cached data")
    return(m)
  }
  data <- x$get()           ## extract matrix associated with object
  m <- solve(data, ...)     ## compute matrix inverse
  x$setinverse(m)           ## store matrix inverse in the object
  m                         
}