## makeCacheMatrix function is intended to create a cache of an inverted matrix. CacheSolve() inverts the matrix if no cache data is retrieved or simply prints the inverted matrix from cache. 

## makeCacheMatrix creates a list that contains four functions: set, get, setInv and getInv. <<- is the assignment operator which ensures that the variables within the function are not exposed to the outside environment. makeCacheMatrix() creates the cache of an object

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL #the result of the inversion is stored here
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setInv <- function (inv) xinv <<- inv
  getInv <- function() xinv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## CacheSolve() inverts the matrix if no cache data is retrieved or simply prints the inverted matrix from cache

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}

