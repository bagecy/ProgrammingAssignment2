## Put comments here that give an overall description of what your
## functions computes the inverse of the special "matrix" in a cache environment


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
     x <<- y                        #y is an input value of set, this makes input x has 2 environment, 2 values
     m <<- NULL
  }
  get <- function() x               # x has become the value in cache environment, after last function, this return the cached value
  setinv <- function(inv) m <<- inv #like set, mean is cache, m is NULL here, but might have value in cache
  getinv <- function() m            # get the value of m in cache environment
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  m <- x$getinv()                   # get the value of m in cache environment
  if(!is.null(m)) {                 #check if mean in the cache has value, if yes, return the value
     message("getting cached data") # if no, go to next command
     return(m)
  }
  data <- x$get()                   # give the cached value to data
  m <- solve(data)                  # calculate mean of the cached value and give it to m
  x$setinv(m)                       # saved the value of m to the cache environment
  m                                 ## Return a matrix that is the inverse of 'x'
}
