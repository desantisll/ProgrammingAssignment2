# These functions allow you to store the inverse of a matrix
# so it can be used later without a computationally expensive reevaluation

##makeCacheMatrix create a special object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

# resets the old inverse value
  i <- NULL
  
# set changes the vector stored in the main function x globally via the <<- operator
# x now holds the args passed to set
# x <- y would only have changed x inside the set function 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
# get returns vector x stored in main function
  get <- function() x
  
# setinverse takes passed argument inverse and assigns it i globally in makeCacheMatrix 
  setinverse <- function(inverse) i <<- inverse
  
# getinverse returns i
  getinverse <- function() i
  
# stores our 4 functions in a list so they can be assigned to an object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix 
# If inverse has already been calculated (and the matrix has not changed)
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
##first it checks if the inverse has already been calculated (ie. m ! NULL)
i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
# if inverse is not cached, we use "get" the matrix stored with makeCacheMatrix storing it in data
  data <- x$get()
  
# then we find the inverse with solve() 
  i <- solve(data)
  
##then we pass i as an argument to setinverse, storing it in makeCacheMatrixs i variable
        x$setinverse(i)
        i
}
