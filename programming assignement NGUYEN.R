#Creating an inverse function
inverse<-function(x) {
  x^-1
}

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL              #Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x    #get the value of the matrix
  setinverse <- function(inverse) m <<- inverse #Set the inversse of the matrix
  getinverse <- function() m                    #get the inversse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated , then the cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()                
  if(!is.null(m)) {                #checking the cache and returning m if present in the cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)           #else, computing inverse matrix
  x$setinverse(m)
  m
}



