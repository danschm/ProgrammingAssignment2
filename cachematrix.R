## The tow functions below used to create a special object that stores 
## a matrix and cache's its inverse.

## makeCacheMatrix creates a matrix object with properties:
## 1. set the values of a matrix
## 2. get the values of matirx
## 3. set inverse matrix inv 
## 4. get the values of the inverse matrix
## returns a list object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## CacheSolve calculates the inverse for a makeCacheMatrix object
## 1. get inverse from makeCacheMatrix object
## 2. check if inverse exists, yes return inverse matrix from cache
## 3. No, build inverse with matrix values from makeCacheMatrix object
## 4. Set makeCacheMatrix object inverse matrix and return it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata, ...)
  x$setinverse(inv)
  inv
}
