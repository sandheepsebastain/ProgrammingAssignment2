## 2 functions to get the inverse of a matrix
## cache the inverse if it doesnt exist already

## Function that creates a special matrix
##and cache's the inverse using the <<- operator

makeCacheMatrix <- function(x = matrix()) {
  matinverse <- NULL
  get <- function() x
  setinverse <- function(inverse) matinverse <<- inverse
  getinverse <- function() matinverse
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Function takes in a "special matrix"
##Checks if an inverse has already been calculated
##If not it sets the inverse of the matrix
##If yes, it prints a message and retrieves the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinverse <- x$getinverse()
  if(!is.null(matinverse)) {
    message("Inverse has already been cached")
    return(matinverse)
  }
  #Code to set inverse into a cache if it hasnt been already calculated
  matrix <- x$get()
  matinverse <- solve(matrix, ...)
  message("Setting Inverse for the first time")
  x$setinverse(matinverse)
  return(matinverse)
}
