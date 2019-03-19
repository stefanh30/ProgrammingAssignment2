## Put comments here that give an overall description of what your
## functions do

## create a list of 4 functions to store matrix, get matrix, store inverse, get inverse
makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL # inverted matrix
      set <- function(y) { #stores new matrix
            x <<- y #x set x globally to be the source matrix
            inv <<- NULL #set inv to NULL (because new matrix means we have no inversed matrix yet)
      }
      get <- function() x #get returns source matrix
      setinv <- function(solve) inv <<- solve # take the inversed matrix and assigns it to inv
      getinv <- function() inv #return inv to the mother function
      list(set = set, get = get, #the function output will be a list
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv <- x$getinv() #get inv from the created list
      if(!is.null(inv)) { #checks if inv is null
            message("getting cached data") #if inv is not null...
            return(inv) #...return inv, done!
      }
      data <- x$get() #if inv is null, get data via get() function, which only forwards x (original matrix)
      inv <- solve(data, ...) #inverse the matrix
      x$setinv(inv) #send inv into the storage vector
      inv #return inversed matrix
}
