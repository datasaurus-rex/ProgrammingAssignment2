##Justin Wagner
##Programming Assignment 2



makeCacheMatrix <- function(a = matrix()) {
  ##cache value - initialize to NULL
      cache <- NULL
      
      ##
      set <- function(b) {
        a <<- b
        cache <<- NULL
      }
      ##Retrieve matrix value
      get <- function() a
      ##This is where the function will invert the matrix while storing the cache
      setinverse <- function(inverse) cache <<- inverse
      ##Retrieves inverted matrix from cache
      getinverse <- function() cache
      
      ##create list
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
  cache <- a$getinverse()
      ##if the inverted matrix exists then return else create matrix
      if(!is.null(cache)) {
        message("getting cached data.")
        ##return the matrix inverse cache
        return(cache)
      }
      data <- a$get()
      cache <- solve(data)
      a$setinverse(cache)
}
