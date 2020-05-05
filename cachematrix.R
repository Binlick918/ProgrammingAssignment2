## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x  ## returns the value of x
    setinverse <- function(inv) m<<-inv  ##cache the value of inverse of x into m
    getinverse <- function() m  ##return the cached value of m (which is the inverse of x)
    
    ## expose the four "methods" of x to the outside world 
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    ## First check if there is a cached value, if yes return that value w/o calculation
    m <-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ## Since no cached data is found, calculate m and cache it
    data<-x$get()
    m <-solve(data)
    x$setinverse(m)
    m
}
