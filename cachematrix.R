## The makeCacheMatrix holds the invertible matrix as an input for the next function.
## cacheSolve computes the input and returns the inverse of the input matrix,
## If the value/output is already present in cache, it will then return a message
## "Getting cached data" along with the result.

## makeCacheMatrix holds the invertible matrix

makeCacheMatrix <- function(x = matrix()) {

  # initialize the inverse matrix to NULL during the first call to makeCacheMatrix  
  inv<-NULL
  
  # funciton to set a new value for the underlying martix
  set<-function(y) {
    
    x<<-y
    inv<<-NULL
    
  }
  
  # get function for underlying matrix
  get<-function() x
  
  # set the Inverse of the matrix x.  Called by cacheSolve
  setinv<-function(inv) inv <<-inv
  
  # returns the inverse of matrix x
  
  getinv<-function() inv
  
  # return value of the makeCacheMatrix function is a list
  
  list(set=set,get=get, setinv=setinv,getinv=getinv)
  
}


## cacheSolve function will take the cached output from makeCacheMatrix function

cacheSolve <- function(x, ...) {
  # get the inverse of the matrix defined inside x.
  
  inv <- x$getinv()
  
  # If already computed inverse of martix stored via setinverse(),
  # and have not invalidated the cache by calling set(), 
  # return the cached version of inverse of martix x 

  
  if(!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
    
  }
  
  # either we havent computed the cached version yet, or we've called
  # set() previously and invalidated the cache.
  
  # call get() to get the underlying matrix
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv) 
  
  #return the cached inverse of matrix x
  inv
  
}
