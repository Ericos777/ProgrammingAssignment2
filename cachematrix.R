##this function contains the get and set functions for the matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ##internal inverse variable
  inverse <- NULL
  
  #set function for the matrix
  set <- function(ma)
  {
    x <<- ma
    inverse <<- NULL ##when setting a new matrix delete computed inverse 
  }
  
  ##get function for the matrix
  get <- function(){
    x
  }
  
  ##set function to store computed inverse
  setinv <- function(inv)
  {
      inverse <<- inv
  }
  
  ##get function to retrieve computed inverse
  getinv <- function() {
    inverse
  }
  
  ##store functions in list to make them available to use
  list(set = set 
       ,get = get
       ,setinv = setinv
       ,getinv = getinv)  
}


##this function will return a inverted matrix of x
##this inverted matrix will be pulled from the cache if possible
##or will calculate the inverse of the matrix and store it in the cache.
cacheSolve <- function(x, ...) {
           
    ##try to get inversed object  
    i <- x$getinv()
    
    ##check if inversed object has a value
    if(!is.null(i)){
      return(i)
    }
    
    ##if not get matrix
    ma <- x$get() 
    
    ##calculate inversed matrix
    i <- solve(ma, ...)
    
    ##cache inversed matrix
    x$setinv(i)
    
    ##return inversed matrix
    i
      
  } 

  
