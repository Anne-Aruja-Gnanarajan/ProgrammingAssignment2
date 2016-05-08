##Anne-Aruja-Gnanarajan

#makeCacheMatrix: This function creates a special "matrix" object that 
#can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #INPUT: an invertible matrix x
  #OUTPUT: a list containing functions explained below
  
  inv <- NULL
  
  #Function 1. to set the matrix
  set <- function(y) {
    x <<- y #assigning value to an object in a different environment
    inv <<- NULL
  }
  
  #Function 2. to get the matrix
  get <- function() x
  
  #Function 3. to set the inverse
  setInverse <- function(calculatedInverse) inv<<-calculatedInverse
  
  #Function 4. to get the inverse
  getInverse <- function() inv
  
  #Returning list of functions
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}



#cacheSolve: This function computes the inverse of the special "matrix" returned
#by the makeCacheMatrix above. If the inverse has already been calculated, 
#then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  #INPUT: x which is the output of makeCacheMatrix ie. list of functions
  #OUTPUT: returns the inverse of the orignal matrix which was input into makeCacheMatrix
  
  inv <- x$getInverse()
  
  #If the inverse has previously been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    
    #otherwise it calculates the inverse 
  } else {
    matrixData <- x$get()
    inv <- solve(matrixData,...)
    
    #setting the inverse in the cache
    x$setInverse(inv)
    return(inv)
  }
}
