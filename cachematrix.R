##Makes the cahce matrix and is very similar to the makeVector function in the example provided

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix = NULL #Sets the inverse function to have no value, yet still creates it
   
    
     set <- function(y) { #one (sets value)
          inverseMatrix <<- NULL
          x <<- y
      
     } 
     
    get <- function() x #two (gets value)
    
    oneInv <- function(inverse) inverseMatrix <<- inverse
    twoInv <- function() inverseMatrix
    list(set = set, 
         get = get,
         oneInv = oneInv, 
         twoInv = twoInv)
}


## Computes the inverse and is similar to the cachemean function in the example except inverse instead of mean

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$twoInverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$oneInv(inverseMatrix)
  inverseMatrix
}
