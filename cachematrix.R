## Routines to cache a matrix object and its inverse to speed access
## to the inverse after the first use.


## Stores a matrix and its inverse (does not calculate the inverse)
## Takes a matrix as input, and copies it to a local variable
## Returns a list containing 4 functions to get and retrieve
## the original and inverse matrix.  As long as the list exists,
## the memory holding the stored version is maintained and can be accessed.


makeCacheMatrix <- function(OriginalMatrix = matrix()) {
  
  ## Store original matrix in local variable
  ## Since matrix is new, we don't have the inverse so
  ## set inverse to NULL
  CachedInverseMatrix <- NULL  
  CachedOriginalMatrix <- OriginalMatrix
  
  ## Generate 4 functions to store and retrieve original and 
  ## inverse matrices
  StoreMatrix <- function(OriginalMatrix){
    CachedInverseMatrix <<- NULL  
    CachedOriginalMatrix <<- OriginalMatrix
    }
  ReadMatrix <- function() {
    print(CachedOriginalMatrix)
    CachedOriginalMatrix
  }
  StoreInverse <- function(InverseMatrix) CachedInverseMatrix <<- InverseMatrix
  ReadInverse <- function() CachedInverseMatrix
  
  ## return a list containing accessor functions
  functionList <- list(StoreMatrix = StoreMatrix, 
       ReadMatrix = ReadMatrix,
       StoreInverse = StoreInverse,
       ReadInverse = ReadInverse)
   }


## Calculate the inverse of a matrix, and store it.  If the inverse is already
## stored from a previous calculation, return the stored copy instead of 
## recalculating.
## Takes a list of functions created by makeCacheMatrix routine

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    ## Verify the input is a list, if not throw  warning message
    if (!is.list(x)) { 
      warning(paste("The object passed into cacheSolve must be a list returned",
              "by makeCacheMatrix function.  Exiting function."),immediate=TRUE)
    }
  
    ## Pull up the stored inverse matrix
    InverseMatrix <- x$ReadInverse()
    
    ## if the stored inverse matrix was null, then an inverse hasn't been 
    ## calculated yet.  Calculate the new inverse.
    if (is.null(InverseMatrix)){
      InverseMatrix <- solve(x$ReadMatrix(), ...)
      x$StoreInverse(InverseMatrix)
    }
    
    ## Call up the inverse matrix so it is returned
    InverseMatrix

}
