## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function accepts a matrix and default the member variables
## 1- inv to NULL
## 2 - x to either an empty matrix if it is 
##     not supplied else initialize x to the 
##     supplied matrix
## There are 4 member functions also
## get() - return the cached matrix
## set(Y) - initialize x with y
## set(inverse) - set matrix inverse inv to the 
##                supplied value "inverse"
## get() - get the cached matrix inverse inv
## These 4 member functions are set in a list,
## and the makeCacheMatrix function returns this
## list object.
##
## In order to accesss the member function ,the
## following syntax should be used
## <list object returned by makeCacheMatrix function>$member function name
## e.g  mx <- makeCacheMatrix(input matrix)
## to get the cached matrix , the syntax is
##      mx$get()


makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the member variable inv
  inv <- NULL
  
  ## set the supplied matrix as cached matrix - x
  ## and initialize inv to null
  set <- function(y) {
    if(is.null(x)){
      x <<- y
      inv <<- NULL
    }
    ## check if the matrix is already set , 
    ## then check if the previous matrix and
    ## current matrix are not the same
    ## then reset the mattrix and set the 
    ## inverse to NULL
    else if(!is.null(x) & !identical(x,y)){
      x <<- y
      inv <<- NULL
    }
  }
  
  ## get the cached matrix
  get <- function() x
  
  ## set the member variable inv 
  setinv <- function(inverse) {
    
    ## check if the matrix inverse is correct
    ## by doing a matrix multiplication
    ## between the matrix and its inverse
    ## then compare the result with 
    ## an identity matrix
    idtymtx <- round (x %*% inverse ,3)
    if(identical(idtymtx,diag(nrow(x)))){
      inv <<- inverse 
    }else{
      message("matrix inverse being set is incorrect")
      inv <<- NULL
    }
    
  }
  
  ##get the cached matrix inverse
  getinv <- function() inv
  
  
  ##list object which holds the 4 member variables
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  

}


## Write a short comment describing this function
## This function will return a ched matrix inverse
## if it already calculated , else calculate the matrix 
## inverse.Cache the matrix inverse and return the inverse

cacheSolve <- function(lst, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##This step will check if the cached matrix
  ##is square invertible i.e the determinant 
  ##of the matrix is non zero , else return 
  ##null matrix inverse
  if(det(lst$get()) == 0 ){
    message("Given matrix is not a square invertible matrix , so matrix inverse can't be found ")
    return(NULL)
  }
  ## Return a matrix that is the inverse of 'x'
  
  ## get the member variable inv
  inv <- lst$getinv()
  
  ## If the matrix inverse is not null , then 
  ## return the cached matrix invrse
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  
  ## get the cached matrix
  data <- lst$get()
  
  ## calculate the matrix inverse
  inv <- solve(data, ...)
  
  ## cache the matrix inverse
  lst$setinv(inv)
  
  ## return the calculated matrix inverse
  inv
  
}
