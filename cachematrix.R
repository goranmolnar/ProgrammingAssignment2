## Functions for calculating inverse matrix with caching support. 
## Based on the special "cacheMatrix" objects that enable the 
## matrix and the corresponding inverse to be preserved. 
## If and inverse has been calculated, any time in the future, 
## the calculation does not need to be repeated, instead, it is 
## simply accessed from the cache, in that manner increasing the 
## script execution speed. The cached matrix calculation is based 
## on two functions: makeCacheMatrix (creates cacheMatrix objects) 
## and cacheSolve (calculates matrix inverse using cache to increase 
## performance). 
## 
## Example use: 
## mtr    =  rbind(c(4, 3), c(3, 7))  
## mtrs   =  makeCacheMatrix(mtr)7
## mtrInv =  cacheSolve(mtrs)


## Creates the special "cacheMatrix" objects that can be used 
## for caching matrices and their corresponding inverses. 
## Returning list has four function elements, get, set 
## (the initial matrix accessors) and getInv, setInv 
## (the inverse accessors), further documented at each definition below. 
makeCacheMatrix <- function(x = matrix()) { 
  ## matrix inverse 
  inv <- NULL 
  
  ## sets the original matrix 
  set <- function(y) {
    x <<- y 
    ## if the initial matrix value changed, 
    ## invalidate cached inverse. 
    inv <<- NULL
  } 
  
  ## gets the original matrix 
  get <- function() x
  
  ## sets the matrix inverse value 
  setinv <- function(inv) inv <<- inv 
  
  ## gets the matrix inverse value 
  getinv <- function() inv
  
  
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Calculates the inverse of the special "cacheMatrix" object. 
## cacheMatrix object need to be initialised using the function 
## makeCacheMatrix above. The function uses caching to alleviate 
## the need to run the resource intensive process of inverse calculation 
## if the inverse is known from before. 
## The matrix inverse is calculated using R function solve(). 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() 
  ## if the result is already available, return the cached version
  if(!is.null(inv)) { 
    message("Getting cached data")
    return(inv)
  } 
  ## if not, calculate the inverse and store it 
  data = x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
