## -----------------------------------------------------------------------------------
## Data Science | R Programming Week 3
## Assignmnet 2
## JLC
## -----------------------------------------------------------------------------------

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions cache and compute the inverse of a matrix.

## -----------------------------------------------------------------------------------





## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get value of matrix
  get <- function() x
  
  ## set inverse value of matris
  setinverse <- function(inverse) inv <<- inverse
  
  ## get inverse value of matrix
  getinverse <- function() inv
  list(
        set=set
        , get=get
        , setinverse=setinverse
        , getinverse=getinverse
      )
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache. For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
	
  inv <- x$getinverse()
  
  ## if inverse already computed, use that value and alert user that cached data is used
  if(!is.null(inv)) {    
    message("getting cached data.")
    return(inv)
  }
  
  ## if not already computed, calucate inverse, set value in cache and output value
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
} 


## TEST RUN USING EXAMPLE FROM BOARDS
## > exampleMatrix <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## > matrixVector = makeCacheMatrix(exampleMatrix)
## > matrixVector$get()
## 		[,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0	1    4
## [3,]    5    6    0
## > cacheSolve(matrixVector)
## 		[,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > cacheSolve(matrixVector)
## getting cached data.
## 		[,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > 
