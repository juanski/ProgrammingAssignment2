## Put comments here that give an overall description of what your
## functions do


## The following functions cache and compute the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv

        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache. For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()

        if(!is.null(inv)) {

        	## if the inverse is already cashed, then return the cached value
                message("getting cached data.") 
                return(inv)
        }

        ## if inverse is not cached the get inverse
        data <- x$get()
        inv <- solve(data)
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
