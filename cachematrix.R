## Assignment 2 on lexical scoping
## there are two functions that are asked to  be build
## one called makeCacheMatrix which will store a time consuming calculation 
## into the cache of the environment of the function - so that it needn't be recalculated
## and 
## one called cacheSolve that will reverse the calculation of the above function. 
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inversed matrix
## 4.  get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
				m <- NULL
				set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setmatrix <- function(solve) m <<- inverse
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}



## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

##end