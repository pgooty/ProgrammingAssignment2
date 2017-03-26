# Week3 - Assignment
##########################################################################################
#
# This function creates a special "matrix" object that can cache its inverse.
# Steps: 1) Set the matrix
#        2) Get the matrix
#        3) Set inverse
#        4) Get inverse
##########################################################################################
makeCacheMatrix <- function(x = matrix()){
	        m <- NULL
                set <- function(y){       # set the matrix
		       x <<- y            # <<- used to assign a value to an object in
	                                  # an environment different from current environment
	               m <<- NULL
	        }
	        get <- function() x       # get the matrix
	        setInverse = function(inverse) m <<- inverse  # set inverse
		getInverse = function() m                     # get inverse
                list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


#############################################################################################
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.
#############################################################################################
cacheSolve <- function(x, ...) {
	        # Return a matrix which is inverse of matrix 'x'
	        m <- x$getInverse()
                if(!is.null(m)) {
		        message("getting cached data")
	                return(m)   
		}
	        mat <- x$get()
		m <- solve(data, ...)
		x$setInverse(m)
		m
}
