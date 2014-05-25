## These functions calculate the inverse of a matrix, caching the result to 
## eliminate unnecessary recomputation of identical matrices

## Creates a special matrix - a list containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setSolve <- function(inverseMatrix) i <<- inverseMatrix
	getSolve <- function() i
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Calculates the inverse matrix created wsith the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getSolve()
        if(!is.null(i)) {
        	message("getting cahced data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        i
}
