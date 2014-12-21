## The "makeCacheMatrix" function creates a special matrix object and caches its inverse.

## Before the "makeCacheMatrix", a square matrix (nrow == ncol) should be created. For example,
## x <- matrix(c(2,3,4,5), nrow = 2, ncol = 2)

makeCacheMatrix <- function(x = matrix()) { ## states that x is a matrix
	m <- NULL ## m will be the inverse matrix and it's reset to NULL when makeCacheMatrix is called
	set <- function(y){ ## takes an input matrix
		x <<- y ## saves the input matrix
		m <<- NULL ## resets the inverse matrix to zero
	}
	get <- function() x ## returns the value of the original matrix
	setinv <- function(solve) m <<- solve ## "cacheSolve" calls this during the first "cacheSolve"
	getinv <- function() m ## "cacheSolve" retrieves the already cached value after the first time
	list(set = set, get = get, ## Each time a new matrix is made by makeVector() function, this is accessed
	setinv = setinv,
	getinv = getinv)
}


## The "cacheSolve" function computes the inverse of the special matrix returned by the 
## "makeCacheMatrix". If the matrix stays the same and its inverse has already been calculated,
##"cacheSolve" will retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        m <- x$getinv() ## the matrix x is accessed and the inverse of it is calculated
        if(!is.null(m)){ ## If the inverse matrix was already calculated (is not NULL, then ... )
        	message("getting cached data") ## ... this message is sent to the console
        	return(m) ## ... and the inverse matrix is shown
        }
        data <- x$get() ## If the x$getinv() returns NULL (the inverse is not calculated yet), this code is accessed
        m <- solve(data, ...) ## The inverse matrix is calculated with solve() function
        x$setinv(m) ## The calculated inverse matrix is stored in x 
        m ## The inverse matrix is sent to the console 
}
