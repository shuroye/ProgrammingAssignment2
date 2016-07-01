## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
##takes in a "matrix" object that can cache its inverse.

##Student: Huroye Scott
## Date: 7/1/2016

##How to test this code:
##    compile code: source("makeCacheMatrix")
##    initialize makeCacheMatrix: test <- makeCacheMatrix()
##    set the value of the makeCacheMatrix: test$set(rbind(c(1,2),c(3,4)))
##    get the value of the makeCacheMatrix: test$get()
##    set the value of the cacheSolve:test$setInv(test$get())
##    get the value of the cacheSolve:test$getInv()
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	                x <<- y
	                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
	getInv <- function() inv
	list(set = set, get = get,
	             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
	        if(!is.null(inv)) {
	                message("getting inverse cached data")
	                return(inv)
	        }
	        data <- x$get()
	        inv <- solve(data, ...)
	        x$setInv(inv)
        inv
}
