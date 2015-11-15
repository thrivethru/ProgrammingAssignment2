## The functions makeCacheMatrix and cacheSolve together create a special
## matrix object and calculate the inverse matrix, setting it in the object's
## cache

## makeCacheMatrix is a function which is really a list of functions to
## 1. To set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix's inverse
## 4. Get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL
	set <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inverse_matrix <<- inverse
	getinverse <- function() inverse_matrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## cacheSolve takes a matrix object created using makeCacheMatrix and returns the 
## inverse of the matrix. If the inverse has already already been set it returns 
## that value, if not it computes the value and sets it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
    	message("getting cached data")
    	return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data)
    x$setinverse(inverse_matrix)
    inverse_matrix
}
