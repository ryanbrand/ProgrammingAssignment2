## makeCacheMatrix either generates a new matrix or 
## takes one as an argument and adds the ability
## to get and set the inverse of the matrix.  
## cacheSolve takes a cacheMatrix as an argument and either
## returns the cached inverse if it exists or computes the inverse
## otherwise.

## Generates cache matrix either from scratch or from
## an existing matrix passed as an argument.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Either returns the cached inverse of the matrix or
## computes the inverse and assigns it to the original.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
		i <- solve(data)
		x$setinverse(i)
		i        
}
