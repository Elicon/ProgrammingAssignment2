## Programming Assignment 2  - R Programming 2016
## Assignment to Cache inverse matrix 

## create a matrix for the cache
	# Following the format of the assignment 
	# Creating a makeCacheMatrix object with
	# four functions in a list
	# 1. set the matrix
	# 2. get the matrix
	# 3. set the inverse of the matrix
	# 4. get the inverse of the matrix
	
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Second Function to compute the inverse and cache the result
 ## Return a matrix that is the inverse of 'x'
    # Following the same format as the assignment example

    # Get the current state of the inverse and see if it
    # has been computed yet
cacheSolve <- function(x, ...) {
   
    inv <- x$getinverse()

    # Cached value exists test
    if(!is.null(inv)) {
    	# Use a double negative test to simply return the computed inverse		
        message("Informational: Getting cached matrix")
        return(inv)
    }

    # If it hasn't got the matrix already

    data <- x$get()

    # Find the inverse and set to inv
    inv <- solve(data, ...)

    # Cache this result in the object
    x$setinverse(inv)

    # Return initial result
    inv    
}
