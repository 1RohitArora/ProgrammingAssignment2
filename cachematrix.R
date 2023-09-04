# Create a special object for caching a matrix and its inverse
# This function initializes an object that stores a matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
# Initialize a list to store the matrix and its cached inverse
	cache <- NULL
    
# Setter function to set the matrix in the cache
	set <- function(y) {
		x <<- y  # Use the <<- operator to assign 'y' to 'x' in the parent environment
		cache <<- NULL  # Clear the cached inverse since the matrix has changed
    	}
   
# Getter function to retrieve the matrix
	get <- function() x
    
# Function to set the cached inverse
	setinverse <- function(inverse) {
		cache <<- inverse  # Cache the inverse matrix
    	}
    
# Function to retrieve the cached inverse if available
		getinverse <- function() cache
    
# Return a list of functions for manipulating the cache object
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	}

# Calculate and cache the inverse of a matrix
# This function takes a cached matrix object and computes its inverse if not already cached.
# If the inverse is already cached, it returns the cached value.

	cacheSolve <- function(x, ...) {
# Retrieve the cached inverse if available
		inverse <- x$getinverse()
    
# If the cached inverse is NULL, calculate it and cache it
		if (!is.null(inverse)) {
		message("Getting cached data")
		return(inverse)
	}
    
# If the cached inverse is not available, calculate it and cache it
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
