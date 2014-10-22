## calculates the inverse of a matrix and caches its values 
## so you dont need to calculate it again

# creates a matrix and caches its value
# function take one argument,x the matrix we wish to invert
makeCacheMatrix <- function(x = matrix()) {
    # declare variables
    i <- NULL
    
    # nested function that super-assigns argument, y to x and NULL to i
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # nested function, get that returns x
    get <- function() x
    
    # uses the solve function to inverse matrices, when called i is super-assigned as the solve value
    setinverse <- function(solve) i <<- solve
    
    # returns i, the inverse of matrix x
    getinverse <- function() i
    
    # list of embedded functions to use
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# creates inverse of matrix unless the value is already chached 
# in which case it will take the cached value
cacheSolve <- function(x, ...) {
    
    # assign i to retrieve inverse value of matrix, x
    i <- x$getinverse()
    
    # condition that returns i when cached inverse is available
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # assign variable data to get the marix values
    data <- x$get()
    
    # use the function solve on matrix to create inverse and assign to i
    i <- solve(data, ...)
    
    # matrix i will be inverted and set 
    x$setinverse(i)
    
    # returns i to the console
    i
}
