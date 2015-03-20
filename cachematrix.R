# Coursera Introduction to R Programming
# 
# Programming Assignment No. 2
# 
# Iraklis Konstantopoulos
# Sydney, 20 March 2015

makeCacheMatrix <- function(x = numeric()) {
    # Create a matrix object that can cache its inverse. 
    # 
    # args: 
    #   x [numeric] An invertible matrix
    
    # Create a null object that will become the cached variable. 
    m <- NULL
 
    # Re-initialise m and re-define x (for continuous writing). 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Now create a function to fetch the current value of x. 
    get <- function() x
    
    # And return a list of two functions: get and set.
    list(set = set, get = get)
}

cacheSolve <- function(x, ...) {
    # Compute the inverse of a makeCacheMatrix() object. 
    # 
    # args: 
    #   x [matrix] A matrix object from makeCacheMatrix()

    # Fetch the value of x, set it to m. 
    m <- x$get
    
    print(m)
    # As per the course notes: 
    #   If the inverse has already been calculated 
    #   (and the matrix has not changed), then it 
    #   should retrieve the inverse from the cache.

    # So, first check if m has a value. 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    # If there is no cached value, solve(x)
    else{
        m <<- solve(x$set)
    }

    # Return m. 
    m
}

testMe <- function() {
    # Test the validity of the above functions. 
    
    # Generate a 10x10 matrix of random numbers (invertible). 
    m <- matrix(rexp(100), ncol=20)
    
    # Create a matrix using makeCacheMatrix().
    myVec <- makeCacheMatrix(m)
    
    # Run the above vector through cacheSolve.
    cacheSolve(myVec)
}
