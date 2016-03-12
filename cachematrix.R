## The solution can be done from the example by substituting the vector
## for the matrix and the mean function for the inverse function

# TO RUN AN EXAMPLE PLEASE TYPE FOR INSTANCE THE FOLLOWING LINES:
#         
#         my_matrix<- matrix(1:4, nrow=2, ncol =2)
#         my_CacheMatrix<-makeCacheMatrix(my_matrix)
#         cacheSolve(my_CacheMatrix)
#         cacheSolve(my_CacheMatrix)

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        # 1.-set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # 2.-get the values of the matrix
        get <- function() x
        
        # 3.-set the value of the inverse
        setinverse <- function(solve)  i<<- solve
        
        # 4.-get the value of the inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
## Assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
