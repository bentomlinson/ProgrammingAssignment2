
## These two functions are used in conjunction with each other when the computation time 
## is sufficiently large that it is worthwhile to cache the result of a given
## computation to avoid repeating it.
## 
## The two functions are used to calculate the inverse of a square matrix
## It is assumed that the matrix is both square (n x n) and invertible
##
## 
## Example in how to use these functions:
## matrix1 <- rbind(c(1, -1/4), c(-1/4, 1))
## matrix2 <- makeCacheMatrix(matrix1)
## cacheSolve(matrix2)
## cacheSolve(matrix2)
##
## The second time cacheSolve is called, the function should recall the data from the cache
## rather than recalculate the inverse 



## This function creates a store for the function ("solve" in this case) and matrix
## so if both are recalled again, the repeated computation is avoided.

makeCacheMatrix <- function(x = matrix()) {
      
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## A list is created with the function and data to be compared 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## This function checks if the function and matrix have already been calculated in makeCacheMatrix
## It checks if the inverse has already been calculated
## If so, it then gets the value from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
        ## check if already cached data
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if not cached, then calculate inverse and cache result
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
