
# The following two functions are used to cache the inverse of a matrix.
# Basically, makeCacheMatrix creates a list which includes functions to:
# Set the value of the matrix
# Get the value of the matrix
# Set the value of inverse of the matrix
# Fet the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


# The cacheSolve function first verifies if the inverse of the matrix has already
# being calculated. If that holds true, then it returns the result and skips the
# calculation, avoiding repetitive processing. If not, it computes the inverse,
# using solve and caches the value via setmatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getmatrix()
                if(!is.null(m)) { 
                        message("Getting cached data")
                        return(m)
                }
                matrix<- x$get()
                m <- solve(matrix)
                x$setmatrix(m)
                return(m)
}

# In order to test both functions, one can run this test sample:
# Test Sample:
# x = rbind(c(-1, 2), c(2,-1))
# m = makeCacheMatrix(x)
# m$get()
#       [,1] [,2]
# [1,]   -1    2
# [2,]    2   -1
# Empty cache - first run
# cacheSolve(m)
#         [,1]      [,2]
# [1,] 0.3333333 0.6666667
# [2,] 0.6666667 0.3333333
# Cache available on the second run
# cacheSolve(m)
# Getting cached data
#        [,1]      [,2]
# [1,] 0.3333333 0.6666667
# [2,] 0.6666667 0.3333333
