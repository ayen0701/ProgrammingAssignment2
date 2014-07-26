##The following function creates a special "matrix" object
##that can cache its inverse
##It is assumed that the matrix supplied is always
#invertible

makeCacheMatrix <- function(x = matrix()) {
invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL 
        }
        get <- function()x
        setinverse <- function(inverse) invx <<- inverse
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse =getinverse)
}


##The next function, cacheSolve, computes the inverse of
##the special "matrix" returned by the fcn makeCacheMatrix
##above. If the inverse has already been calculated (and
##the matrix has not changed), then cacheSolve shd retrieve
#the inverse from the cache (storage)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinverse()
        if (!is.null(invx)) {
                message("getting cached data")
                return(invx)
        } else { #solves the inverse if it can't find in cache
                invx <- solve(x$get())
                x$setinverse(invx)
                return(invx)
        }
}
