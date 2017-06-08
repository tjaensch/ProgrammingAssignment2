## Put comments here that give an overall description of what your
## functions do

## Rewrote given function to take matrix argument instead of numeric vector

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y) {
        x <<- y;
        m_inverse <<- NULL;
    }
    get <- function() return(x);
    setinverse <- function(inv) m_inverse <<- inv;
    getinverse <- function() return(m_inverse);
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## Rewrote given function to match variable names in makeCacheMatrix function above

cacheSolve <- function(x, ...) {
    m_inverse <- x$getinverse()
    if(!is.null(m_inverse)) {
        message("Getting cached data...")
        return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinverse(m_inverse)
    m_inverse
}
