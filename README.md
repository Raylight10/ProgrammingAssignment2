### Assignment: Caching the Inverse of a Matrix

"The operater '<<-' opertor is used  a value to an object in an environment
that is different from the current environment. Below are two functions 
that are used to create a special object that stores a numeric matrix and caches its inverse.
The inverse of matrix can be calculated 'solve' function in R package.

## 'makeCacheMatrix' function creates a special object that stores a numeric matrix,
which is really a list containing a function: 
1.  set: set the value of the matrix
2.  get: get the value of the matrix
3.  setInverse: set the value of the inverse of matrix 
4.  getInverse: get the value of the inverse of matrix. 
<!-- -->"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve   
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse= getInverse)
}

##  'CacheSolve' function do the following jobs:
" - calculates the inverse of the special 'matrix' created with the above function. 
  - checks to see first if the inverse of matrix is already calculated. 
  - if the inverse of matrix is already calculated, it gets inverse of matrix from 
    cache and skips the computation.
  -if not, calculates the inverse of matrix of the data and sets the value of the inverse of matrix in 
   the cache  via the `setInverse` function."

cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
