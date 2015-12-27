## Creates a list containing 4 fuctions that allow access to the matrix and the
## inverted version (if it is available) help within the function. 
## Based on supplied makeVector and cahchemean functions.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ##print(x)
    ##print(class(x))
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(invmatrix) m <<- invmatrix
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Uses list continaing functions that access matrices held by makeCacheMatrix 
## to create inverse matrix assuming this has not already been created and has 
## not changed in which case it uses the cached version

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}