#Here we are going to make a function 
#that is going to give us the inverse of a matrix

#1. we are going to create a function to set and get the matrix

makeVector <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {m <<- inverse}
    getInverse <- function() {m}
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}

#2. the next function is going to calculate the inverse of the matrix 

cachesolve <- function(x, ...) {
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

