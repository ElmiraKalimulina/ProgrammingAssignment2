## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # xInverseMatrix stores result (inverse matrix) NULL is default value
    xInverseMatrix <- NULL
    
    # The function 'set' set a matrix to object created by makeCacheMatrix function
    set <- function(y){
        x <<- y
        xInverseMatrix <<- NULL 
    }
    
    # get the value of the initial matrix
    get <- function() x 
    
    #set the value of the inverse matrix
    setInverse <- function(inverse) xInverseMatrix <<- inverse 
    
    # return inverse matrix
    getInverse <- function() xInverseMatrix 
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
