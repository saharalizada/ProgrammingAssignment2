## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <<- NULL
    set <- function(y){
        x <<- y 
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    } #check if inverse matrix is alreay cached
    data <- x$get()
    if (is.null(data) || !is.matrix(data) || !is.numeric(data) || det(data) == 0) {
        stop("The matrix is not invertible")
    } #check if matric is invertible
    i <- solve(data, ...) #compute the inverse matrix
    x$setinverse(i) #cache the inverse matrix
    i 
}
