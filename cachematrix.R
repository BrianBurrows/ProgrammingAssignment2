## makeCacheMatrix creates a special matrix object for storing the matrix inverse
## cacheSolve will solve for the inverse once, and then simply return the inverse
# if that inverse has already been computed

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
	inv <- NULL 
        set <- function(matSet){
                mat <<- matSet
                inv <<- NULL 
        }
        get <- function() mat 
        setinv <- function(invGiven) inv <<- invGiven
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inv <- mat$getinv() 
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- mat$get()
        inv <- solve(data,...)  # Compute the matrix inverse (though it is a slow technique)
        mat$setinv(inv)
        inv
}
