## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix is a construction matrix than prepares, throug a list object, the elements that will be 
## needed by the cacheSolve function to either retrieve the previously calculated inverse matrix 
## or to calculate a new inverse
##
## Write a short comment describing this function
## makeCacheMatrix builds the elements needed to solve the problem possed and stores them in a list
## of four elements that should be passed as argument to the cacheSolve function
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve function either restores the unchanged inverse of the original matrix if no changes
## have occured, or calculate the new inverse otherwise and displays the inverse to the console
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
