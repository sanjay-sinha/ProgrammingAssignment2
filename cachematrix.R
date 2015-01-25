## Matrix inversion is computationally intensive and there may be benefit 
## to cachining the inverse of a matrix rather than computing it repeatedly.
## Additinallly the two functons below demonstrate the use of the <<- operator
## which can be used to assign a value to an object in an environment that is 
## different from the current environment. Below are two functions that are 
## used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix() function below creates a special "vector", 
## which is a list containing a function to do the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of a given matrix.
## it checks if the inverse has previously been computed and 
## fetches the stored result if available. Otherwise, it calculates the inverse  
## of the matrix and sets the value of the inverse in the cache via
## the setinverse function before returning the computed inverse of matrix 
## to the calling program.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		message("computing matrix inverse")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
