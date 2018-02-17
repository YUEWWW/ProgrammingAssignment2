## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
#This function Function inputs a matrix, set the value of the matrix, get the value of the matrix, set the inverse matrix and get the inverse matrix.
#Assume that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inverse) inver <<- inverse 
        getinver <- function() inver
        list(set=set, get=get, setinver=setinver, getinver=getinver)
}



## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Return a matrix that is the inverse of 'x'
        
cacheSolve <- function(x, ...) {
        inver <- x$getinver()
        
        # if the inverse has already been calculated, get it from the cache
        if (!is.null(inver)){
                message("getting the cached")
                return(inver)
        }
        
        # If not, calculate the inverse 
        Ma <- x$get()
        inver <- solve(Ma, ...)
        x$setinver(inver)
        
        return(inver)
}
