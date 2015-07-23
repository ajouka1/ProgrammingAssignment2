## The following is a pair fo functions that cache and compute ##the inverse of a matrix
## functions do



## This function creates a special "matrix" object that can cache its inverse. 


makeCacheMatrix <- function(mx = matrix()) {
        inverse<- NULL
        set <- function (x){
                mx<<- x
                inverse<<- NULL
        }
        get <- function() return(mx)
        setinverse <- function(inverse) inverse <<- inv
        getinverse <- function() return(inverse)
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## this function computes the inverse of the special matrix, returned by the first function, if the inverse has already been calculated , then it return the value from the cache without further computation. 

cacheSolve <- function(mx, ...) {
        ## Return a matrix that is the inverse of 'x
        inverse <- mx$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- mx$get()
        inverse <- solve(data, ...)
        mx$setinverse(inverse)
        return(inverse)
}
