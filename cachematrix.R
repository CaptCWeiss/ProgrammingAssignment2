makeCacheMatrix <- function(x = matrix()) {
    ## creates a list containing functions to
    ## 1. set the values of the matrix
    ## 2. get the values of the matrix
    ## 3. set the inverse of the matrix
    ## 4. get the inverse of the matrix 
    
    minv <- NULL ## initialize matrix inverse values with NULL                     

    ## defining the functions:
    set <- function(y) { ## sets the raw values                       
        x <<- y 
        minv <<- NULL ## resetting 'minv' in case the values were changed             
    }
    get <- function() x ## gets the the raw values                           
    setminv <- function(solve) minv <<- solve ## computes the inverse values 
    getminv <- function() minv ## gets the inverse values       

    ## returns the named functions as a list:
    list(set = set, get = get,                
         setminv = setminv,
         getminv = getminv)
}


cacheSolve <- function(x, ...) {
    ## Returns the inverse of matrix 'x'
    ## Assumes to run 'makeCacheMatrix' before calling this function.
    
    minv <- x$getminv() ## invoke the getminv function to see if there is cached data
    
    if(!is.null(minv)) { ## return the cached inverse if it exists                 
        message("getting cached inverse")
        return(minv)
    }
    
    #return the computed inverse if the cached doesn't exist:
    data <- x$get() ## getting the raw data                               
    minv <- solve(data, ...) ## inverting it and assigns it to 'minv'
    x$setminv(minv) ## setting the new computed minv in case it will be asked again
    minv ## returning the computed minv
}
