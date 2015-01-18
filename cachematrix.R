## Function makeCacheMatrix takes a square matrix as input; creates a list of
## functions to add and retrieve matrix and its invserse to and from cache
##
## Function takes input from above function cacheSolve() checks for inverse 
## in cache; returns it if the value exists in cache
#############################################################################

## Function makeCacheMatrix takes a square matrix as input. 
## If input is not square matrix, it throws an error.
## The function returns a list of functions to
## cache input matrix 
## get input matrix
## cache inverse of input matrix
## get inverse of input matrix from cache

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    if (nrow(x) == ncol(x))
    {
        set <- function(mat) 
        {
            x <<- mat
            invmat <<- NULL
        }
        
        get <- function() x   
        setinv <- function(inverse) invmat <<- inverse
        getinv <- function() invmat
        
        ## create a list of functions defined above
        list(get=get, set=set, setinv=setinv, getinv=getinv)   
    }
    else
    {
        message("Need a square Matrix. # of components must be equal to dim^2")
    }
}


## cacheSolve function recieves the list from above function as input
## If the cache contains inverse, the function returns it. If not,
## it calculates the inverse, sets it in the cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## get inverse from cache
    invmat <- x$getinv()
    
    ## check if inverse exists in cache, if not calculate and set it.
    if(!is.null(invmat))
    {
        message("Fetching Inverse from the Cache...")
        return(invmat)
    }    
    else
    {
        mat <- x$get()
        invmat <- solve(mat)
        x$setinv(invmat)
        return(invmat)
    }
}
