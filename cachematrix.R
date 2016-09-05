## Jonathan Friedlieb
## Coursera R Programming course September 2016

## This function takes a matrix as an input and outputs a list with the functions:
## $set,$get, $setSolve.  The set and get functions operate as descrbed in the HW
## documentation*.  $setSolve is analgous to $setMean in the HW documentation but
## calculates the matrix inverse instead of a vector mean.
## The keys to the function are:
## creating and saving the inverse matrix in the makeVector environment 
## Setting m to NULL to determine if a new matrix has been evaluated
## creating the functions that make the cached values available for evaluation by the cacheSolve function
## generating a memory pointer ginving the location of the cached data

## *https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md



makeCacheMatrix <-function(x=matrix()) { ## Set variable type as 'matrix'
   
        ## Initialize x (data) and m and set m to NULL
             
        m <- NULL                   
        set <- function(y) {    
                x <<- y
                m <<- NULL
        }
        
        ## Create internal functions to invert matrix and export solution
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)     
        
}


## CacheSolve use list object created by makeCacheMatrix as its argument. It
## uses the functions in the list to access the cached data in the makeCacheMatrix
## environment and an IF statement see if the matrix values have changes.  If there's no change it
## prints the inverse matrix and 'getting cached data'.  If the matrix is new it
## calculates and prints the new inverse matrix.


cacheSolve <- function(x, ...) {
        m <- x$getsolve()  ## Uses function created in makeCacheMatrix to get inverted matrix (if it exists)
        
        if(!is.null(m)) {  ## Evaluates m to see if matrix has changed
                message("getting cached data")
                return(m)   ## Matrix is unchanged, prints message and saved matrix
        }
        data <- x$get()  ## Matrix is changed.  Solves matrix, sets value, and prints
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
