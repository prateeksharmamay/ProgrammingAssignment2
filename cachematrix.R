## This is a version made by Prateek Sharma for Coursera's R Programming Course - Programming Assignment 2

## The following functions compute the inverse of a matrix and saves it
## to the cache so that the next time the user tries to calculate the inverse of
## matrix, the previously saved value is returned instead of doing the calculations again.

## The function "makeCacheMatrix" creates a special "matrix" object, which is really a list 
## containing a function to
## -> set the value of the matrix
## -> get the value of the matrix
## -> set the value of the inverse of matrix
## -> get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {     ## create a matrix object x and other related Functions
        
        m <- NULL     ## define the cache m
        set <- function(y) {
                x <<- y         ## assign the input matrix y to the variable x in the parent environment
                m <<- NULL      ## re-initialize cache m in the parent environment with NULL
        }
        get <- function() x     ## return matrix x
        setMatrixInverse <- function(inverseMatrix) m <<- inverseMatrix         ## set the cache m equal to the inverse of the matrix x
        getMatrixInverse <- function() m        ## return the cached inverse of matrix x
        
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse
        )       ## "makeCacheMatrix" Function returns list of set, get, setMatrixInverse and getMatrixInverse Functions.
}


## The function "cacheSolve" calculates the inverse of the special "matrix" created
## with the above function. But it first checks if the inverse
## has already been caclulated. If Yes, then it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache through the 'setMatrixInverse' function.

cacheSolve <- function(x, ...) {     ## Return inverse of matrix 'x' 
        
        
        m <- x$getMatrixInverse()       ## retrieving cached Inverse of Matrix x
        if(!is.null(m)) {       ## if Inverse of Matrix x has already been computed, it is Returned
                message("Getting Cached Inverse of Matrix")
                return(m)
        }
        CalcMatrixInverse <- x$get()    ## otherwise, Matrix x is retrieved using "get()"
        m <- solve(CalcMatrixInverse, ...)      ## Inverse of Matrix x is computed using solve()
        x$setMatrixInverse(m)   ## cache is assigned with the Inverse of Matrix x
        m       ## "cacheSolve" Function returns inverse of matrix x
}