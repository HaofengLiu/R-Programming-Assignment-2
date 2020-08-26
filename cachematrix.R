## Put comments here that give an overall description of what your
## functions do

## Some functions to cache the inverse of a matrix

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( x = matrix() ) {

    inverse_matrix <- NULL                      ## Initialize the inverse matrix

    set <- function( matrix ) {                 ## Create a new function to set the matrix
            x <<- matrix
            inverse_matrix <<- NULL
    }

    get <- function() {                         ## Create a new function to get the matrix
            x                                   ## Return the matrix
    }

    set_inverse <- function( inverse ) {        ## Create a new function to set the inverse of the matrix
            inverse_matrix <<- inverse
    }

    get_inverse <- function() {                 ## Create a new function to get the inverse of the matrix
            inverse_matrix                      ## Return the inverse matrix
    }

    list(set = set, get = get,                  ## Return a list of the methods
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function( x, ... ) {

    final_matrix <- x$get_inverse()             ## Return a matrix that is the inverse of 'x'

    if( !is.null( final_matrix ) ) {            ## Just return the inverse if its already set
                message( "getting cached data" )
                return(final_matrix)
    }

    data <- x$get()                             ## Get the matrix from our object

    final_matrix <- solve(data) %*% data        ## Calculate the inverse using matrix multiplication

    x$set_inverse( final_matrix )               ## Set the inverse to the object

    final_matrix                                ## Return the matrix
}
