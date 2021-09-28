##### Assignment number 2 for R Programming. This is an R
##### script for two functions. First function makes,
##### stores, and  inverse the matrix. The second
##### function takes the results from make_cash_matrix


#### Matrix function: The function creates a matrix and the inverse is cached
make_cache_matrix <- function(matrix_variables = matrix()) {

    inverse_matrix <- NULL

    set <- function(matrix) {
        matrix_variables <<- matrix
        inverse_matrix <<- NULL
    }
    set_inverse <- function(inverse) {
        inverse_matrix <<- inverse
    }

    get <- function() {
        matrix_variables
    }
    get_inverse <- function() {
        inverse_matrix
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##### Calculates in verse of the matrix created by make_cache_matrix.
cache_solve <- function(x, ...) {

    ### Return a matrix that is the inverse of 'x'
    matrix_inverse <- x$getInverse()

    if(!is.null(matrix_inverse)) {
        message("getting cached data")
        return(matrix_inverse)
    }

    cached_matrix <- x$get()

    matrix_inverse <- solve(cached_matrix) %*% cached_matrix

    x$setInverse(matrix_inverse)

    matrix_inverse
}