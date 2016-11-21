## This set of function allows you to 
## create an object to store a matrix "makeCacheMatrix"
## compute and return the inversion matrix with cacheSolve
## the inversion matrix will be stored in your "makeCacheMatrix" with the initial matrix
## if you call the cacheSolve again on the same object, the result will be
## collected from the cache with no redundant calculation.
## if you modify the matrix in the object, the inversion matrix will be
## removed at the same time.

## this function creates a list of 4 elements that allows you to
## put a matrix in the object with set_matrix
## get the matrix from the object with get_matrix
## put the inversion matrix in the object with set_inv
## get the inversion matrix from the object with get_inv
## all these objects are stored in the cache and accessible from other functions.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL                          # initialize inversion matrix to be non calculated
  set_matrix <- function(y) {
    x <<- y                                   # replace the existing matrix with the new one, in the cache
    inv_matrix <<- NULL                       # remove the old inversion matrix if it was previously computed
  }
  get_matrix <- function() x                  # output the stored matrix
  set_inv <- function(inv) inv_matrix <<- inv # store the inversion matrix in the object
  get_inv <- function() inv_matrix            # output the stored inversion matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inv = set_inv,
       get_inv = get_inv)
}



## cacheSolve is designed to be applied to an object created with makeCacheMatrix
## if the inversion matrix of the input object hasn't been calculated already,
## the function will compute it, store it in the cache and return it as the output
## of the function. 
## If the inversion matrix already exists in the object, it will just return it 
## without further calculations.

cacheSolve <- function(x, ...) {
  matrix_inv <- x$get_inv()          # get the inversion matrix from the object, if it exists
  if(!is.null(matrix_inv)) {
    message("getting cached data")   # return the existing inversion matrix without calculation
    return(matrix_inv)               # return the inversion matrix and exit the function
  }
  data <- x$get_matrix()             # get the matrix if the inversion matrix was empty
  matrix_inv <- solve(data, ...)     # compute the inversion matrix
  x$set_inv(matrix_inv)              # put the inversion matrix in the cache in the object x
  matrix_inv                         # return the inversion matrix
}
