## 
## This function creates a special "matrix" object that 
## can cache its inverse. In this way, we can speed up the
## computing time


##The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
##    set the value of the original matrix
##    get the value of the original matrix
##    set the value of the inverse of the original matrix
##    get the value of the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {

	  matrix_inv <- NULL
	
	  ##only the function makeCacheMatrix can modify the list 
	  ##returned containing the matrix and the inverse computed.
	  ##$set() is the only method that can modify the matrix.	  
        set <- function(y) {
                x <<- y
                matrix_inv <<- NULL
        }

        ##get the original matri
	  get <- function() x
		
	  ##set or calculate the inverse of the matrix
        set_matrix_inv <- function(solve) matrix_inv <<- solve

	  ##get the inverse of the matrix
        get_matrix_inv <- function() matrix_inv


        list(set = set, get = get,
             set_matrix_inv = set_matrix_inv,
             get_matrix_inv = get_matrix_inv)

}


## 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  ##check if the matrix inverse has been calculated,
	  ##if yes, get the cached matrix inverse and return
	  matrix_inv <- x$get_matrix_inv()
        if(!is.null(matrix_inv)) {
                message("getting cached data")
                return(matrix_inv)
        }

	  ##if the matrix inverse cached data is not found, 
	  ##calculate the matrix inverse now and return
        data <- x$get()
        matrix_inv <- solve(data, ...)
        x$set_matrix_inv(matrix_inv)
        matrix_inv
}
