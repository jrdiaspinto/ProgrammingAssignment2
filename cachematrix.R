# R Programming course - 2nd assignment


## This function creates a matrix object that can cache its 
## inverse

makeCacheMatrix <- function(matrix = numeric()) {
        # Every time makeCachematrix is called the ´inverted´ 
        # matrix is set NULL
        inverted_matrix <- NULL
        
        # 1st function: For each new square matrix this function 
        # replace the older data
        set_data <- function(x) {
                matrix <<- x
                inverted <<- NULL
        }
        
        # 2nd function: It returns the values of the original 
        # matrix
        get_data <- function() { matrix }
        
        # 3rd function: It is called by CacheSOlve() during the 
        # first CacheSolve() access; it stores the inverted 
        # matrix by using  the symbol of <<- superassignment
        set_inverted <- function(solve) {
                inverted_matrix <<- solve
        }
        
        # 4th function: It will return the cached value to 
        # CacheSOlve() on later access
        get_inverted <- function() { inverted_matrix }
        
        # This is a list of internal functions to be called each 
        # time in CacheSolve()
        list(set_data = set_data, get_data = get_data,
             set_inverted = set_inverted,
             get_inverted = get_inverted)
}



## This function computes the inverse of the matrix defined in the
## function above

cacheSolve <- function(matrix, ...) {
        # Access the matrix data and gets the inverted matrix
        inverted_matrix <- matrix$get_inverted()
        
        # If the inverted matrix was already cached(not NULL) 
        # this part writes a message and returns the value of 
        # the inverted matrix; CacheSolve() ends
        if(!is.null(inverted_matrix)) {
                message("GETTING CACHED DATA")
                return(inverted_matrix)
        }
        
        # If matrix$get_inverted is NULL this part of the 
        # function is used
        data <- matrix$get_data()
        
        # If inverted_matrix was NULL this part computes 
        # the inverted matrix
        inverted_matrix <- solve(data, ...)
        
        # This part stores the inverted matrix of matrix
        # in makeCachematrix
        matrix$set_inverted(inverted_matrix)
        
        # Returning the inverted matrix
        return(inverted_matrix)        
}
