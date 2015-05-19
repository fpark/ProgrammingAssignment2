## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`
## If the inverse has already been calculated (and the matrix has not changed), then
## this function returns the inverse from the cache.
##
##
cacheSolve <- function(x, ...) {
## If a result already exist, return it from cache.
## If a result does not exist,  compute inverse and cache the result before returning the computed value.
##
## Args: special matrix objectcreated by makeCacheMatrix() below
## Returns: Inversed Matrix

    inversematrix <- x$getinverse()
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    data <- x$get()
    inversematrix <- solve(data, ...)
    x$setinverse(inversematrix)
    return(inversematrix)


}

makeCacheMatrix <- function() {
## Builds special matrix object using lexical scoping
## set function is used to pass matrix and validate it is an invertible matrix.
##
## Returns:
##   list of 4 functions - get, set, getinverse, setinverse


    inversematrix <- NULL
    set <- function(mat) {
        # Error Handling
        if (det(mat) == 0)
            stop("Argument is not an invertible matrix: det(x) is 0")
        m <<- mat
        inversematrix <<- NULL

    }
    get <- function() m
    setinverse <- function(invmat) inversematrix <<- invmat
    getinverse <- function() inversematrix
    list(get = get, set=set,
         setinverse = setinverse,
         getinverse = getinverse)
}



## this function is used to test computed inverse of square matrix result is saved on first call.
## subsequent calls cacheSolve for same matrix returns previously saved result instead of recomputed.
## if passed argument is not an invertible matrix, it should generate an error
test <- function() {


    # this matrix is not an invertible matrix and expect to generate an error.
    mat <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3)
    oMatrix <- makeCacheMatrix()
    oMatrix$set(mat)

    # this is an invertible matrix and expect to return inverted matrix
    mat <- matrix(c(10,20,30,4,5,6,7,8,8), nrow=3, ncol=3)
    oMatrix <- makeCacheMatrix()
    oMatrix$set(mat)

    ## first cacheSolve() call computes invertse matrix
    cacheSolve(oMatrix)

    ## subsequent cacheSolve(m) calls returns computed result from the first call/
    cacheSolve(oMatrix)
    cacheSolve(oMatrix)

}
