## Put comments here that give an overall description of what your
## functions do

## This object returns a list of 4 functions used for nxn matrix inversion.
## When called makeCacheMatrix caches the matrix x in the parent environment
## and provides access to access to the 4 functions below.
makeCacheMatrix <- function(x = matrix()) {
        MInv <- NULL
        set <- function(u = matrix()) {
                x <<- u
                MInv <<- NULL
        }
        get <- function() x
        setMatrixI <- function(inver) MInv <<- inver
        getMatrixI <- function() MInv
        list(set = set, get = get,
             setMatrixI = setMatrixI,
             getMatrixI = getMatrixI)

}
 




## This object returns a matrix that is the inverse of cached the matrix x
## It also caches inverse matrix in the parent environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MInv <- x$getMatrixI()
        if(!is.null(MInv)) {
                message("getting cached data")
                return(MInv)
        }
        data <- x$get()
        MInv <- solve(data, ...)
        x$setMatrixI(MInv)
        MInv
}

## TESTING EXAMPLES
#> ma
#    [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> mma<-makeCacheMatrix(ma)
#> cacheSolve(mma)
 #    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> mb
#    [,1] [,2]
#[1,]    5    7
#[2,]    6    8
# mmb<-makeCacheMatrix(mb)
# cacheSolve(mmb)
#    [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
#> mc
#     [,1] [,2]
#[1,]    9   11
#[2,]   10   12
#> mmc<-makeCacheMatrix(mc)
#> cacheSolve(mmc)
#     [,1] [,2]
#[1,]   -6  5.5
#[2,]    5 -4.5
#> md
#     [,1] [,2] [,3]
#[1,]    0    1    1
#[2,]    1    0    0
#[3,]    1    1    0
#> mmd<-makeCacheMatrix(md)
#> cacheSolve(mmd)
#     [,1] [,2] [,3]
#[1,]    0    1    0
#[2,]    0   -1    1
#[3,]    1    1   -1
#> 
## The caching is demonstrated as follows:
#> mmd$get()
#     [,1] [,2] [,3]
#[1,]    0    1    1
#[2,]    1    0    0
#[3,]    1    1    0
#
#> mmd$getMatrixI()
#     [,1] [,2] [,3]
#[1,]    0    1    0
#[2,]    0   -1    1
#[3,]    1    1   -1
#> 
