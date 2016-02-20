##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ##Clean up (if explicit needed).
    cleanup <- function() {
        ##Checkbar wit is.null()
        mtr <<- mtrinverse <<- mtr_ctime <<- mtrinverse_ctime <<- NULL
    }
   
    ##The single functions are created analog to the example in the assignment, 
    ##so that they can be called as attributes of the object, given back by 
    ##"constructor" makeCacheMatrix()
    
     setmatrix <- function(y = matrix()) { 
         
         print("Setting source matrix in the cache.")
         ##Setting up a new matrix, we don't care if it is probably the same one; 
         ##everything will be overriden
         
        ##This is a matrix passed to the function, made persistent for further checks
        mtr <<- y
       
        ##This is the default (empty) reverse matrix (by now placeholder, 
        ##not a matrix really), made persistent. The empty matrix() is reversible, 
        ##so this way You can distinguish a never set mtrinverse from one set to matrix()
        mtrinverse <<- NULL
        
        ##Timestamp needed to control, whether the matrix was changed since last
        ##generation of inverse
        tstmp <- as.POSIXct(Sys.time())
        mtr_ctime <<- tstmp
     }
     
     
     getmatrix <- function() {
         print("Getting source matrix from the cache.")
         if (is.null(mtr)) {
             print("ERROR: The original matrix is not set!")
         }
         else {
            mtr
         }
     }
     
     setinverse <- function(x) {
         
         mtrinverse <<- solve(x)
         print("Setting inverse matrix in the cache.")
         tstmp <- as.POSIXct(Sys.time())
         ##The timestamp of inverse should normally match the time state 
         ##of the original matrix.
         mtrinverse_ctime <<- mtr_ctime
     }
     
     getinverse <- function() {
         print("Getting inverse matrix from the cache.")
         return(mtrinverse)
     }
     
     inversevalid <- function() {
         ##TRUE if mtrinverse is not NA and it is a matrix and ist actual
         return(!is.null(mtrinverse) && is.matrix(mtrinverse) && mtrinverse_ctime == mtr_ctime )
     }
     
    
     
     ##Default initialization
     setmatrix()
     
     list(cleanup = cleanup, setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse, inversevalid = inversevalid )
    
    ##Nothing more as far as I've understood the assignment...
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. 
##- If the inverse has already been calculated 
##- and the matrix has not changed, 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,  ...) {
        
    ##If the inverse has already been calculatedand the matrix has not changed...
    
    if(x$inversevalid()) {
            print("Cached inverse matrix valid")
            ##...then the cachesolve should retrieve the nverse from the cache.
            return(x$getinverse()) 
    }
    ##...inverse must be calculated now
    else {
        print("Cached inverse matrix NOT valid")
        x$setinverse(x$getmatrix())
        return(x$getinverse())
    }
}    

##Here is the evidence that the repeated execution of cacheSolve(km)
##returns the cached inverse matrix
#
# > source("Assignmetn_P-Programming_Week3/ProgrammingAssignment2/cachematrix.R")
# > km$cleanup()
# > km <- makeCacheMatrix()
# [1] "Setting source matrix in the cache."
# > cacheSolve(km)
# [1] "Cached inverse matrix NOT valid"
# [1] "Getting source matrix from the cache."
# [1] "Setting inverse matrix in the cache."
# [1] "Getting inverse matrix from the cache."
# [,1]
# [1,]   NA
# > cacheSolve(km)
# [1] "Cached inverse matrix valid"
# [1] "Getting inverse matrix from the cache."
# [,1]
# [1,]   NA
# > km$setmatrix(matrix(c(2,1,5,3),2,2))
# [1] "Setting source matrix in the cache."
# > cacheSolve(km)
# [1] "Cached inverse matrix NOT valid"
# [1] "Getting source matrix from the cache."
# [1] "Setting inverse matrix in the cache."
# [1] "Getting inverse matrix from the cache."
# [,1] [,2]
# [1,]    3   -5
# [2,]   -1    2
# > cacheSolve(km)
# [1] "Cached inverse matrix valid"
# [1] "Getting inverse matrix from the cache."
# [,1] [,2]
# [1,]    3   -5
# [2,]   -1    2
