## The function makeCacheMatrix set a special matrix which includes the following functions :
#get, set, getinverse, setinverse
## The function cacheSolve computes the determinant and the inverse of an invertible matrix et cache the resulsts.

## The function makeCacheMatrix creates a special matrix (here variable "x") object that can cache its inverse.
## $get will return the matrix itsefl
## $set will the matrix with new values and will set the inverse to NULL
## $setinvere will store the inverse of the matrix
## $getinverse will return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix <- NULL
      
      ## $set, set the matrix to its new value and set the inverse to NULL
      set <- function(y) {
            x <<- y
            inverseMatrix <<- NULL
      }
      
      ## $get function return the matrix
      get <- function() x
      
      ## $set, set the inverse of the matrix
      setinverse <- function(inverse) inverseMatrix <<- inverse
      
      ##$get return the inverse of the matrix
      getinverse <- function() inverseMatrix
      
      ##creat the list of functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


##The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should return the inverse from the cache and print "getting cached data"

##Before calcaulating the inverse the function calculates the determinant of the matrix in order to check in the matrix is invertible, 
##if it is not the function returns prints "Matrix not invertible" and set the inverse as NaN

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## $get the inverse
      m <- x$getinverse()
      
      ## if "m" is not null, then return the cached data and print a message
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## if "m" is null, get the matrix
      data <- x$get()
      
      ##Comput the determinant, if it is 0 then matrix not invertible
      
      if(det(data) == 0) {
            
            ##Print the fact that matrix is not invertible
            message("Matrix not invertible")
            
            ## set the inverse of the matrix as NaN
            m <- NaN
            x$setinverse(m)
      }
      else {
            ##if determinant different of 0, then compute the inverse matrix
            m <- solve(data)
            
            ## cache the inverse
            x$setinverse(m)
            
            ##return the inverse matrix
            m
      }
}

