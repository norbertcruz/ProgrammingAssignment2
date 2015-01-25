
## The functions included work in pair to calculate and store the inverse of a
## square matrix.  If the inverse of that particular matrix was already
## calculated and stored, the functions will not solve the matrix, but return
## the cached value instead.



## makeCacheMatrix() receives a matrix and returns a list that contains 
## functions to store and return information about the matrix given.

makeCacheMatrix <- function(A = matrix()) {

    inv <- NULL  
    
    set <- function(B) {  ## changes the stored matrix
        
        if (!identical(A, B)){  ## verifies if matrix was changed
            
            A <<- B ## caches the new matrix
        
            inv <<- NULL  ## resets cached inverse to NULL
            
        }
        
    }
    
    get <- function() A  ## returns the stored matrix
    
    setinverse <- function(inverse) {## changes the values of the stored inverse
        
        inv <<- inverse  ## this inverse is cached in the environment created
                        ## when makeCacheMatrix() is called
    }
    
    getinverse <- function() inv  ## returns the stored inverse
    
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse) ## returns a list of functions that provide
                                  ## access to the contents of the environment
}


## cacheSolve() receives a list created by makeCacheMatrix() to calculate 
## the inverse of the matrix if it has not been previously calculated and 
## stored in the list.  If the matrix has not changed and the inverse has 
## been calculated, the cached inverse will be retrieved.

cacheSolve <- function(x = list(), ...) { 
            
    inv <- x$getinverse()  ## obtains the stored inverse
            
    if(!is.null(inv)) {  ## verifies if the inverse is cached
                
        message("getting cached solution")
            
        return(inv)  ## returns cached inverse
                
    }  
            
    ## Runs if no cached inverse is found
    
    matrix <- x$get()
    
    inv <- solve(matrix, ...)  ## calculates the inverse
                
    x$setinverse(inv) ## caches the inverse
                
    inv  ## returns the inverse
        
}
