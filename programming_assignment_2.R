#This function makeCacheMatrix creates a special "matrix",
# which is actually a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverted matrix
#4. get the value of the inverted matrix
makeCacheMatrix <- function (x=matrix()){
              Inv <- NULL #initialize the inverse value as NULL
              
              #set the value of the matrix
              set <- function(y){
                      x <<- y #enter a new x
                      Inv <<- NULL #since a new x has been entered, reset the Inv
              }
              get <- function()x #get the value of the matrix
              setInv <- function(Inverse) Inv <<- Inverse #set the inverse
              getInv <- function() Inv #get the inverse
              
              #return a list containing all four functions above
              list(set = set, get = get,
                   setInv = setInv,
                   getInv = getInv)
        
}


#The following function calculates the inverse of the specail "matrix"
#created with the above function. However, it first checks to make sure
#whether the inverse has already been calculated. If so, it retrieves 
#the inverse from the cache and skips the computation. Otherwise, it
#calculates the inverse of the new matrix and sets the value of the inverse
#in the cache via the setInv function
cacheSolve <- function(x,...){
        Inv <- x$getInv() #get the inverse
        
        #check whether the inverse of x is already cached. if so,
        #return message and return the inverse
        if(!is.null(Inv)){
                message("getting cached matrix")
                return(Inv)
        }
        tmp <- x$get() #if not cached, we convey the matrix to "tmp"
        Inv <- solve(tmp,...) # compute the inverse of "tmp"
        x$setInv(Inv) #cache the calculated inverse
        Inv #return the inverse matrix
        
}



