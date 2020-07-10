## Assigment: Caching the inverse of a matrix
#1. MakeCacheMatrix:  this function gives an output of a special "matrix" object that can save (cache) its inverse.

makeCacheMatrix <- function(x = matrix()) {
inversa<- NULL
set<- function(y){
    x<<-y
    inversa<<-NULL
}
    get<-function() x
    setinversa<- function(solveMatrix) inversa<<- solveMatrix
    getinversa<- function() inversa
    list(set=set,get=get,setinversa=setinversa,getinversa=getinversa)
}

#2. This functions gives an output of the inverse of the special "matrix" returned by the last function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversa<- x$getinversa()
    if(!is.null(inversa)){
        message("getting cached data...")
        return(inversa)
    }
    data<- x$get()
    inversa<- solve(data)
    x$setinversa(inversa)
    inversa
}


a<- c(1,0)
b<- c(0,1)

c<-matrix(c(a,b),nrow=2,ncol=2)
c

d<-makeCacheMatrix(c)
cacheSolve(d)
