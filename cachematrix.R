## The two functions are used to get the inverse of a matrix.
## Since calculations can be time-consuming, we remove the redundancy of
##  calculating the inverse by caching the result.

## makeCacheMatrix returns a list of functions to set/get the given matrix
## and to set/get the inverse of the given matrix
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    
    get<-function(){
        x
    }
    
    setinv<-function(mm){
        inv<<-mm
        inv
    }
    
    getinv<-function() {
        inv
    }
    
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve checks if matrix has been inversed, otherwise calculates the
## inverse and store the result in the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    m_inv<-x$getinv()
    if(!is.null(m_inv)){
        message("Getting cached data")
        return(m_inv)
    }else{
        message("Not cached, calculating inverse")
        data<-x$get()
        m_inv<-solve(data)
        x$setinv(m_inv)
        m_inv
    }
    
}


##The following lines can be uncommented to test the two functions
# testmatrix<-matrix(1:4,2,2)
# testCacheMatrix<-makeCacheMatrix(testmatrix)
# result<-cacheSolve(testCacheMatrix)
# print(result)
# result<-cacheSolve(testCacheMatrix)
# print(result)
