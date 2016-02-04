## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function(){
        x
        }
    #setinv<-function(solve)inv<<-solve
    setinv<-function(mm){
        inv<<-mm
        inv
    }
    getinv<-function() {
        inv
        }
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

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


##The following lines can be uncommented to test the
##two functions
testmatrix<-matrix(1:4,2,2)
testCacheMatrix<-makeCacheMatrix(testmatrix)
result<-cacheSolve(testCacheMatrix)
print(result)
result<-cacheSolve(testCacheMatrix)
print(result)
