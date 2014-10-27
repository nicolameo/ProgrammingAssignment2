## FUNCTIONS makeCacheMatrix AND cacheSolve ARE SET TO:
## - CREATE A SPECIAL "MATRIX" OBJECT.
## - CACHE THE INVERSE OF THE SPECIAL "MATRIX".
## THE OBJECTIVE IS TO BENEFIT FROM CACHING A MATRIX INVERSION WHICH IS USUALLY A COSTLY COMPUTATION, RATHER THAN COMPUTING IT REPEATEDLY.

## FUNCTION makeCacheMatrix
## IT CREATES A SPECIAL "MATRIX" OBJECT THAT CAN CACHE ITS INVERSE AND PERFORMS AS FOLLOWS:
## SETTING THE MATRIX
## GETTING THE MATRIX
## SETTING THE INVERSE OF THE MATRIX
## GETTING THE INVERSE OF THE MATRIX

makeCacheMatrix <- function( mat = matrix() )
{
    inv <- NULL

    ## SETTING THE MATRIX
    set <- function( matvalue )
    {
        mat <<- matvalue
        inv <<- NULL
    }

    ## GETTING THE MATRIX
    get <- function() { return(mat) }

    ## SETTING THE INVERSE OF THE MATRIX
    setinv <- function(inverse) { inv <<- inverse }

    ## GETTING THE INVERSE OF THE MATRIX
    getinv <- function() { return(inv) }

    ## RETURNING THE INVERSE OF THE MATRIX
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## FUNCTION cacheSolve
## IT COMPUTES THE INVERSE OF THE SPECIAL "MATRIX" OBJECT CREATED WITH THE ABOVE FUNCTION makeCacheMatrix.
## THE FUNCTION CHECKS IF THE INVERSE OF THE MATRIX HAS ALREADY BEEN CALCULATED (AND THE MATRIX HAS NOT CHANGED).
## IF SO, IT RETRIEVES THE INVERSE OF THE MATRIX FROM THE CACHE AND SKIPS THE COMPUTATION.
## OTHERWISE IT CALCULATES THE INVERSE OF THE DATA AND SETS ITS VALUE IN THE CACHE VIA THE FUNCTION setinv.

cacheSolve <- function(x, ...)
{
    ## GETTING THE INVERSE OF THE MATRIX RETURNED BY makeCacheMatrix
    matinv <- x$getinv()

    ## CACHING THE INVERSE OF THE MATRIX IF ALREADY CALCULATED
    if( !is.null(matinv) )
    {
        message("getting cached data")
        return(matinv)
    }

    data <- x$get()

    ## COMPUTING THE INVERSE OF A SQUARE MATRIX
    mativ <- solve(data) %*% data

    ## SETTING THE INVERSE OF THE MATRIX
    x$setinv(matinv)

    return(matinv)
}