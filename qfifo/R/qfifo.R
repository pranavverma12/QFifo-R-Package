#' The constructor for fifo1
#' @return An S3 object of class fifoq
#' @export
#' @examples
#' q <- qfifo()

qfifo <- function() {
  structure(list(data=list()), class ="qfifo")
}


#' Add a value to the queue
#'
#' @param q is the current fifo queue object
#' @param v is the value to be added to the queue
#'
#' @return The updated queue object
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)

add <- function(q, v){
  UseMethod("add")
}

#' @export
add.qfifo <- function(q1,v){
  q1$data[length(q1$data)+1] <- v
  q1
}

#' Return the top value in the queue
#'
#' @param q is the current fifo queue object
#'
#' @return The top of the queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' v <- top(q)

top <- function(q){
  UseMethod("top")
}

#' @export
top.qfifo <- function(q1){
  print(q1$data[[1]])
}

#' Delete the top element from the queue
#'
#' @param q is the current fifo queue object
#'
#' @return The modified queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' q <- add(q,5678)
#' q <- process(q)

process <- function(q){
  UseMethod("process")
}

#' @export
process.qfifo <- function(q1){
  if(length(q1$data) == 0)
    stop("No elements on the queue to process")
  q1$data[1] <- NULL
  q1
}
