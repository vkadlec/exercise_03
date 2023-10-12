# IndexOfMin

IndexOfMin <- function(array, first, last){
  index <- first
  for (k in (first):last){
    if (array[k] < array[index]){
      index <- k
    }
  }
  return(index)
}

IndexOfMin(c(7,92,87,1,4,3,2,6),1,8)


# SelectionSort

SelectionSort <- function(a, n){
  for (i in 1:(n-1)){
    j <- IndexOfMin(a, i, n)
    b <- a[i]
    a[i] <- a[j]
    a[j] <- b
  }
  return(a)
}

SelectionSort(c(7,92,87,1,4,3,2,6),8)


# RecursiveSelectionSort

RecursiveSelectionSort<- function(a, first, last){
  if (first < last){
    index <- IndexOfMin(a, first, last)
    b <- a[first]
    a[first] <- a[index]
    a[index] <- b
    a <- RecursiveSelectionSort(a, first+1, last)
  }
  return(a)
}

RecursiveSelectionSort(c(7,92,87,1,4,3,2,6),1,8)
