
has_forcings <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_forcings))

  length(x@forcings) > 0
}

has_exposure <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_exposure))

  nrow(x@exposure@series) > 0
}

# Checks if a scenario has no or constant forcing functions & exposure
#
# This is an important property of a scenario, because in case of constant
# forcings we only need to simulate a single moving window instead of all of
# them. This is due to the fact that results for all windows will be identical.
has_constant_forcings <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_constant_forcings))

  nrow(x@exposure@series)<2 &
    ( length(x@forcings)==0 |
        all(sapply(names(x@forcings), function(nm) nrow(x@forcings[[nm]])<2)) )
}

has_controls <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_controls))

  length(x@control)>0
}

has_windows <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_windows))

  x@window.length>0
}

has_transfer <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_transfer))

  if(!is(x, "Transferable"))
    return(FALSE)
  if(has_regular_transfer(x))
    return(TRUE)
  has_irregular_transfer(x)
}

has_regular_transfer <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_regular_transfer))

  if(!is(x, "Transferable"))
    return(FALSE)
  x@transfer.interval > 0
}

has_irregular_transfer <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_irregular_transfer))

  if(!is(x, "Transferable"))
    return(FALSE)
  length(x@transfer.times) > 0
}

has_units <- function(x) {
  if(length(x) > 1)
    return(sapply(x, has_units))
  is(x, 'units')
}
