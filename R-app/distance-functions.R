#calculates distance between two points on Earth using the Great Circle Distance formula
distancecalc <- function(lo1, la1, lo2, la2) {
  lo1 <- d2r(lo1)
  lo2 <- d2r(lo2)
  la1 <- d2r(la1)
  la2 <- d2r(la2)
  return(3958.756 * acos(sin(la1) * sin(la2) + cos(la1) * cos(la2) * cos(lo1 - lo2)))
}

#converts degrees to radians
d2r <- function(num) {
  num <- num / 180 * pi
  return(num)
}
