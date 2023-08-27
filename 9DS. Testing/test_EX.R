convert <- function(f, target = 'c') {
  if (target == 'c') {
    return ((f - 32) / 1.8)
  } else if (target == 'k') {
    celsius <- (f - 32) / 1.8
    return (celsius + 273.15)
  } else {
    stop("error")
  }
}

print(convert(50, 'c'))

print(convert(-500, 'k'))
print(convert(0, 'k'))
print(convert(1000, 'k'))

