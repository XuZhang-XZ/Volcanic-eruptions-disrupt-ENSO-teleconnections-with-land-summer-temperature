
new_year = function(x = year, y = month) {
  
  Sel = which(y == 12)
  x[Sel] = x[Sel] + 1
  x
  
}


