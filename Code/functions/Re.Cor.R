

## Function
Re.Cor = function(r,p) {
  Re = array(NA,length(r))
  Re[which(p > 0.05)] = "NS"
  Re[which(p <= 0.05 & r > 0)] = "P"
  Re[which(p <= 0.05 & r < 0)] = "N"
  Re
}
