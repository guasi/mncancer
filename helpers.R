# shift i to top, i can be index or string
shift_to_top <- function(x,i) {
  if (is.character(i)) { 
    i <- which(grepl(i,x))  
  } else {
    i <- as.numeric(i)
  }
  
  if (length(i) > 0) {
    return(c(x[i], x[-i]))
  } else {
    return(x)
  }
}

# layout for plotly
my_layout <- function(p, title) {
  layout(p = p,
         title   = list(text = title,
                        y = 1,
                        yanchor = "top",
                        pad = list(t = 15)),
         xaxis   = list(title = ""),
         yaxis   = list(title = "Rate per 100,000 persons"),
         margin  = list(t = 70, r = 50),
         modebar = list(orientation = "v"),
         font    = list(size = 10))
}