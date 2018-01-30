# Matrix analysis

x = 123
m = matrix(nrow = 200, ncol = 200)

start  = Sys.time();
for(row in 1:nrow(m)){
  for(col in 1:ncol(m)){
    m[row, col] = x
  }
}
end    = Sys.time();
m_time = end - start 

print(sprintf("Size of matrix is %f bytes", object.size(m)))
print(sprintf("Matrix assignment runs in %f seconds", end-start))

d = as.data.frame(m)

start  = Sys.time();
for(row in 1:nrow(d)){
  for(col in 1:ncol(d)){
    d[row, col] = x
  }
}
end    = Sys.time();
d_time = end - start 

print(sprintf("Size of dataframe is %f bytes", object.size(d)))
print(sprintf("Dataframe assignment runs in %f seconds", end-start))

# Intuition:
#   Go for the matrix if you'll be randomly accessing data (stochastic)
#   Go for the dataframe if you're accessing an example (row) at a time.
#
#   Why? Dataframe accesses an entire row at a time, whereas the matrix
#   only accesses the requested memory area.
