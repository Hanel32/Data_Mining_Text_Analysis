
# Professor example for analysis of efficiency methods in scripting languges. 

n  = 1000000
v  = runif(n)
l1 = as.list(v)
l2 = list(v)

start = Sys.time();
for(i in 1:n){
  tmp = v[i];
}
end   = Sys.time();

print(sprintf("Size of vector is %f bytes", object.size(v)))
print(sprintf("vector runs in %f seconds", end-start))

start = Sys.time();
for(i in 1:n){
  tmp = l1[[i]]
}
end   = Sys.time();

print(sprintf("Size of list 1 is %f bytes", object.size(l1)))
print(sprintf("list 1 runs in %f seconds", end-start))

start = Sys.time();
for(i in 1:n){
  tmp = l2[[i]]
}
end   = Sys.time();

print(sprintf("Size of list 2 is %f bytes", object.size(l2)))
print(sprintf("list 2 runs in %f seconds", end-start))
