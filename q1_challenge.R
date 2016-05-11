
question1 <- function(N,T) {

options(digits=10);

stream = floor(runif(T, min = 1, max = 10));

M = vector(mode = "numeric", length = T);
L = vector(mode = "numeric", length = T);

for (i in 1:T)
{
max = tail(sort(stream[0:i]), N);
last = tail(stream[0:i], N);
M[i] = prod(max);
L[i] = prod(last);
}

seql = M-L;

print(paste0("Mean is: ", mean(seql)));
print(paste0("Standard Deviation is: ", sd(seql)));

}

