#Let's make and label our matrix
A = matrix(0,11,10)
dimnames(A) = list( c("business", "computer", "economy", "growth", "operating", "recession", "recovery", 
                      "release", "software", "system", "virus"), c("c1", "c2", "c3", "c4", "c5", "m6", 
                      "m7", "m8", "m9", "m10"))
A[,1] = c(0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0)
A[,2] = c(0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0)
A[,3] = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1)
A[,4] = c(0, 0, 0, 0, 1, 0, 0, 1, 0, 2, 0)
A[,5] = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1)
A[,6] = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
A[,7] = c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0)
A[,8] = c(0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0)
A[,9] = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0)
A[,10] = c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)

print("Basic Word/Document Matrix:")
print(A)

B = apply(A,2, function (x) x/sqrt(sum(x^2)))
print("Normalized so each document vector magnitude == 1")
print(B)

print("Cosine similarities between documents:")
print(t(B) %*% B)

SVD = svd(B)
print("From the singular value decomposition:")
print("Left Singular Vectors:")
print(SVD$u)
print("Right Singular Vectors:")
print(SVD$v)
print("Singular Values:")
print(SVD$d)

#plot left singular vectors
plot(c(1:11),SVD$u[,1], type="l", ylim=c(-.8,.6), main = "First Two Left Singular Vectors, 
     \nDimension vs. Value", xlab="Dimension(k)", ylab=expression("Value(U"[k]~")"))
lines(SVD$u[,2], lty=2)
legend(4,.73, c("1st Left Singular Vector","2nd Left Singular Vector"), 
       cex=.7, lty=c(1,2), bty="n")

#plot right singular vectors
plot(c(1:10),SVD$v[,1], type="l", ylim=c(-.6,.6), main = "First Two Right Singular Vectors, 
     \nDimension vs. Value", xlab="Dimension(k)", ylab=expression("Value(V"[k]~")"))
lines(SVD$v[,2], lty=2)
legend(6.5,.65, c("1st Right Singular Vector","2nd Right Singular Vector"), 
       cex=.7, lty=c(1,2), bty="n")

docxcoord = apply(B,2, function(x) sum(SVD$u[,1] * x))
docycoord = apply(B,2, function(x) sum(SVD$u[,2] * x))
plot(docxcoord,docycoord, col="white", main="Document Vectors Projected onto First 2 Left Singular Vectors", 
     xlab="Projection onto First Left Singular Vector", ylab="Projection onto Second Left Singular Vector")
text(docxcoord,docycoord,labels=colnames(B))

wordxcoord = apply(B,1, function(x) sum(SVD$v[,1] * x))
wordycoord = apply(B,1, function(x) sum(SVD$v[,2] * x))
plot(wordxcoord,wordycoord, xlim=c(-1.3,.02), col="white", main="Word Vectors Projected onto First 2 Right 
     Singular Vectors", xlab="Projection onto First Right Singular Vector", ylab="Projection onto 
     Second Right Singular Vector")
text(wordxcoord,wordycoord,labels=rownames(B))