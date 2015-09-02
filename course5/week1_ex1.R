# According to the UCSC Genes track (it will be on top if you have the default
# arrangement), how many transcripts does RIT1 have? Note that the RefSeq Genes
# track below is a separate annotation of the same gene.
3

# According to UCSC Genes track, how many exons does the top transcript of RIT1
# (uc031pqc.1) have?
6

# Look up a nearby gene, CCT3. How many transcripts does this gene have,
# according to the UCSC Genes track?
5

# If we sequence from these RNAs uniformly at a rate of 1 read per 1 basepair,
# how many total reads will we expect on average for this gene?

l = c(3000, 1000, 5000)
k = c(4, 7, 1)
sum(l * k)

# Now suppose, the proportion of each transcript changes, although the total
# number of copies of transcripts from the gene stays constant. Now we have: 
# k1 = 7 k2 = 4 k3 = 1 Now how many total reads will we expect on average from this
# gene?

k = c(7, 4, 1)
sum(l * k)

# For this assessment we will consider only a single gene for simplicity.
# 
# The l's give the lengths for the regions which contribute to the exon counts
# for 1, 2 and 3 and to the junction counts for {1,2} and for {2,3}. We will
# assume these counts are disjoint: so a read counts for an exon only if it does
# not cross a junction. This is a simplified model, which allows independence of
# the counts.
# 
# Suppose the following lengths:

lengths = c(100,200,300,100,100)

# The matrix which identifies transcripts with exons and junctions:
    
mat = cbind(c(1,1,0,1,0),c(1,1,1,1,1),c(0,1,1,0,1))

# The length of the transcripts is then:
lengths %*% mat 

# Suppose we align 1000 reads to this gene. So 
w = 1000

# Suppose we observe the following read counts for the exons and for the two junctions:
    
counts = c(125,350,300,125,100)

# Given the formula above, and the assumption of uniform read distribution and
# Poisson counts, we can get a rough estimate of theta by just solving the
# linear system above. The estimate is only a rough one, because we will not use
# the probabilistic model in the previous video. We will walk through the
# estimation step-by-step below.

theta.hat = c(1, 2, 3) / 10000
mat %*% theta.hat * lengths * w

LHS = counts/(lengths * w)
theta.hat = lm.fit(mat, LHS)$coefficients
theta.hat

# What is the estimate of theta using our rough estimator, for the first
# transcript (the transcript with exon 1 and exon 2)?
theta.hat[1]

mat %*% theta.hat * lengths * w

# What would be the rough estimate of theta for the first transcript if the
# counts for exons and junctions were 60,320,420,60,140?
counts = c(60,320,420,60,140)
LHS = counts/(lengths * w)
theta.hat = lm.fit(mat, LHS)$coefficients
theta.hat

# Note that if we only had one transcript for this gene, our estimate of theta
# would be simply:
# 
# total counts / (total length * w)
# 
# giving us a number which is 1/10^9 times the common RPKM or FPKM measure
# (reads or fragments per kilobase of sequence, per million mapped reads).