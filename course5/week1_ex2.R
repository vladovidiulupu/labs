# Here is the beginning of the file SRR1039508_1.fastq, which contains the first
# read of the pair for the experiment:
# 
# @SRR1039508.1 HWI-ST177:290:C0TECACXX:1:1101:1225:2130/1 
# CATTGCTGATACCAANNNNNNNNGCATTCCTCAAGGTCTTCCTCCTTCCCTTACGGAATTACA + 
# HJJJJJJJJJJJJJJ########00?GHIJJJJJJJIJJJJJJJJJJJJJJJJJHHHFFFFFD
#
# @SRR1039508.2 HWI-ST177:290:C0TECACXX:1:1101:1311:2131/1 
# CCCTGGACTGCTTCTTGAAAAGTGCCATCCAAACTCTATCTTTGGGGAGAGTATGATAGAGAT + 
# HJJJJJJJJJJJJJJJJJIIJIGHIJJJJJJJJJJJJJJJJJJJJJJGHHIDHIJJHHHHHHF
#
# @SRR1039508.3 HWI-ST177:290:C0TECACXX:1:1101:1414:2163/1 
# TCGATCCATCGATTGGAAGGCACTGATCTGGACTGTCAGGTTGGTGGTCTTATTTGCAAGTCC + 
# HJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJGJJIGHIJJBGIJCGIAHIJHHHHHHHFFFFF
# 
# Here we see three reads, with names, sequence, +, then quality score.
# 
# We will try to find the position of the third read using a simple BLAST search
# to the human genome (build hg19 / GRCh37).

# The third read aligns uniquely to chromosome 16 of the human genome, falling
# in the second exon of the gene DHX38, which is on the plus strand (transcribed
# from left to right). What is the genomic position to which the first basepair
# of the third read aligns? (Use human genome build: hg19 / GRCh37)
72130081

# The first three reads in the second file are as follows:
# 
# @SRR1039508.1 HWI-ST177:290:C0TECACXX:1:1101:1225:2130/2 
# CAGATGAGGCGTGTTGGCCAGAGAGCCATTGTCAACAGCAGAGATGNNNNNNNNNNNNAATCC + 
# HJJJJJJJJJJHIIIJJJJJJJJJJJJJJJJJJJJJJJHIJIJHII#################
#
# @SRR1039508.2 HWI-ST177:290:C0TECACXX:1:1101:1311:2131/2 
# TACTCCGGAGAACAGATGGGATTCCCTAGGAGACCCTTGAGGGAAAAGGGAGCCCCAATCTCT + 
# FJJJJJJJFHEHJJJHIIJJGGIIJJGIIJGJHJJJJJHGIJJIGIHHHHFFFDDDDDDDDDE
#
# @SRR1039508.3 HWI-ST177:290:C0TECACXX:1:1101:1414:2163/2 
# TCGCTCTCTCCGTTTCAGGGAAGCCAGCAAGTCCAGTCCGAGTAATGAAGGGCGGGGAGCAGG + 
# HJJJJJJJJJJJJJJJJJJJIJJJJJJJJJJIJJJJJJFHIJFHJJJJJIHHHFDDDDDDDDD
# 
# Align the third read above to the human genome, as we did previously.

# The third read in the second file also aligns uniquely to chromosome 16 of the
# human genome, aligning to the minus strand, also in the second exon of the
# gene DHX38. What is the genomic position to which the first basepair of the
# third read in the second file aligns?
72130242

# Above we saw the two reads of a paired-end fragment, both aligning within an
# exon of a human gene.
# 
# For a strand-specific RNA-sequencing protocol, we could have seen this kind of
# alignment to a + strand gene:
# 
# [1st read + strand] ... [2nd read - strand]
# 
# ...or we could have seen the 1st read aligning on the right side:
# 
# [2nd read + strand] ... [1st read - strand]
# 
# A strand-specific protocol means that we only observe fragments with the same
# strand as the gene.
# 
# However, many experiments, including the one we are examining are not
# strand-specific. This means that, for a plus strand gene, we will observe, in
# addition to the above two kinds of paired-end fragments, two more:
# 
# [1st read - strand] ... [2nd read + strand]
# 
# [2nd read - strand] ... [1st read + strand]