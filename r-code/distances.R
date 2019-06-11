#DISTANCE

setwd('E:/DHSI2019_r-stylometry')

df = read.csv('table_with_frequencies.txt',sep=" ")

#MANHATTAN
df$absolute_freq_diff_manhattan = abs(df$ABronte_Agnes - df$Austen_Sense)
dist_AB_manhattan = sum(df$absolute_freq_diff_manhattan)

barplot(df$absolute_freq_diff_manhattan)

#EUCLIDEAN
df$absolute_freq_diff_euclidean = abs(df$ABronte_Agnes*df$ABronte_Agnes - df$Austen_Sense*df$Austen_Sense)
dist_AB_euclidean = sqrt(sum(df$absolute_freq_diff_euclidean))

barplot(df$absolute_freq_diff_euclidean)

#EDER's ANTI-ZIPF
df$absolute_freq_diff_antizipf = abs(sqrt(df$ABronte_Agnes) - sqrt(df$Austen_Sense))
dist_AB_antizipf = sqrt(sum(df$absolute_freq_diff_antizipf))

barplot(df$absolute_freq_diff_antizipf)


