Tired of typing write.table(df, "file.bed", sep="\t", quote=F, row.names=F, col.names=F) for the nth time?

Bedtools failing because R silently converted large coordinates into scientific notation (like 1e+06)?
Accidental headers or row names breaking your tab-delimited columns?
Invalid coordinates causing start > end errors downstream?

We’ve all been there.

bed_exporter fixes this with a single line of code. Export clean BED files without issues!

source("https://raw.githubusercontent.com/ezecalvo/bed_exporter/main/bed_exporter.R")

write_bed(my_df, "out.bed")
