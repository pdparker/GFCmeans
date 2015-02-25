# Transform .Rmd files to slidy files

.SUFFIXES: .csv .R

all: f1995.csv f1998.csv f2003.csv f2006.csv \
	m1995.csv m1998.csv m2003.csv m2006.csv \
	s1995.csv s1995.csv s1995.csv s1995.csv \
	allMeans.csv

s1995.csv m1995.csv f1995.csv: listWise1995.R
	/usr/bin/Rscript listWise1995.R

s1998.csv m1998.csv f1998.csv: listWise1998.R
	/usr/bin/Rscript listWise1998.R

s2003.csv m2003.csv f2003.csv: listWise2003.R
	/usr/bin/Rscript listWise2003.R

s2006.csv m2006.csv f2006.csv: listWise2006.R
	/usr/bin/Rscript listWise2006.R

allMeans.csv: combinedMeans.R
	/usr/bin/Rscript combinedMeans.R

clean:
	rm *.csv