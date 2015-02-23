# Transform .Rmd files to slidy files

.SUFFIXES: .csv .R

all: f1995.csv f1998.csv f2003.csv f2006.csv

f1995.csv: listWise1995.R
	/usr/bin/Rscript listWise1995.R

f1998.csv: listWise1998.R
	/usr/bin/Rscript listWise1998.R

f2003.csv: listWise2003.R
	/usr/bin/Rscript listWise2003.R

f2006.csv: listWise2006.R
	/usr/bin/Rscript listWise2006.R

clean:
	rm *.csv