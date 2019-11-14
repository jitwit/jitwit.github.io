pages := \
	index \
	benchmarks/index \
	posts/dfs-alga \
	posts/distance-sphere

all : $(pages:=.ss)
	make $(pages:=.html)

%.html : %.ss load.ss code/style-sheet.ss code/outils.ss
	echo "(make-page \"$<\")" | scheme -q load.ss

clean :
	find . -name "*~" -exec rm {} \;

.PHONY : clean
