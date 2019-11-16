pages := \
	index \
	benchmarks/index \
	posts/dfs-alga \
	posts/distance-sphere \
	posts/math-html \
	posts/diff-geo \
	posts/diff-geo/ex2-1

all : $(pages:=.ss)
	make $(pages:=.html)

%.html : %.ss load.ss code/style-sheet.ss code/outils.ss
	echo "(make-page \"$<\")" | scheme -q load.ss

clean :
	find . -name "*~" -exec rm {} \;

.PHONY : clean
