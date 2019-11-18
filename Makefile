pages := \
	index \
	benchmarks/index \
	posts/dfs-alga \
	posts/distance-sphere \
	posts/math-html \
	posts/diff-geo \
	posts/diff-geo/ex2-1 \
	posts/minimal-webgl \
	posts/minimal-webgl-shaded \
	posts/webgl-curve-1 \
	posts/mandelbrot

all : $(pages:=.ss)
	make $(pages:=.html)

%.html : %.ss load.ss code/style-sheet.ss code/outils.ss
	echo "(make-page \"$<\")" | scheme -q load.ss

clean :
	find . -name "*~" -exec rm {} \;

deep-clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*html" -exec rm {} \;

.PHONY : clean deep-clean
