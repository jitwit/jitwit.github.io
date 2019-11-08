pages := \
	index \
	benchmarks/index \
	posts/dfs-alga

all :
	make $(pages:=.html)

%.html : %.ss load.ss
	echo "(make-page \"$<\")" | scheme -q load.ss

clean :
	find . -name "*~" -exec rm {} \;

.PHONY : clean
