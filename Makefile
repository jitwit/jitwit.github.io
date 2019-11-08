pages ::= \
	index \
	alga-benches
# 	dfs-alga \

%.html : %.ss load.ss Makefile
	echo "(make-page \"$<\")" | scheme -q load.ss

all:
	make $(pages:=.html)

clean:
#	rm $(pages:=.html)
	rm *~
	rm css/*~

# .PHONY: all
