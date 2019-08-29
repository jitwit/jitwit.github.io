pages ::= \
	index \
	dfs-alga

%.html : %.ss
	scheme -q --script $<

all:
	make $(pages:=.html)

clean:
	rm *.html
	rm *~

.PHONY: all
