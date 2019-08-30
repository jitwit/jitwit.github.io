pages ::= \
	index \
	dfs-alga

%.html : %.ss style-sheet.ss
	scheme -q --script $<

all:
	make $(pages:=.html)

clean:
	rm *.html
	rm *~

# .PHONY: all
