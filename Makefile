pages ::= \
	index \
	dfs-alga

%.html : %.ss style-sheet.ss
	echo "(time (render))" | scheme -q $<

all:
	make $(pages:=.html)

clean:
	rm *.html
	rm *~

# .PHONY: all
