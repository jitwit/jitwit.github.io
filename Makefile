pages ::= \
	index \
	dfs-alga

%.html : %.ss style-sheet.ss 
	echo "(time (render))" | scheme -q $<

all:
	make $(pages:=.html)

clean:
	rm $(pages:=.html)
	rm *~
	rm css/*~

# .PHONY: all
