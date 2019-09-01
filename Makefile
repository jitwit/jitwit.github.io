rwildcard=$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))

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
	rm css/*~

# .PHONY: all
