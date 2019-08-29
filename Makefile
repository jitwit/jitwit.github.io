index.html:
	echo "(run)" | scheme -q "index.ss"

dfs-alga.html:
	echo "(run)" | scheme -q "dfs-alga.ss"

clean:
	find . -name "*.so" -exec rm {} \;
	find . -name "*~" -exec rm {} \;



