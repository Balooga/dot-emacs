all: parenface

VER=1.3

parenface: parenface.el parenface-pkg.el test-load.el LICENSE README.md
	mkdir /tmp/parenface-$(VER)
	cp ./parenface.el /tmp/parenface-$(VER) 
	cp ./parenface-pkg.el /tmp/parenface-$(VER) 
	cp ./test-load.el /tmp/parenface-$(VER) 
	cp ./LICENSE /tmp/parenface-$(VER) 
	cp ./README.md /tmp/parenface-$(VER) 
	tar -C /tmp -cvf /tmp/parenface-$(VER).tar parenface-$(VER)

clean:
	rm -rf /tmp/parenface-$(VER)
	rm /tmp/parenface-$(VER).tar
