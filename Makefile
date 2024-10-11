.PHONY: package clean install

all:
	@echo "Run: make package"

package:
	idris2 --build qimaera.ipkg

install:
	idris2 --install qimaera.ipkg

clean:
	rm -rf build/
