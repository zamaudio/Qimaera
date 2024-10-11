.PHONY: package clean install

all:
	@echo "Run: make package"

package:
	idris2 --build shor.ipkg

install: package
	idris2 --install shor.ipkg

clean:
	rm -rf build/
