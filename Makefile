default: battleship.cob
	@mkdir -p build/
	cobc --free -x -o build/battleship battleship.cob

run: build/battleship
	./build/battleship

install: build/battleship
	install ./build/battleship /usr/local/bin/

uninstall: /usr/local/bin/battleship
	rm -rf /usr/local/bin/battleship

dep:
	apt-get install -y gnucobol

clean: build/battleship
	rm -R build/
	


