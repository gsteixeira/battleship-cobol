PACKAGE = battleship-cobol
PACKAGE_VERSION = 0.4.1
PACKAGE_BUGREPORT = gsteixei@gmail.com
PACKAGE_NAME = battleship-cobol
PACKAGE_STRING = battleship-cobol $(PACKAGE_VERSION)
PACKAGE_TARNAME = battleship-cobol-v$(PACKAGE_VERSION).tar.gz
BINARY_TARNAME = battleship-cobol-v$(PACKAGE_VERSION)-x86_64.tar.gz

COBC := cobc
COBC_FLAGS := --free -x

prefix = /usr/local
bindir = ${prefix}/bin
mandir = ${prefix}/man/man6/
distdir := dist
builddir := build
docsdir := docs
srcdir := .

default: $(srcdir)/battleship.cob
	@mkdir -p $(builddir)
	$(COBC) $(COBC_FLAGS) -o $(builddir)/battleship $(srcdir)/battleship.cob

run: $(builddir)/battleship
	$(COBC) $(COBC_FLAGS) -j -o $(builddir)/battleship $(srcdir)/battleship.cob

install: $(builddir)/battleship $(docsdir)/battleship.6
	install $(builddir)/battleship $(bindir)
	test -d "$(mandir)" || mkdir -p $(mandir)
	install -g 0 -o 0 -m 0644 $(docsdir)/battleship.6 $(mandir)
	gzip $(mandir)/battleship.6

uninstall: $(bindir)/battleship
	rm -rf $(bindir)/battleship
	rm -rf $(mandir)/battleship.6.gz

distclean:
	test -d "$(distdir)" && rm -Rf $(distdir)
	test -f "$(PACKAGE_TARNAME)" && rm -rf $(PACKAGE_TARNAME)
	test -f "$(BINARY_TARNAME)" && rm -rf $(BINARY_TARNAME)

dist: $(builddir)/battleship
	test -d "$(distdir)" || mkdir "$(distdir)"
	test -d "$(distdir)$(bindir)" || mkdir -p "$(distdir)$(bindir)"
	test -d "$(distdir)$(mandir)" || mkdir -p "$(distdir)$(mandir)"
	install "$(builddir)"/battleship "$(distdir)$(bindir)"
	install -m 0644 $(docsdir)/battleship.6 "$(distdir)$(mandir)"
	gzip $(distdir)$(mandir)/battleship.6
	tar czf $(BINARY_TARNAME) -C $(distdir) usr/

srcdist:
	test -d "$(distdir)" || mkdir "$(distdir)"
	tar czf $(PACKAGE_TARNAME) \
		--exclude=./$(builddir) \
		--exclude=./$(distdir) \
		--exclude=./.git* \
		--exclude=./*tar.gz .

srcdistcheck: $(PACKAGE_TARNAME)
	tar -tvf $(PACKAGE_TARNAME)

distcheck: $(BINARY_TARNAME)
	tar -tvf $(BINARY_TARNAME)

clean:
	test -d "$(builddir)" && rm -R $(builddir)
	test -d "$(distdir)" && rm -R $(distdir)
