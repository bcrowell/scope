# making a new version:
#    update version below and in make_plain_text_manpage.pl
#    make post
#    touch /home/bcrowell/Documents/web/source/scope/scope.source
#    cd /home/bcrowell/Documents/web/source && make
# Update it on freshmeat.
# Updating manpage doesn't work right, for reasons I don't understand. M4 seems to remember
# the previous version of manpage.txt rather than reading it in afresh. To work around this,
# need to rename it to something other than manpage.txt, alter the scope.source file to refer
# to the new name, run M4.

VERSION = 0.1.1

FFT_VERSION = 1.28
FFT_TARBALL = Math-FFT-$(FFT_VERSION).tar.gz
FFT_DIR = Math-FFT-$(FFT_VERSION)
FILES = scope Makefile scope.1 $(FFT)

prefix=/usr
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin

MANDIR = $(prefix)/share/man/man1

default:
	# No compilation is required. The file ``scope'' contains the
	# Perl source code, and begins with human-readable documentation on how to install the program.

install: scope.1
	- perl -e 'if (eval "require Math::FFT") {print "Math::FFT is already installed."} else {system("make fft")}'
	perl -e 'open(F,"<scope") or die "file not found"; local $$/; $$code = <F>; close F; open(F,">temp") or die "error writing"; print F "#!".`which perl`."\n$$code"; close F;'
	# ... make sure it starts with the proper #! line, regardless of whether we're on Linux, BSD, etc.
	- test -d $(DESTDIR)$(bindir) || mkdir -p $(DESTDIR)$(bindir)
	# ... if the intended directory doesn't exist, create it
	install -m 755 temp $(DESTDIR)$(bindir)/scope
	# ... 755=u:rwx,go:rx
	rm temp
	gzip -9 <scope.1 >scope.1.gz
	- test -d $(DESTDIR)$(MANDIR) || mkdir -p $(DESTDIR)$(MANDIR)
	install -m 644 scope.1.gz $(DESTDIR)$(MANDIR)
	rm -f scope.1.gz

fft:
	tar -zxf $(FFT_TARBALL)
	cd $(FFT_DIR) && perl Makefile.PL && make && make install && cd - && rm -Rf $(FFT_DIR)

deinstall:
	rm -f $(DESTDIR)$(bindir)/scope
	rm -f $(DESTDIR)$(MANDIR)/scope.1.gz

dist: scope.tar.gz
	#

scope.tar.gz: $(FILES) scope.1
	rm -Rf scope_dist
	mkdir scope_dist
	cp $(FILES) scope_dist
	tar -zcvf scope.tar.gz scope_dist
	rm -Rf scope_dist

clean:
	rm -Rf scope*.tar.gz
	rm -f scope.1.gz
	rm -Rf *.dsc *.asc *.changes *.diff.gz
	rm -f *~
	rm -f scope.1
	rm -Rf scope_dist $(FFT_DIR)

post: scope.tar.gz scope scope.1
	cp scope.tar.gz $(HOME)/Lightandmatter/scope
	make_plain_text_manpage.pl >$(HOME)/Documents/web/source/scope/manpage.txt

scope.1: scope
	pod2man --section=1 --center="scope $(VERSION)" --release="$(VERSION)" \
	        --name=scope <scope >scope.1
