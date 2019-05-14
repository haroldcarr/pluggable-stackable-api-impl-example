#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2019 Jun 03 (Mon) 09:21:29 by Harold Carr.
#

G                   = .generated

all : pdf

pdf:
	cd src; pandoc Example.lhs \
                       -o ../$(G)/Example.pdf \
                       --variable fontsize=14pt \
                       -V theme:tim \
                       -t beamer \
                       --template tim.beamer

clean :
	stack clean --full
	rm -f $(G)/*
	rm -f src/images/*.html

# End of file.
