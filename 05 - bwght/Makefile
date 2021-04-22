# -*- mode: GNUMakefile; -*-

LABS := wageprc

R_LIBS_USER := ~/local/share/R

RMD_FILES := $(addsuffix .Rmd, $(LABS)))
HTML_FILES := $(patsubst %.Rmd,%.html,$(RMD_FILES))

all: html

html: $(HTML_FILES)

%.html: %.Rmd
	R --vanilla --slave -e "rmarkdown::render('$<')"

.PHONY: clean
clean:
	-@$(RM) $(HTML_FILES)
