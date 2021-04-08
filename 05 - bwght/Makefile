# -*- mode: GNUMakefile; -*-

LABS := lab6

R_LIBS_USER := ~/local/share/R

RMD_FILES := $(addsuffix .Rmd, $(LABS)) $(addsuffix -sol.Rmd, $(LABS))
HTML_FILES := $(patsubst %.Rmd,%.html,$(RMD_FILES))

all: html

html: $(HTML_FILES)

%.html: %.Rmd
	R --vanilla --slave -e "rmarkdown::render('$<')"

.PHONY: clean
clean:
	-@$(RM) $(HTML_FILES)
