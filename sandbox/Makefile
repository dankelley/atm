SOURCES=$(wildcard *.R)
OBJECTS=$(SOURCES:.R=.out)
all: $(OBJECTS)
%.out: %.R
	Rscript $< > $@
clean: force
	-rm *.out *.pdf *~
force:
