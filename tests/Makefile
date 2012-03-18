
WAMCC=../dist/build/wam/wam

SOURCES=$(shell ls *.pl)

WAMFILES= $(SOURCES:.pl=.wam)

all: $(WAMFILES)

%.wam : %.pl
	$(WAMCC) -i $< -o $@ -c

clean:
	-rm -f $(WAMFILES)

stats:
	ls $(WAMFILES) | xargs wc -l
