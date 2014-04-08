
TALKS = $(shell find . -maxdepth 1 -type d -path "./*")

.PHONY: clean cleanall
clean:
	for d in $(TALKS); do cd $$d && make clean; cd $(CURDIR); done

cleanall:
	for d in $(TALKS); do cd $$d && make cleanall; cd $(CURDIR); done