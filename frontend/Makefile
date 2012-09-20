##############################################################################
# Installation of frontend
##############################################################################

CYMAKE = ../bin/.local/cymake

# copy cabal installation of front end into local directory
$(CYMAKE): $(HOME)/.cabal/bin/cymake
	mkdir -p $(@D)
	#cp -p $< $@
	mv $< $@ # move cymake away to avoid problems with different architectures

# install the sources of the front end from its repository
$(HOME)/.cabal/bin/cymake:
	cd curry-base     && cabal install # --force-reinstalls
	cd curry-frontend && cabal install # --force-reinstalls

.PHONY: update
update: clone
	cd curry-base     && git pull
	cd curry-frontend && git pull

.PHONY: clean
clean:
	cd curry-base     && cabal clean
	cd curry-frontend && cabal clean
