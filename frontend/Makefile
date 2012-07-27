##############################################################################
# Installation of frontend
##############################################################################

CYMAKE = ../bin/.local/cymake

# repository with new front-end:
FRONTENDREPO = git://git-ps.informatik.uni-kiel.de/curry

# copy cabal installation of front end into local directory
$(CYMAKE): $(HOME)/.cabal/bin/cymake
	mkdir -p $(@D)
	cp -p $< $@

# install the sources of the front end from its repository
$(HOME)/.cabal/bin/cymake:
	$(MAKE) clone
	cd curry-base     && cabal install # --force-reinstalls
	cd curry-frontend && cabal install # --force-reinstalls

# Retrieves a fresh clone of the frontend IF IT IS NOT ALREADY PRESENT.
.PHONY: clone
clone:
	if [ ! -d curry-base ] ; then \
	  git clone $(FRONTENDREPO)/curry-base.git ; \
	fi
	if [ ! -d curry-frontend ] ; then \
	  git clone $(FRONTENDREPO)/curry-frontend.git ; \
	fi

.PHONY: update
update: clone
	cd curry-base     && git pull
	cd curry-frontend && git pull

.PHONY: clean
clean:
	-cd curry-base     && cabal clean
	-cd curry-frontend && cabal clean
