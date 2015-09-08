PREFIX = ~/.emacs.d/plugins/dotemacs
NAME=dotemacs.el
PLUGIN = $(PREFIX)/$(NAME)

HEADER = Generated by dotemacs.\n;; Do not edit this file!\n;; Instead edit the original and then type make install.

all: ${PLUGIN}

$(PLUGIN): $(NAME) | $(PREFIX)
	@echo "Installing plugin..."
	@pwd | xargs -I{} sed -re 's:<path-to-dotemacs>:{}\/dotemacs:' \
	-e 's:<path-to-init\.d>:{}\/init.d:' -e 's/<header>/$(HEADER)/' \
	$< > $@

$(PREFIX):
	mkdir -p $@

.PHONY: install
install: ${PLUGIN}
	@echo "Creating symbolic link..."
	@grep dotemacs/path-to-dotemacs $(PLUGIN) | sed -En 's/.*?"(.*)".*/\1/p' | xargs -I{} ln -s {} ~/.emacs

.PHONY: uninstall
uninstall:
	rm -rf ${PREFIX}
	find ~/.emacs -type l -exec rm -f {} \;