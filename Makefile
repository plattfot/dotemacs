ROOT = $(shell pwd)
INIT_DIR = $(ROOT)/init.d
PLUGINS_DIR =$(ROOT)/plugins

INIT = configuration
PLUGINS = dotemacs dd-log-parser dd-newfile dd-pybuild2 highlight-extra houdini work

plugins_comp_files = $(PLUGINS:%=$(PLUGINS_DIR)/%.elc)

.PHONY: all
all: init plugins

.PHONY: plugins
plugins: $(plugins_comp_files)

.PHONY: init
init: $(INIT_DIR)/$(INIT).elc

%.elc : %.el
	emacs --batch \
	--eval "(package-initialize)" \
	--eval "(add-to-list 'load-path \"~/.emacs.d/plugins/\")" \
	--eval "(byte-compile-file \"$<\")"

$(INIT_DIR)/$(INIT).elc: $(INIT_DIR)/$(INIT).org
	emacs --batch \
		--eval "(require 'org)" \
		--eval "(org-babel-load-file \"$<\")" \
		--eval "(byte-compile-file \"$*.el\")"
.PHONY: clean
clean:
	rm -f -- $(INIT_DIR)/$(INIT).el{,c} $(plugins_comp_files)
