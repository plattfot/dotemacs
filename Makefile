
VARIABLES= variables.el
all: ${VARIABLES}

${VARIABLES}:
	@pwd | xargs -I{} echo -e ";; Auto generated file by dotemacs.\n;; Do not modify!\n\n"\
	                          "(defvar dotemacs/path-to-dotemacs \"{}/dotemacs\")\n"\
	 			  "(defvar dotemacs/path-to-init.d \"{}/dotemacs\")\n"\
                                  > $@
.PHONY: install
install: ${VARIABLES}
	@grep dotemacs/path-to-init.d $< | sed -En 's/.*?"(.*)".*/\1/p' |\
	xargs -I{} sed -E 's/<path-to-init.d>/{}/' \
        -E 's/<header>/Generated by dotemacs.\n;; Do not edit this file!\n;; Instead edit the original and then type make install./'\
	dotemacs

.PHONY: clean
clean:
	rm -f ${VARIABLES}

