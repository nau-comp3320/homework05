
SUBDIRS = doc

.PHONY: subdirs $(SUBDIRS)

all: $(SUBDIRS) index.html

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

index.html: README.asciidoc
	asciidoctor -o $@ $<
