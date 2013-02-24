.PHONY: all doc install release

version = 1.0
fullname = netamqp-$(version)

all:
	omake

doc:
	omake doc/html

install:
	ocamlfind install netamqp \
		META *.mli *.cmi netamqp.cma amqp0-9-1.xml \
		-optional netamqp.cmxa netamqp.a \
		-patch-version "$(version)"

# Note that the files netamqp_method_0_9.ml* are generated. For running
# the generator we need PXP, though, so by distributing the generated
# files we avoid this dependency.

FILES = \
  netamqp_basic.mli \
  netamqp_basic.ml \
  netamqp_channel.mli \
  netamqp_channel.ml \
  netamqp_connection.mli \
  netamqp_connection.ml \
  netamqp_endpoint.mli \
  netamqp_endpoint.ml \
  netamqp_exchange.mli \
  netamqp_exchange.ml \
  netamqp_queue.mli \
  netamqp_queue.ml \
  netamqp_rtypes.mli \
  netamqp_rtypes.ml \
  netamqp_transport.mli \
  netamqp_transport.ml \
  netamqp_tx.mli \
  netamqp_tx.ml \
  netamqp_types.mli \
  netamqp_types.ml \
  amqp_gen.ml \
  amqp0-9-1.xml \
  META \
  Makefile \
  OMakefile \
  OMakeroot \
  INSTALL \
  LICENSE

GFILES = \
  generated/netamqp_methods_0_9.ml \
  generated/netamqp_methods_0_9.mli


release:
	if [ ! -d doc/html ]; then echo "No docs!"; exit 1; fi
	mkdir -p release
	rm -rf release/$(fullname)
	mkdir release/$(fullname)
	mkdir release/$(fullname)/doc
	mkdir release/$(fullname)/doc/html
	mkdir release/$(fullname)/generated
	mkdir release/$(fullname)/examples
	cp $(FILES) release/$(fullname)
	cp $(GFILES) release/$(fullname)/generated
	cp examples/*.ml release/$(fullname)/examples
	cp doc/html/*.html release/$(fullname)/doc/html
	cp doc/html/*.css release/$(fullname)/doc/html
	cp doc/*.pdf release/$(fullname)/doc
	cd release && tar czf $(fullname).tar.gz $(fullname)


