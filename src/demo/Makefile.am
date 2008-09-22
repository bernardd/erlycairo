erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_erlycairo)/src
erlsrc_DATA = erlycairo_demo.erl
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_erlycairo)/ebin
erlbeam_DATA = erlycairo_demo.beam
CLEANFILES = $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
