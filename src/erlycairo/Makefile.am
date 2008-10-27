erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_erlycairo)/src
erlsrc_DATA = erlycairo_app.erl erlycairo.erl erlycairo_server.erl erlycairo_sup.erl
EXTRA_DIST = $(erlsrc_DATA) erlycairo.app.src

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_erlycairo)/ebin
erlbeam_DATA = erlycairo_app.beam erlycairo.beam erlycairo_server.beam erlycairo_sup.beam erlycairo.app
CLEANFILES = $(erlbeam_DATA)

SUFFIXES = .erl .beam 
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<

erlycairo.app: erlycairo.app.src
	cp $< $@
