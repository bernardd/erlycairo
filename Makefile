ERL=erl

all:    erl c-node

erl:
	$(ERL) -make

c-node:
	( cd c_src && $(MAKE) )

docs:	
	erl -pa `pwd`/ebin \
	-noinput \
	-run edoc_run application "'ErlyCairo'" '"."' '[no_packages]'

clean:	clean-erl clean-c-node
 
clean-erl:
	rm -fv ebin/*
	rm -fv erl_crash.dump

clean-c-node:
	( cd c_src && $(MAKE) clean )

clean-docs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
