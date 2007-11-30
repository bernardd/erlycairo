ERL=erl
APP_NAME=erlycairo_demo
NODE_NAME=$(APP_NAME)

all: 
	$(ERL) -make

c-node:
	( cd c_src && $(MAKE) )

docs:	
	erl -pa `pwd`/ebin \
	-noinput \
	-run edoc_run application "'ErlyCairo'" '"."' '[no_packages]'

run:
	$(ERL) -pa `pwd`/ebin \
	-s $(APP_NAME) \
	-sname $(NODE_NAME)
		
clean:
	rm -fv ebin/*
	rm -fv erl_crash.dump

clean-c-node:
	( cd c_src && $(MAKE) clean )

clean-docs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
