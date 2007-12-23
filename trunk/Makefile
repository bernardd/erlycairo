ERL=erl
APP_NAME=erlycairo
NODE_NAME=$(APP_NAME)

all: 
	( $(ERL) -make && \
	if [ ! -e ebin/$(APP_NAME).app ]; then cp -f src/$(APP_NAME)/$(APP_NAME).app.src ebin/$(APP_NAME).app; fi )

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
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-c-node:
	( cd c_src && $(MAKE) clean )

clean-docs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
