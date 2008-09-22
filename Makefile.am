SUBDIRS = c_src src doc

NODE_NAME = $(PACKAGE_TARNAME)
clean-local:
	-find . -name "*~" -exec rm {} \;

docs:
	erl -pa `pwd`/ebin \
   	  -noinput \
        -run edoc_run application "'ErlyCairo'" '"."' '[no_packages]'

