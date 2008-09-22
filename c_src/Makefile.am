bindir = $(ERLANG_INSTALL_LIB_DIR_erlycairo)/priv/bin
bin_PROGRAMS = erlycairo
erlycairo_SOURCES = erlycairo.c
erlycairo_CFLAGS = $(cairo_CFLAGS) -I$(ERLANG_LIB_DIR_erl_interface)/include
erlycairo_LDADD = $(cairo_LIBS) -lpthread -lnsl\
	-L$(ERLANG_LIB_DIR_erl_interface)/lib -lerl_interface -lei 

erlycairodir = $(ERLANG_INSTALL_LIB_DIR_erlycairo)/priv/src
erlycairo_DATA = $(erlycairo_SOURCES) $(erlycairo_INCLUDES)

