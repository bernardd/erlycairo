# set OTP variable according to your environment

OTP=$(HOME)/R11B

UNAME = $(shell uname)


ifeq ($(UNAME), Darwin)
  ERL_INTERFACE=$(OTP)/erlang/lib/erl_interface-3.5.5.3
  COMPILER=gcc
  LIBS=-lcairo -lpng -lerl_interface -lei -lpthread
  INCLUDEDIRS=-I/opt/local/include/cairo -I$(ERL_INTERFACE)/include
  CC=$(COMPILER) $(INCLUDEDIRS)
  LIBDIRS=-L/opt/local/lib  -L$(ERL_INTERFACE)/darwin-x86/lib -L/usr/lib
else
  ifeq ($(UNAME), Linux)
    ERL_INTERFACE=$(OTP)/erlang/lib/erl_interface-3.5.5.3
    COMPILER=gcc
    LIBS=-lcairo -lpng -lerl_interface -lei -lnsl -lpthread  
    INCLUDEDIRS=-I/usr/include/cairo -I$(ERL_INTERFACE)/include
    CC=$(COMPILER) $(INCLUDEDIRS)
    LIBDIRS=-L/usr/lib  -L$(ERL_INTERFACE)/linux-x86/lib -L/usr/lib/happycoders
  endif
endif

OUTDIR=../priv/bin
OBJS=erlycairo.o

all: ${OBJS}
	$(CC) -o $(OUTDIR)/erlycairo ${OBJS} $(LIBDIRS) $(LIBS)
	rm -f *.o

clean:
	rm -f ../priv/bin/erlycairo