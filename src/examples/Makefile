
EXECUTABLES = demo1 demo2 demo3
FPC_OPTS = -Fu..


all: $(EXECUTABLES)

%: %.pas
	fpc $(FPC_OPTS) $<

clean:
	rm -f *.o *.ppu *~ *.bak $(EXECUTABLES)
