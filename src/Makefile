CPU = 6502
C1541 = c1541
X64 = ../launch_c64.sh
OUTPUT = "myprg.prg"
LABEL_FILE = "symbols.list"

DISKFILENAME = my.d64
DISKNAME = myprg
ID = 17

AS = acme
ASFLAGS = --format cbm --labeldump $(LABEL_FILE) --cpu $(CPU)

SRCS = \
	main.asm

all: d64

$(OUTPUT): $(SRCS)
	$(AS) -o $(OUTPUT) $(ASFLAGS) $(SRCS)

d64: $(OUTPUT)
	$(C1541) -format $(DISKNAME),$(ID) d64 $(DISKFILENAME)
	$(C1541) -attach $(DISKFILENAME) -write $(OUTPUT)
	$(C1541) -attach $(DISKFILENAME) -list

run: d64
	echo "ll \"$(LABEL_FILE)\"" > moncmds
	$(X64) -moncommands moncmds $(OUTPUT)
#	$(X64) -moncommands moncmds $(DISKFILENAME)

clean:
	rm -f $(OUTPUT) $(DISKFILENAME)



