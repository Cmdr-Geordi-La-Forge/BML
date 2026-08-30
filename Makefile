AS = aarch64-linux-gnu-as
LD = aarch64-linux-gnu-ld

ASFLAGS = -g
LDFLAGS = -nostdlib

SRCS = src/boot.S src/env.S src/prims.S src/eval.S src/parse.S src/rat.S src/jit.S src/print.S
OBJS = $(SRCS:.S=.o)
TARGET = compiler

all: $(TARGET)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $^

%.o: %.S
	$(AS) $(ASFLAGS) -I inc -o $@ $<

clean:
	rm -f $(OBJS) $(TARGET)
