# The switch that is used for testing is the current switch.
SWITCH := default

EXE := stack.exe

# This line is somewhat problematic, as it assumes that ocamlfind and
# monolith are installed in the current switch, whatever it is.
-include $(shell ocamlfind query monolith)/Makefile.monolith
