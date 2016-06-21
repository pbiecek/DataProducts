CXX=g++
CXXFLAGS=-I.
DEPS = main.h
OBJ = hellomake.o hellofunc.o 

%.o: %.c $(DEPS)
	$(CXX) -c -o $@ $< $(CXXFLAGS)

jnpcleaner: $(OBJ)
	g++ -o $@ $^ $(CXXFLAGS)