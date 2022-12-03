CXX = g++-11
CXXFLAGS = -std=c++20 -g -MMD -Wall -Wextra -fsanitize=undefined,address
EXEC = sol
OBJECTS = sol.o rps.o rpsfactory.o p1.o
DEPENDS = ${OBJECTS:.o=.d}

${EXEC}: ${OBJECTS}
	${CXX} ${CXXFLAGS} ${OBJECTS} -o ${EXEC}

-include ${DEPENDS}

.PHONY: clean recomp

clean:
	rm ${EXEC} ${OBJECTS} ${DEPENDS}

recomp:
	make clean
	make
