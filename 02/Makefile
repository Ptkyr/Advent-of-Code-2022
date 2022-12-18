CXX = g++
CXXFLAGS = -std=c++14 -g -MMD -Wall -Wextra -fsanitize=undefined,address
EXEC = ps
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
