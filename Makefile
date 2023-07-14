CXX		= g++
CXXFLAGS	= -g -Wall -std=c++11
EXTRAS		= lexer.cpp
LEX		= flex
OBJS		= Register.o Scope.o Symbol.o Tree.o Type.o allocator.o \
		  checker.o generator.o lexer.o parser.o string.o writer.o \
		  Label.o
PROG		= scc


all:		$(PROG)

$(PROG):	$(EXTRAS) $(OBJS)
		$(CXX) -o $(PROG) $(OBJS)

clean:;		$(RM) $(EXTRAS) $(PROG) core *.o

lexer.cpp:	lexer.l
		$(LEX) $(LFLAGS) -t lexer.l > lexer.cpp
