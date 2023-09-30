all: dlxc dlxf

dlxc: dlxc.cpp
	g++ -O3 -static -fomit-frame-pointer -o dlxc dlxc.cpp

dlxf: dlxf.f90
	ifort -fast -o dlxf dlxf.f90
