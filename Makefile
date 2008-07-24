all: dlxc dlxf

dlxc:
	g++ -O3 -static -fomit-frame-pointer -o dlxc dlxc.cpp

dlxf:
	ifort -fast -o dlxf dlxf.f90
