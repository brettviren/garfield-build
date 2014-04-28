CDECK  ID>, RANDOM.
	subroutine Iniranfl
c
c	Initialize the random numbers generator
c	iranfl is intent for calc. of number of call of geenerator
c       It is so as it can be possible to figer out, where the
c       new circle starts, if the user knows the period.
c
	implicit none

c 	include  'random.inc'
        real*8 iranfl

        integer sseed              ! Flag to start first event
                                   ! from seed point of random number generator.
        real*8 rseed               ! Place for seed.
        integer seed(2)            ! Form for writting and inputting
                                   ! without modification during
                                   ! binary to demical transformation.
        equivalence (rseed,seed(1))

        common / comran /
     +  iranfl,
     +  rseed, sseed

        save / comran /
	

c	real*8 iranfl
c	common / comran / iranfl
c	save / comran /

	iranfl=0

	end
