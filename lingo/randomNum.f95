Program randNum
IMPLICIT NONE
INTEGER:: rand1

rand1 = randomNumber(3,9)

WRITE(*,*) rand1




END PROGRAM

INTEGER FUNCTION randomNumber(low, high)
! ----------------------------------------------------
! Purpose:
!  Return a random number between low and high.  The random number may equal low or high
! VARIABLES USED:
!   NAME:             TYPE:     			COMMENT:
!  low				INTEGER					the lower bound for the random number
!  high				INTEGER					the upper bound for the random number
!  seed 			REAL					the seed used to create the random number
! -----------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: low, high
REAL :: seed

CALL RANDOM_SEED

CALL RANDOM_NUMBER(seed)

randomNumber = INT(seed * ((high+1)-low)) + low

END FUNCTION


