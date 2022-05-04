PROGRAM initialize

IMPLICIT NONE
CHARACTER(len=1), DIMENSION(5) :: displayArray = '0'
CHARACTER(len=5) :: word

word = 'gorse'

CALL initDisplayArray(displayArray,word)

WRITE(*,*) displayArray




END PROGRAM

SUBROUTINE initDisplayArray(displayArray, unknownWord)
! ----------------------------------------------------
! Purpose:
!    Initializes the display array (the printable form of the word) to the first letter of the unknown word
!    followed by four underscores
! VARIABLES USED:
!   NAME:             TYPE:     			COMMENT:
! displayArray  	CHARACTER			The array that is being intialized
! unknownWord		CHARACTER			The unknown word that the teams are trying to guess
! i					INTEGER				Counting variable
! --------------------------------------------------
IMPLICIT NONE
CHARACTER(len=1), DIMENSION(5), INTENT(OUT) :: displayArray
CHARACTER(len=5), INTENT(IN) :: unknownWord
INTEGER :: i

displayArray(1) = unknownWord(1:1)

DO i=2,5
	displayArray(i) = '_'
END DO

END SUBROUTINE

