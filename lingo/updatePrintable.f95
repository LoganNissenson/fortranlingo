PROGRAM update
IMPLICIT NONE

CHARACTER(len=1), DIMENSION(5) :: progressArray = '0'
CHARACTER(len=5) :: unknownWord, guess
CHARACTER(len=15) :: getResultOfGuess 
INTEGER :: i

unknownWord = 'float'
CALL initProgressArray(progressArray,unknownWord)

WRITE(*,*) progressArray

guess = 'flame'

CALL updateProgressArray(progressArray,guess, unknownWord)

WRITE(*,*) progressArray
WRITE(*,*) getResultOfGuess(guess,unknownWord,progressArray)


END PROGRAM

SUBROUTINE initProgressArray(progressArray, unknownWord)
! ----------------------------------------------------
! Purpose:
!    Initializes the display array (the printable form of the word) to the first letter of the unknown word
!    followed by four underscores
! VARIABLES USED:
!   NAME:             TYPE:                 COMMENT:
! displayArray      CHARACTER            The array that is being intialized
! unknownWord        CHARACTER            The unknown word that the teams are trying to guess
! i                    INTEGER                Counting variable
! --------------------------------------------------
IMPLICIT NONE
CHARACTER(len=1), DIMENSION(5), INTENT(OUT) :: progressArray
CHARACTER(len=5), INTENT(IN) :: unknownWord
INTEGER :: i

progressArray(1) = unknownWord(1:1)

DO i=2,5
    progressArray(i) = '_'
END DO

END SUBROUTINE



INTEGER FUNCTION isInString(letter,string)
CHARACTER, INTENT(IN) :: letter
CHARACTER(len=5), INTENT(IN) :: string
INTEGER :: i, flag

flag=0

DO i=1,5
    IF(string(i:i)==letter) flag=1
END DO

isInString = flag

END FUNCTION

INTEGER FUNCTION isInArray(letter,array)
CHARACTER, INTENT(IN) :: letter
CHARACTER(len=1), DIMENSION(5), INTENT(IN) :: array
INTEGER :: i,flag

flag=0
DO i=1,5
    IF(array(i) == letter) flag=1
END DO

isInArray = flag

END FUNCTION


CHARACTER FUNCTION getResultOfGuess(guess, unknownWord, progressArray)

CHARACTER(len=5), INTENT(IN) :: guess, unknownWord
CHARACTER(len=1), DIMENSION(5), INTENT(IN) :: progressArray
CHARACTER(len=5) :: sharedLetters, getSharedLetters
CHARACTER(len=15) :: resultOfGuess
INTEGER :: i,j,k, isInString, isInArray

DO i=1,15
    resultOfGuess(i:i) = ''
END DO

sharedLetters = getSharedLetters(guess,unknownWord,5)

k=1
DO i=1,5
    IF (guess(i:i) == unknownWord(i:i)) THEN
        resultOfGuess(k:k) = '<'
        resultOfGuess(k+1:k+1) = guess(i:i)
        resultOfGuess(k+2:k+2) = '>'
        k=k+3
    ELSE
        IF( TRIM(sharedLetters) /= '1') THEN   ! getSharedLetters returns '-1   ' if the two words don't share any letters
            IF(isInString(guess(i:i),unknownWord)/=0 .AND. isInArray(guess(i:i),progressArray)/=1) THEN
                resultOfGuess(k:k) = '"'
                resultOfGuess(k+1:k+1) = guess(i:i)
                resultOfGuess(k+2:k+2) = '"' 
                k=k+3    
            ELSE
                resultOfGuess(k:k) = guess(i:i)
                k=k+1
            END IF
        ELSE
            resultOfGuess(k:k) = guess(i:i)
            k=k+1
        END IF
    END IF
END DO

getResultOfGuess = resultOfGuess

END FUNCTION


SUBROUTINE updateProgressArray(progressArray, guess, unknownWord)
! ----------------------------------------------------
! Purpose:
!  Updates the display array of the game (the array that shows the user the progress they have made towards guessing the word)
!
! VARIABLES USED:
!   NAME:             TYPE:                 COMMENT:
! sharedLetters        CHARACTER            A character string that holds the characters that the guess shares with the unknown word
! i                    INTEGER                counting variable
! j                    INTEGER                counting variable
! --------------------------------------------------
IMPLICIT NONE

CHARACTER(len=1), DIMENSION(5), INTENT(OUT) :: progressArray
CHARACTER(len=5), INTENT(IN) :: guess, unknownWord 
!CHARACTER(len = 5) :: sharedLetters, getSharedLetters
INTEGER :: i,j


DO i=1, 5
    IF (guess(i:i) == unknownWord(i:i)) THEN
        progressArray(i) = guess(i:i)
    END IF
END DO


END SUBROUTINE