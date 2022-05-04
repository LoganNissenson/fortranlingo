PROGRAM sharedletters
IMPLICIT NONE
CHARACTER(len=5) :: str1, str2, shared, getSharedLetters

str1 = 'float'
str2 = '11111'

shared = getSharedLetters(str1,str2,5)

WRITE(*,*) TRIM(shared), LEN(shared)

END PROGRAM



CHARACTER FUNCTION getSharedLetters(string1,string2,stringLength)
! ----------------------------------------------------
! Purpose:
!   Returns a string containing the characters that string1 and string2 share
!   If they don't share any characters the function returns '-1'
!   Note:  Both strings have to be the same length
! VARIABLES USED:
!   NAME:             TYPE:     			COMMENT:
! stringlength		INTEGER				The length of the strings that are compared
! i					INTEGER				counting variable
! j					INTEGER				counting variable
! k					INTEGER				Serves as a counting variable and a flag to determine if the strings shared any characters
! shardChar			CHARACTER			The string that is returned.  It contains all the characters that the strings share or it contains '-1' if 
!										the strings do not share any characters
! --------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: stringLength
INTEGER :: i,j,k=1
CHARACTER(len=stringLength), INTENT(IN) :: string1, string2
CHARACTER(len=stringLength) :: sharedChar

DO i=1, stringLength
	sharedChar(i:i) = ''
END DO

DO i=1, stringLength
	DO j=1, stringLength
		IF(string1(i:i) == string2(j:j)) THEN
			sharedChar(k:k) = string1(i:i)
			k = k+1
		END IF
	END DO
END DO

IF(k /= 1) THEN
	getSharedLetters = sharedChar
ELSE
	getSharedLetters = '-1'
END IF

END FUNCTION