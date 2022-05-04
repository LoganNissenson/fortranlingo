!--------------------------------------------------------------------------
!  TITLE: CS260 Lingo
!  AUTHOR: Logan Nissenson, Darren Kirk, Janessa Foraker
!  CLASS:  CS260
!  DATE WRITTEN: 4/20/12
!  LAST REVISION: 5/1/12
!  DESCRIPTION: The game lingo translated into FORTRAN (search LINGO in youtube to see an example of the actual game)
!  VARIABLES USED:
!    NAME:           	TYPE:    		 	COMMENT:
! anotherRound			CHARACTER		Controls whether or not another round is played
! unknownWord			CHARACTER		The unknown word that the teams are trying to guess
! randomWord			CHARACTER		A function that picks a random word from a file
! team1Name				CHARACTER		Team one's name	
! team2Name				CHARACTER		Team two's name	
! activeTeam			CHARACTER		The team whose current turn it is
! guess					CHARACTER		The team's guess (must be five characters)
! progressArray			CHARACTER		The array that keeps track of the progress the teams of made towards guessing the word
! guessedWords			CHARACTER		Array that holds all the guesses the teams have made in their attempt to guess the word
! getResultOfGuess		CHARACTER		Returns the result of the player's guess
! team1Stats			INTEGER			Array that keeps track of team one's stats
! team2Stats			INTEGER			Array that keeps trakc of team two's stats
! i, j					INTEGER			counting variables
! wordsInRound			INTEGER			How many words will be played in the round
! team1RandomInt		INTEGER			Random int used to determine which team goes first
! team2RandomInt		INTEGER			Random int used to determine which team goes first
! randomNumber			INTEGER			Function for calculating a 'random' number in a certain range
! totalGuesses			INTEGER			How many guesses the users have made
! points				INTEGER			The amount of points that are rewarded to whoever guesses the word correctly
!--------------------------------------------------------------------------
PROGRAM lingo

! Declaration of variables needed
IMPLICIT NONE
CHARACTER(len=1) :: anotherRound
CHARACTER(len=5) :: unknownWord, randomWord
CHARACTER(len=20) :: team1Name, team2Name, activeTeam, guess
CHARACTER(len=1), DIMENSION(5) :: progressArray ='0'
CHARACTER(len=15), DIMENSION(100) :: guessedWords = ''
CHARACTER(len=15) :: getResultOfGuess
INTEGER, DIMENSION(3) :: team1Stats=0, team2Stats=0
INTEGER :: i,j, wordsInRound, team1RandomInt, team2RandomInt, randomNumber
INTEGER :: totalGuesses = 0, points

! Welcome the user and display the rules of the program
WRITE(*,*)
WRITE(*,*) "Welcome to the game LINGO!!!"
WRITE(*,*) "*************************************************************"
WRITE(*,*) "Prepare yourself.....for great trials lay ahead"
WRITE(*,*)
CALL instructions()
WRITE(*,*)

! Get team information
WRITE(*,*) "***********************************************************"
WRITE(*,*) "Team 1, what would you like your name to be"
READ(*,*) team1Name
WRITE(*,*)
WRITE(*,*) "Team 2, what would you like your name to be"
READ(*,*) team2Name

! Now entering the Main loop of the program.  At the end of this 
DO
	IF (anotherRound == 'n' .OR. anotherRound == 'N') EXIT
	
	DO
		WRITE(*,*)
		WRITE(*,*) "How many words should be in this round?  Must be even"
		READ(*,*) wordsInRound
		IF(MOD(wordsInRound,2) == 0) EXIT
		WRITE(*,*) "Sorry that is not an even number."
	END DO
	
	! Determining which player goes first
	DO
		team1RandomInt = randomNumber(1,10)
		team2RandomInt = randomNumber(1,10)
		IF( team1RandomInt /= team2RandomInt) EXIT
	END DO
		
	IF (team1RandomInt > team2RandomInt) THEN
		activeTeam = team1Name
	ELSE
		activeTeam = team2Name
	END IF
	
	WRITE(*,*)
	WRITE(*,*) TRIM(team1Name)," you drew the number", team1RandomInt
	WRITE(*,*) TRIM(team2Name)," you drew the number", team2RandomInt
	WRITE(*,*)
	
	! Now entering the second main loop of the program.  Each cycle means a new word that the users
	! are attempting to guess
	DO i=1, wordsInRound
		
		
		! Getting the word from the data file
		unknownWord = randomWord('lingowords.txt',5)
		
		! Initializing the display array
		CALL initProgressArray(progressArray, unknownWord)
		
		WRITE(*,*) "***************************************************************************"
		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*) "The word has now been chosen!!!"
		WRITE(*,*) "It is ", progressArray
		WRITE(*,*) 
		WRITE(*,*) "Prepare for the ultimate battle of intellects!!!!  LINGO!!!!"
		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*) "****************************************************************************"
		WRITE(*,*)
		
		! Initializing the points, guessedWords, and totalGuesses
		points = 100
		guessedWords = ''
		totalGuesses = 0
		
		DO
			! Initialize guessed words back to blanks and totalGuesses to zero
			
			WRITE(*,*)
			WRITE(*,*) "The word is currently ",points," points"
			WRITE(*,*) TRIM(activeTeam), " it is your now your turn to guess"
			WRITE(*,*) "You have five guesses"
			
			DO j=1, 5
				WRITE(*,*)
			
				! Making sure that the guess is five characters
				DO
					WRITE(*,*) "What is your guess?"
					READ(*,*) guess
					IF( LEN_TRIM(guess) == 5) EXIT
					WRITE(*,*) "Sorry, your guess must be five letters"
				END DO
				
				CALL updateProgressArray(progressArray,guess,unknownWord)
				guessedWords(totalGuesses+1) = getResultOfGuess(guess,unknownWord,progressArray)
				totalGuesses = totalGuesses + 1
				
				! Updating the player statistics
				IF(activeTeam == team1Name) THEN
					team1Stats(3) = team1Stats(3)+1
				ELSE
					team2Stats(3) = team2Stats(3)+1
				END IF
				
				! Checking to see how the guess compared to the actual word
				IF(guess == unknownWord) THEN
					WRITE(*,*)
					WRITE(*,*) "Congrats!!! You guessed the word correctly!"
					
					! Giving the winning player their due
					IF(activeTeam == team1Name) THEN
						team1Stats(1) = team1Stats(1)+points
						team1Stats(2) = team1Stats(2)+1
					ELSE
						team2Stats(1) = team2Stats(1)+points
						team2Stats(2) = team2Stats(2)+1
					END IF
					EXIT
				ELSE
					CALL printResult(guess, unknownWord, progressArray, j, guessedWords, points, activeTeam,totalGuesses)
				END IF
			END DO
			
			! Switching the player
			IF(guess == unknownWord) THEN
				IF(activeTeam == team1Name) THEN
					activeTeam = team2Name
				ELSE
					activeTeam = team1Name
				END IF
				EXIT
				
			ELSE						! If the word was not guessed correctly, the active player is switched
				points = points-20
				
				! switching the player
				IF(activeTeam == team1Name) THEN
					activeTeam = team2Name
				ELSE
					activeTeam = team1Name
				END IF
			END IF
		END DO
	END DO
	
	CALL printStats(team1stats, team2stats, team1Name, team2Name)
	WRITE(*,*)
	DO
		WRITE(*,*) "Would you like to play again? y/n"
		READ(*,*) anotherRound
		IF(anotherRound == 'y' .OR. anotherRound == 'n'.OR. anotherRound == 'Y'.OR. anotherRound == 'N') EXIT
		WRITE(*,*) "Sorry that is not a correct input"
	END DO
END DO
	

	
END PROGRAM


INTEGER FUNCTION  randomNumber(low, high)
! ----------------------------------------------------
! Purpose:
!  Return a random number between low and high.  The random number may equal low or high
! VARIABLES USED:
!   NAME:                                TYPE:                     COMMENT:
!      low            INTEGER       the lower bound for the random number
!      high            INTEGER           the upper bound for the random number
!      seed             REAL           the seed used to create the random number
! --------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: low, high
REAL :: seed

CALL RANDOM_SEED

CALL RANDOM_NUMBER(seed)

randomNumber = INT(seed * ((high+1)-low)) + low

END FUNCTION



CHARACTER FUNCTION randomWord(filename, wordlength)
! ----------------------------------------------------
! Purpose: Pick a random word from a file containing words
!   
! VARIABLES USED:
!   NAME:             TYPE:     				COMMENT:
! filename			CHARACTER				The filename of the file that contains the words. Must be 14 characters long.
! wordlength		INTEGER					The length of the words in the file
! err				INTEGER					Iostat variable used for readin the data file
! amountOfWords		INTEGER					Keeps track of how many words are in the data file
! randomIndex		INTEGER					Holds a random number that is used for the index of the returned word
! randomNumber		INTEGER					The randomNumber function
!  i				INTEGER					Counting variable
! wordArray			CHARACTER				Array that holds the words from the file
! --------------------------------------------------
IMPLICIT NONE
CHARACTER(len=14), INTENT(IN) :: filename
INTEGER, INTENT(IN) :: wordlength
INTEGER :: err, amountOfWords=0, randomIndex, randomNumber,i
CHARACTER(len=5), DIMENSION(300) :: wordArray = '00000'

IF(wordArray(1) == '00000') THEN        ! If words have not already been read into the array
	OPEN(UNIT=11, FILE=filename, status='old', ACTION='READ', IOSTAT=err)

	IF(err /= 0) THEN
		WRITE(*,*) "error in file open"
		STOP
	END IF

	i=1
	DO
		IF(err /= 0) EXIT
		READ(11,*, IOSTAT = err) wordArray(i)
		amountOfWords = amountOfWords + 1
		i=i+1
	END DO

	CLOSE(11)
END IF

randomIndex = randomNumber(1,amountOfWords-1)
randomWord = wordArray(randomIndex)

END FUNCTION

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



LOGICAL FUNCTION isInString(letter,string)
! ----------------------------------------------------
! Purpose:
!  Checks to see if a letter is in an string.  If it is a 1 is returned.  Else a zero is returned
!
! VARIABLES USED:
!   NAME:             TYPE:                 COMMENT:
! letter			CHARACTER				The letter of interest
! string			CHARACTER				The string of interest
! i					INTEGER					counting variable
! flag				LOGICAL  				Set to true if the letter is in the string.  Else it is set to false
! ----------------------------------------------------
IMPLICIT NONE
CHARACTER, INTENT(IN) :: letter
CHARACTER(len=5), INTENT(IN) :: string
INTEGER :: i
LOGICAL :: flag

flag=.FALSE.

DO i=1,5
    IF(string(i:i)==letter) flag=.TRUE.
END DO

isInString = flag

END FUNCTION

LOGICAL FUNCTION isInArray(letter,array)
! ----------------------------------------------------
! Purpose:
!  Checks to see if a letter is in an array.  If it is a 1 is returned.  Else a zero is returned
!
! VARIABLES USED:
!   NAME:             TYPE:                 COMMENT:
! letter			CHARACTER				letter of interest
! array				CHARACTER				array of interest
! i					INTEGER					counting variable
! flag				LOGICAL				    Set to true if the letter is the array.  Else it is set to false
! ----------------------------------------------------
IMPLICIT NONE
CHARACTER, INTENT(IN) :: letter
CHARACTER(len=1), DIMENSION(5), INTENT(IN) :: array
INTEGER :: i
LOGICAL :: flag

flag=.FALSE.
DO i=1,5
    IF(array(i) == letter) flag=.TRUE.
END DO

isInArray = flag

END FUNCTION


CHARACTER FUNCTION getResultOfGuess(guess, unknownWord, progressArray)
! ----------------------------------------------------
! Purpose:
!  Returns a string showing how their guessed word matched up to the unknown word
!
! VARIABLES USED:
!   NAME:             TYPE:                 COMMENT:
! guess				CHARACTER			The word that the team guessed
! unknownWord	`	CHARACTER			The word that the teams are trying to guess
! progressArray		CHARACTER			The array that holds the progress the teams have made
! resultOfGuess		CHARACTER			The character that shows how the guessed word matches up to the unknown word
! i					INTEGER				counting variable
! k					INTEGER				keeps track of the current index in resultOfGuess
! letterFrequency	INTEGER				Function for calculating the frequency of a letter in a string
! isInArray			LOGICAL				Logical function for telling whether or not a character is in an array
! isInString		LOGICAL				Logical function for telling whether or not a character is in a string
! ----------------------------------------------------
IMPLICIT NONE
CHARACTER(len=5), INTENT(IN) :: guess, unknownWord
CHARACTER(len=1), DIMENSION(5), INTENT(IN) :: progressArray
CHARACTER(len=15) :: resultOfGuess
INTEGER :: i,k, letterFrequency
LOGICAL :: isInArray, isInString

DO i=1,15
    resultOfGuess(i:i) = ''
END DO



k=1
DO i=1,5
    IF (guess(i:i) == unknownWord(i:i)) THEN
        resultOfGuess(k:k) = '['
        resultOfGuess(k+1:k+1) = guess(i:i)
        resultOfGuess(k+2:k+2) = ']'
        k=k+3
    ELSE
		IF(isInString(guess(i:i),unknownWord) .AND. (.NOT. isInArray(guess(i:i),progressArray))) THEN
			IF(letterFrequency(guess(i:i),guess(1:i),i)<=letterFrequency(guess(i:i),unknownWord,5)) THEN
				resultOfGuess(k:k) = '<'
				resultOfGuess(k+1:k+1) = guess(i:i)
				resultOfGuess(k+2:k+2) = '>' 
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
!   NAME:             	TYPE:                 COMMENT:
! progressArray		   CHARACTER			  Array that holds the progress that user has made towards guessing the word
! guess				   CHARACTER			  The word that the team guessed
! unknownWord		   CHARACTER			  The unknown word that the teams are trying to guess
! i                    INTEGER                counting variable
! j                    INTEGER                counting variable
! --------------------------------------------------
IMPLICIT NONE

CHARACTER(len=1), DIMENSION(5), INTENT(OUT) :: progressArray
CHARACTER(len=5), INTENT(IN) :: guess, unknownWord 
INTEGER :: i,j


DO i=1, 5
    IF (guess(i:i) == unknownWord(i:i)) THEN
        progressArray(i) = guess(i:i)
    END IF
END DO


END SUBROUTINE

INTEGER FUNCTION letterFrequency(letter, string, stringLength)
! ----------------------------------------------------
! Purpose:
!  Returns the amount of times that a letter occurs in a string.  Aka the letter's frequency in the string
!
! VARIABLES USED:
!   NAME:             TYPE:                 COMMENT:
! letter			CHARACTER				The letter whose frequency will be found
! string			CHARACTER				The string that is being examined
! frequency			INTEGER					The amount of times the letter is in the string
! i					INTEGER					counting variable
! ----------------------------------------------------
IMPLICIT NONE
CHARACTER, INTENT(IN) :: letter
INTEGER, INTENT(IN) :: stringLength
CHARACTER(len=stringLength), INTENT(IN) :: string
INTEGER :: i, frequency
frequency = 0

DO i=1,stringLength
	IF(string(i:i) == letter) frequency = frequency+1
END DO

letterFrequency = frequency

END FUNCTION


SUBROUTINE printResult(guess, unknownWord, progressArray, turnsUsed, guessedWords, points, activeTeam,totalGuesses)
! ----------------------------------------------------
! Purpose:
!  Prints the results of the player's guess
!
! VARIABLES USED:
!   NAME:             	TYPE:                 COMMENT:
! guess				 	CHARACTER			The word that team guessed
! unknownWord			CHARACTER			The word that the teams are trying to guess
! progressArray			CHARACTER			Stores the progress the teams have made towards guessing the word
! guessedWords			CHARACTER			Array that holds all the words the teams have guessed
! activeTeam			CHARACTER			The team whose turn it is
! getResultOfGuess		CHARACTER			The result of the player's guess
! points				INTEGER				The amount of points the word is currently worth
! turnsUsed				INTEGER				The amount of turns the team has used
! totalGuesses			INTEGER				The amount of guesses used so far in the round
! i,j					INTGER				counting variables
! ----------------------------------------------------
IMPLICIT NONE
CHARACTER(len=5), INTENT(IN) :: guess, unknownWord
CHARACTER(len=1), DIMENSION(5), INTENT(IN) :: progressArray
CHARACTER(len=15), DIMENSION(100), INTENT(IN) :: guessedWords
CHARACTER(len=20), INTENT(IN) :: activeTeam
CHARACTER(len=15) :: getResultOfGuess
INTEGER, INTENT(IN) :: points,turnsUsed,totalGuesses
INTEGER :: i,j

WRITE(*,*)
WRITE(*,*) "**************************************************************************"
WRITE(*,*)
WRITE(*,*) "The words guessed so far are:"
WRITE(*,*) "---------------------------------"

DO i=1, totalGuesses, 5
	WRITE(*,*) TRIM(guessedWords(i)),'  ',TRIM(guessedWords(i+1)),'  ',TRIM(guessedWords(i+2)), &
	'  ',TRIM(guessedWords(i+3)),'  ',TRIM(guessedWords(i+4))
	WRITE(*,*)
END DO
!29	FORMAT(1X,A5,3X,A5,3X,A5,3X,A5,3X,A5)

WRITE(*,*)
WRITE(*,*) "The unknown word is  ", progressArray
WRITE(*,*) "You guessed the word ", TRIM(getResultOfGuess(guess, unknownWord, progressArray))
WRITE(*,*)
WRITE(*,*) TRIM(activeTeam)," you have ",5-turnsUsed," turns left"
WRITE(*,*) "The word is currently worth ",points," points"
WRITE(*,*)
WRITE(*,*) "***************************************************************************"

END SUBROUTINE

SUBROUTINE printStats(team1Stats,team2Stats,team1Name,team2Name)
! ----------------------------------------------------
! Purpose:
!  Print the user statistics and the winner of the game
!
! VARIABLES USED:
!   NAME:             TYPE:                 COMMENT:
! team1Stats		 INTEGER			   Array that holds the game statistics for team 1
! team2Stats		 INTEGER			   Array that holds the game statistics for team 2
! team1Name			 CHARACTER			   The name of team 1
! team2Name			 CHARACTER			   The name of team 2
! i					 INTEGER			   counting variable
! ----------------------------------------------------
INTEGER, DIMENSION(3), INTENT(IN) :: team1Stats,team2Stats
CHARACTER(len=20), INTENT(IN) :: team1Name,team2Name
INTEGER :: i

WRITE(*,*) "*********************************************************************"
WRITE(*,*)
WRITE(*,*) "             ",team1Name,"                      ",team2Name 
WRITE(*,*) "           -------------                          --------------"
WRITE(*,*) "SCORE:        ", team1Stats(1),"                                     ",team2Stats(1)
WRITE(*,*) "Guess#:       ", team1Stats(3),"                                        ",team2Stats(3)
WRITE(*,*) "#Correct:     ", team1Stats(2),"                                        ",team2Stats(2)
!38 FORMAT(1X,A20,2X,I3,11X,I2,16X,I2)
!33 FORMAT(1X,A20,2X,I3,11X,I2,16X,I2)


END SUBROUTINE


SUBROUTINE Instructions ()

WRITE(*,*) "Here are the rules of LINGO:"
WRITE(*,*) "-The object of the game is to guess a five letter word"
WRITE(*,*) "-The first letter of the word will be given to you and"
WRITE(*,*) "printed to the screen, followed by blank spaces"
WRITE(*,*) "-Each team will be given five opportunities to guess"
WRITE(*,*) "the correct word"
WRITE(*,*) "-Then the other team will be given the oppurtunity to"
WRITE(*,*) "guess the word"
WRITE(*,*) "-If the team guesses the right letter in the correct"
WRITE(*,*) "space, then it will print out the letter surrounded by [ ]"
WRITE(*,*) "-If the team guesses the right letter but it is in the"
WRITE(*,*) "wrong position, then it will print out the letter surrounded by < >"
WRITE(*,*) "-If the first team guesses the word within five tries,"
WRITE(*,*) "then the word will be worth 100 points."
WRITE(*,*) "-Each time the word changes to the other team, the point"
WRITE(*,*) "value for the word drops by 20 points."

END SUBROUTINE



		
		
		
	 