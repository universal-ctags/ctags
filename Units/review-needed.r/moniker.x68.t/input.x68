* http://www.xrmx.com/solutions/software/68k-fe/samples/moniker.x68
*       MONIKER.X68
*       Author : Greg Colley
*       Date   : 29/01/99
 
*       Program Description.
*       This will prompt for surname and firstname, and check if its uppercase
*       If it is it prints Initial + surname else it repromts.
*	  This program will only exit when nothing is entered in the surname or 
*	  firstname.
 
PRTSTR  	EQU     1                       Print string Function
READSTR 	EQU     2                       Read string function

        	ORG     $1000                   Start of code location

*       	Print user prompt for enter the firstname
*		=========================================
START   	MOVEA.L #PROMPT1,A1             Pointer to start of prompt text
        	MOVE.B  #PRTSTR,D0              Set up print string function
        	MOVE.W  #(PROMPT2-PROMPT1),D1   The prompt string length
        	TRAP    #15                     Print Prompt
        
*       	Get firstname
*		=============
        	MOVEA.L #F_NAME,A1              Pointer to store teh sentance
        	MOVE.B  #READSTR,D0             Set up readstring function
        	TRAP    #15                     Get string from KB
        	MOVE.W  D1,D4                   Save length of input string to d4
            

*       	Check if Return is pressed
        	CMPI.W  #0,D4                   Is the length = 0
        	BEQ     QUIT                    If length = 0 then Quit

*		Set up the stuff to check it the entered word is in CAPS
*		========================================================
      	MOVEA.L #F_NAME,A0              Move the first char to A0
       	JSR     CHECK2                  Check if uppercase
        
        	CMPI.B  #1,D5                   See if all the sentance is CAPS
        	BCS     START                   if it is'nt then re-enter                          


*       	Print user prompt for enter the surname
*		=======================================
SURNAME 	MOVEA.L #PROMPT2,A1             Pointer to start of prompt text
        	MOVE.B  #PRTSTR,D0              Set up print string function
        	MOVE.W  #(F_NAME-PROMPT2),D1    The prompt string lenght
        	TRAP    #15                     Print Prompt
        
*       	Get surname
*		===========
        	MOVEA.L #S_NAME,A1              Pointer to store the sentance
        	MOVE.B  #READSTR,D0             Set up readstring function
        	TRAP    #15                     Get string from KB
        	MOVE.W  D1,D4                   Save length on input string
            MOVE.W  D1,D3                   Save length of input string to d3

*       	Check is Return is pressed
        	CMPI.W  #0,D4                   Is the length = 0
        	BEQ     QUIT                    If length = 0 then Quit

*		Set up the stuff to check it the entered word is in CAPS
*		========================================================
        	MOVEA.L #S_NAME,A0              Move the first char to A0  
        	JSR     CHECK2                  Check if uppercase        
        
        	CMPI.B  #1,D5                   See if all the sentance is CAPS
        	BCS     SURNAME                 if it is'nt then re-enter     

*       	Move the first char for fname and prints it (Initial Bit)
*		=========================================================
INITIAL 	MOVEA.L #F_NAME,A1              Move the first char to A1
        	MOVE.B  (A1),D1                 Move the first char of F_NAME to D1
        	MOVE.B  #6,D0                   Set up trap number
        	TRAP    #15                     Print the Initial

PRNSURNAME	MOVEA.L #S_NAME,A1              Pointer to start of prompt text
        	MOVE.B  #0,D0                   Set up print string function
        	MOVE.W  D3,D1                   The prompt string lenght
        	TRAP    #15                     Print Prompt

QUIT    	STOP    #$2700                  Stop the prorgam


*       	Check if uppercase
*		==================
*		This subroutine will return a 1 in, d5 if it's OK or 
*        	return a 0 in d5 if its not ok.

CHECK2  	CMPI.B  #'A',(A0)               Is Char > A ?
        	BCS     RETURNFALSE             If no then re-enter
        	CMP.B   #('Z'+1),(A0)+          Check if char is < Z
        	BCC     RETURNFALSE             If it is then it must be a cap
        	SUBI.B  #1,D4                   Decrease s_name / f_name Length
        	BNE     CHECK2                  jump if the sentance is not = 0

RETURNTRUE  MOVE.B  #1,D5			  Moves a one to D5 to make CAPS ture
            RTS					  Jump back to the main program

RETURNFALSE MOVE.B  #0,D5			  Moves a zero to D5 to make CAPS false
            RTS                             Jump back to the main program



*       	Var's & Const's
*		===============

PROMPT1 DC.B    'Please enter your firstname (Max 80): '
PROMPT2 DC.B    'Please enter your surname   (Max 80): '
F_NAME  DS.B    80
S_NAME  DS.B    80
DUMMY	  DS.B    1

		END	$1000				End of assembley
