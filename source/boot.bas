	'$INCLUDE: 'COMMON.BAS'

	PRINT
	PRINT "Initializing The Rhode Warrior:"

	RANDOMIZE TIMER

	DATA 2,Syntax error,3,RETURN without GOSUB,4,Out of DATA,5,Illegal function call
	DATA 6,Overflow,7,Out of memory,9,Subscript out of range,10,Duplicate definition
	DATA 11,Division by zero,13,Type mismatch,14,Out of string space,16,String formula too complex
	DATA 19,No RESUME,20,RESUME without error,24,Device timeout,25,Device fault,27,Out of paper
	DATA 39,CASE ELSE expected,40,Variable required,50,FIELD overflow,51,Internal error
	DATA 52,Bad file name or number,53,File not found,54,Bad file mode,55,File already open
	DATA 56,FIELD statement active,57,Device I/O error,58,File already exists,59,Bad record length
	DATA 61,Disk full,62,Input past end of file,63,Bad record number,64,Bad file name
	DATA 67,Too many files,68,Device unavailable,69,Communication-buffer overflow
	DATA 70,Permission denied,71,Disk not ready,72,Disk-media error,73,Advanced feature unavailable
	DATA 74,Rename across disks,75,Path/File access error,76,Path not found,99,?
	PRINT TAB(5);"Error messages...";
	DIM ERRMSG$(99)
	DO:READ I%,A$:ERRMSG$(I%)=A$:LOOP WHILE A$<>"?"
	PRINT "Ok."

	PRINT TAB(5);"System Info...";
	DIM LAST$(3)
	OPEN "I",#1,"SYSINFO.DAT"
	INPUT #1,CALLERS
	FOR I%=0 TO 3:INPUT #1,LAST$(I%):NEXT I%
	INPUT #1,MAINMSG$
	CLOSE #1
	PRINT "Ok."

	DATA Deceased,0,0,Visitor,2,10,Stranger,2,10,Prisoner,1,5,Serf,2,15
	DATA Squire,3,15,Knight,3,20,Noble,3,25,Lord,3,30,Proconsul,3,30
	DATA King,3,30
	PRINT TAB(5);"Access...";
	DIM LEVEL$(10),MO%(10),MC%(10)
	FOR I%=0 TO 10:READ LEVEL$(I%),MC%(I%),MO%(I%):NEXT I%
	PRINT "Ok."

	DATA No class,Fighter,Magician,Thief,Bard,Cleric,Assassin,Jester,Barbarian,Sage,Alchemist,Hero
	PRINT TAB(5);"Class...";
	DIM CLASS$(11)
	FOR I%=0 TO 11:READ CLASS$(I%):NEXT I%
	PRINT "Ok."

	PRINT TAB(5);"Armor...";
	OPEN "I",#1,"ARMOR.LIS"
	INPUT #1,MA%
	DIM ARMOR$(MA%),AC(MA%)
	FOR I%=0 TO MA%:INPUT #1,ARMOR$(I%),AC(I%):NEXT I%
	CLOSE #1
	PRINT "Ok."

	PRINT TAB(5);"Weapon...";
	OPEN "I",#1,"WEAPON.LIS"
	INPUT #1,MW%
	DIM WEAPON$(MW%), WC(MW%)
	FOR I%=0 TO MW%:INPUT #1,WEAPON$(I%),WC(I%):NEXT I%
	CLOSE #1
	PRINT "Ok."

	PRINT TAB(5);"Real Estate...";
	OPEN "I",#1,"RESTATE.LIS"
	INPUT #1,MR%
	DIM RE$(MR%),RC(MR%)
	FOR I%=0 TO MR%:INPUT #1,RE$(I%),RC(I%):NEXT I%
	CLOSE #1
	PRINT "Ok."

	PRINT TAB(5);"Security...";
	OPEN "I",#1,"SECURITY.LIS"
	INPUT #1,MS%
	DIM SECURITY$(MS%),SC(MS%)
	FOR I%=0 TO MS%:INPUT #1,SECURITY$(I%),SC(I%):NEXT I%
	CLOSE #1
	PRINT "Ok."

	PRINT TAB(5);"Spells...";
	DATA 5,Charm,10,Intuition,25,Strength,50,Accuracy,75,Shield,100,Hone,250
	DATA Teleport,500,Heal,750,Blast,1000,Resurrect,2500,Cure,5000
	DATA Disintegrate,7500
	DIM SPELL$(12),SP%(12)
	READ SP%(0):FOR I%=1 TO 12:READ SPELL$(I%),SP%(I%):NEXT I%
	PRINT "Ok."

	PRINT TAB(5);"Gangs...";
	DIM GANG$(24)
	OPEN "I",#1,"GANG.DAT"
	GANG$(0)="None"
	FOR I%=1 TO 24
		INPUT #1,GANG$(I%),GM%,GM%,GM%,GM%
	NEXT I%
	PRINT "Ok."

	ON ERROR GOTO ErrHandle
Logon:
	BEEP
	PRINT
	PRINT "Loading LOGON...";
	CHAIN "LOGON"

ErrHandle:
	PRINT "System error =";ERR
	END
