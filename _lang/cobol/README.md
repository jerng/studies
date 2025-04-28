#### Install : GNU Cobol : Ubuntu
```
sudo apt install gnucobol4
```

###### Code Sample 

`hello.cob` :
```cobol 
      * Full-line comment with '*' on COL7 : example ;
      *
      * Code is allowed to be on COL8 to COL72 ;
      *
      * Blank lines are allowed : 

      * Divisions are a thing : there are four kinds :
      *   IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE

       IDENTIFICATION DIVISION. 
       PROGRAM-ID. hello. *> Inline comment with '*>' : example
      
       ENVIRONMENT DIVISION
      * Vacuous Division declarations must be terminated by '.'
       .

       DATA DIVISION
       .

       PROCEDURE DIVISION.
       DISPLAY "Hello World!".
       STOP RUN.
```

#### Compile and Execute
```
cobc -x -o hello.out hello.cob
./hello.out
```
