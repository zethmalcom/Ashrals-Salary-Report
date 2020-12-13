        IDENTIFICATION DIVISION.
        PROGRAM-ID. PROGRAM-4.
        AUTHOR. Zeth Malcom.
      ****************************************************************
      *  ASHRALS, Ltd. is a company that sells costumes, shoes, and 
      *  accessories to costume shops. This program sorts the company's
      *  VENDOR, WAREHOUSE AND COSTUMEID and merges the company's
      *  inventory files into one file.  Then a summary report file will
      *  be produced from this file showing the quantity and value of 
      *  the costumes, shoes, and accesories, gtouped by the venfor they
      *  purchased from, the warehouse they are sent to, and the costume
      *  information.
      * ***************
      *  INPUT:
      *     The INVENTORY FILES contains the following
      *     data in each record:
      *         1.  VENDOR ID
      *         2.  WAREHOUSE ID
      *         3.  COSTUME ID
      *         4.  COSTUME NAME
      *         5.  COSTUME SIZE
      *         6.  COSTUME TYPE
      *         7.  NUMBER IN STOCK
      *         8.  REORDER POINT
      *         9.  COSTUME PRICE 
      *
      * ***************
      *  OUTPUT:
      *      The DETAILED SUMMARY REPORT contains 
      *      the following information:
      *    ************
      *    DETAIL LINE:
      *         1.  EXPANDED VENDOR ID
      *         2.  EXPANDED WAREHOUSE ID 
      *         3.  COSTUME NAME
      *         4.  COSTUME SIZE
      *         5.  COSTUME TYPE
      *         6.  QTY IN STOCK
      *         7.  TOTAL COST
      *
      *    *************
      *    FINAL TOTALS:
      *         1.  TOTAL COST FOR EACH COSTUME, WAREHOUSE, VENDOR, AND 
      *             ALSO A GRAND TOTAL LINE
      *
      ****************************************************************

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER.    IBM-PC.
        OBJECT-COMPUTER.    IBM-PC.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT UNSORTED-FILE1
                ASSIGN TO 'UNSORTED-CH20P4F20.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT UNSORTED-FILE2 
                ASSIGN TO 'UNSORTED-LA10P4F20.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT UNSORTED-FILE3 
                ASSIGN TO 'UNSORTED-NY30P4F20.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT INVENTORY-FILE1
                ASSIGN TO 'INVENTORYFILE1.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT INVENTORY-FILE2
                ASSIGN TO 'INVENTORYFILE2.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT INVENTORY-FILE3
                ASSIGN TO 'INVENTORYFILE3.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.

            SELECT MERGED-INVENTORY-FILE
               ASSIGN TO "MERGEDINVENTORY.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

            SELECT DETAILED-SUMMARY-REPORT
                ASSIGN TO PRINTER 'ASHRALS-DSR.TXT'.

            SELECT INVENTORY-ERROR-REPORT
                ASSIGN TO PRINTER 'ERRORS-DSR.TXT'.

            SELECT CORRECT-INVENTORY-REPORT
                ASSIGN TO "CORRECT-INVENTORY.TXT"
                ORGANIZATION IS LINE SEQUENTIAL.





      *  YOU NEED SELECT STATEMENT(S) FOR THE SD FILES
            SELECT SORT-FILE1 
                ASSIGN TO 'SORTINGFILE1.TMP'.

            SELECT SORT-FILE2 
                ASSIGN TO 'SORTINGFILE2.TMP'.
            
            SELECT SORT-FILE3 
                ASSIGN TO 'SORTINGFILE3.TMP'.

            SELECT MERGEFILE 
                ASSIGN TO "MERGEFILE.TMP". 

        DATA DIVISION.
        FILE SECTION.

        FD UNSORTED-FILE1
           RECORD CONTAINS 136 CHARACTERS.
      
       01  UNSORTED-RECORD1.
           05  UR1-VENDOR-ID                    PIC X(4).
           05  UR1-WAREHOUSE-ID                 PIC X(3).
           05  UR1-COSTUME-ID                   PIC X(3).
           05  FILLER                           PIC X(126).

        FD UNSORTED-FILE2
           RECORD CONTAINS 136 CHARACTERS.
      
       01  UNSORTED-RECORD2.
           05  UR2-VENDOR-ID                    PIC X(4).
           05  UR2-WAREHOUSE-ID                 PIC X(3).
           05  UR2-COSTUME-ID                   PIC X(3).
           05  FILLER                           PIC X(126).
      
        FD UNSORTED-FILE3
           RECORD CONTAINS 136 CHARACTERS.
      
       01  UNSORTED-RECORD3.
           05  UR3-VENDOR-ID                    PIC X(4).
           05  UR3-WAREHOUSE-ID                 PIC X(3).
           05  UR3-COSTUME-ID                   PIC X(3).
           05  FILLER                           PIC X(126).

        FD INVENTORY-FILE1
           RECORD CONTAINS 136 CHARACTERS.
      
       01  INVENTORY-RECORD1.
           05  IR1-VENDOR-ID                    PIC X(4).
           05  IR1-WAREHOUSE-ID                 PIC X(3).
           05  IR1-COSTUME-ID                   PIC X(3).
           05  FILLER                           PIC X(126).

        FD INVENTORY-FILE2
           RECORD CONTAINS 136 CHARACTERS.
      
       01  INVENTORY-RECORD2.
           05  IR2-VENDOR-ID                    PIC X(4).
           05  IR2-WAREHOUSE-ID                 PIC X(3).
           05  IR2-COSTUME-ID                   PIC X(3).
           05  FILLER                           PIC X(126).
        
        FD INVENTORY-FILE3
           RECORD CONTAINS 136 CHARACTERS.
      
       01  INVENTORY-RECORD3.
           05  IR3-VENDOR-ID                    PIC X(4).
           05  IR3-WAREHOUSE-ID                 PIC X(3).
           05  IR3-COSTUME-ID                   PIC X(3).
           05  FILLER                           PIC X(126).
      
        FD MERGED-INVENTORY-FILE
           RECORD CONTAINS 136 CHARACTERS.
      
       01  MERGED-INVENTORY-REC.
           05  MI-VENDOR-ID                      PIC X(4).
           05  MI-WAREHOUSE-ID                   PIC X(3).
           05  MI-COSTUME-ID                     PIC X(3).
           05  MI-COSTUME-ARRAY OCCURS 6 TIMES.
               10  MI-COSTUME-NAME               PIC X(9).
               10  MI-COSTUME-SIZE               PIC A.
               10  MI-COSTUME-TYPE               PIC A.
               10  MI-QTY-IN-STOCK               PIC S9(3).
               10  MI-REORDER-POINT              PIC S9(3).
               10  MI-COSTUME-PRICE              PIC S99V99.

       FD CORRECT-INVENTORY-REPORT
           RECORD CONTAINS 136 CHARACTERS.
      
       01  CORRECT-INVENTORY-FILE.
           05  CI-VENDOR-ID                      PIC X(4).
           05  CI-WAREHOUSE-ID                   PIC X(3).
           05  CI-COSTUME-ID                     PIC X(3).
           05  CI-COSTUME-ARRAY OCCURS 6 TIMES.
               10  CI-COSTUME-NAME               PIC X(9).
               10  CI-COSTUME-SIZE               PIC A.
               10  CI-COSTUME-TYPE               PIC A.
               10  CI-QTY-IN-STOCK               PIC S9(3).
               10  CI-REORDER-POINT              PIC S9(3).
               10  CI-COSTUME-PRICE              PIC S99V99.
      *YOU WILL NEED SD DESCRIPTION(S) FOR THE SORT AND MERGE FILES
       
       SD SORT-FILE1.
       01 SORT-RECORD1.
           05  SORT-VENDOR-ID1                      PIC X(4).
           05  SORT-WAREHOUSE-ID1                   PIC X(3).
           05  SORT-COSTUME-ID1                     PIC X(3).
           05  SORT-COSTUME-DATA-ARRAY1 OCCURS 6 TIMES.
               10  SORT-COSTUME-NAME1               PIC X(9).
               10  SORT-COSTUME-SIZE1               PIC A.
               10  SORT-COSTUME-TYPE1               PIC A.
               10  SORT-REORDER-POINT1              PIC S9(3).
               10  SORT-NUMBER-IN-STOCK1            PIC S9(3).
               10  SORT-PURCHASE-PRICE1             PIC S9(2)V99.
           
       SD SORT-FILE2.
       01 SORT-RECORD2.
           05  SORT-VENDOR-ID2                      PIC X(4).
           05  SORT-WAREHOUSE-ID2                   PIC X(3).
           05  SORT-COSTUME-ID2                     PIC X(3).
           05  SORT-COSTUME-DATA-ARRAY2 OCCURS 6 TIMES.
               10  SORT-COSTUME-NAME2               PIC X(9).
               10  SORT-COSTUME-SIZE2               PIC A.
               10  SORT-COSTUME-TYPE2               PIC A.
               10  SORT-REORDER-POINT2              PIC S9(3).
               10  SORT-NUMBER-IN-STOCK2            PIC S9(3).
               10  SORT-PURCHASE-PRICE2             PIC S9(2)V99.

       SD SORT-FILE3.
       01 SORT-RECORD3.
           05  SORT-VENDOR-ID3                      PIC X(4).
           05  SORT-WAREHOUSE-ID3                   PIC X(3).
           05  SORT-COSTUME-ID3                     PIC X(3).
           05  SORT-COSTUME-DATA-ARRAY3 OCCURS 6 TIMES.
               10  SORT-COSTUME-NAME3               PIC X(9).
               10  SORT-COSTUME-SIZE3               PIC A.
               10  SORT-COSTUME-TYPE3               PIC A.
               10  SORT-REORDER-POINT3              PIC S9(3).
               10  SORT-NUMBER-IN-STOCK3            PIC S9(3).
               10  SORT-PURCHASE-PRICE3             PIC S9(2)V99.

       SD MERGEFILE.
       01 MERGE-REC.
           05  MERGE-VENDOR-ID                      PIC X(4).
           05  MERGE-WAREHOUSE-ID                   PIC X(3).
           05  MERGE-COSTUME-ID                     PIC X(3).
           05  MERGE-COSTUME-DATA-ARRAY OCCURS 6 TIMES.
               10  MERGE-COSTUME-NAME               PIC X(9).
               10  MERGE-COSTUME-SIZE               PIC A.
               10  MERGE-COSTUME-TYPE               PIC A.
               10  MERGE-REORDER-POINT              PIC S9(3).
               10  MERGE-NUMBER-IN-STOCK            PIC S9(4).
               10  MERGE-PURCHASE-PRICE             PIC S9(2)V99.

       FD  DETAILED-SUMMARY-REPORT
           RECORD CONTAINS 71 CHARACTERS.
      
       01  REPORT-RECORD                   PIC X(71).

       FD  INVENTORY-ERROR-REPORT
           RECORD CONTAINS 136 CHARACTERS.
      
       01  ERROR-REPORT-RECORD             PIC X(136).





       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.

            05  FIRST-RECORD            PIC X(3)      VALUE 'YES'.
            05  EOF-FLAG                PIC X         VALUE ' '.
                88  NO-MORE-DATA                      VALUE 'N'.

        
        01  SUB  PIC 99   VALUE 1.

        01  REPORT-FIELDS.
            05  PROPER-SPACING             PIC S9   VALUE +1.
            05  WS-PAGE-NUMBER             PIC S9   VALUE +0.

        01  WS-DATE.
            05  RUN-YEAR                PIC 9(4).
            05  RUN-MONTH               PIC 99.
            05  RUN-DAY                 PIC 99.
        
        01  TEMPORARY-FIELDS.

            05  TOTAL-PRICE-TMP         PIC 999999V99.
            05  TOTAL-COSTUME-TMP       PIC 9999999V99.
            05  TOTAL-VENDOR-TMP        PIC 9999999V99.
            05  TOTAL-WAREHOUSE-TMP     PIC 99999999V99.
            05  GRAND-TOTAL-TMP         PIC 999999999V99.
            05  VENDOR-NAME-TMP         PIC X(13).
            05  VENDOR-ID-TMP           PIC X(4).
            05  WAREHOUSE-NAME-TMP      PIC X(11).
            05  WAREHOUSE-ID-TMP        PIC X(3).
            05  COSTUME-NAME-TMP        PIC X(9).
            05  ADVANCING-PAGE-TMP      PIC 9.
            05  COSTUME-BREAK           VALUE 'NO'.
            05  BAD-FILE-TMP            VALUE 'NO'.
            05  BAD-WAREHOUSE-TMP       VALUE 'NO'.
            05  BAD-VENDOR-TMP          VALUE 'NO'.
            05  VENDOR-CHECK            VALUE 'NO'.
            05  WAREHOUSE-CHECK         VALUE 'NO'.
            05  FIRST-CORRECT-ITEM      VALUE 'YES'.

        01  DETAIL-FIELDS.

            05  DF-WAREHOUSE-ID         PIC X(3).
            05  DF-VENDOR-ID            PIC X(4).
            05  DF-COSTUME-ID           PIC X(3).
            05  DF-COSTUME-NAME         PIC X(15).
            05  DF-QTY-IN-STOCK         PIC 9(4).
            05  DF-COSTUME-PRICE        PIC 999V99.
            05  DF-TOTAL-COSTUME        PIC 9999999V99.
            05  DF-TOTAL-VENDOR         PIC 9999999V99.
            05  DF-TOTAL-WAREHOUSE      PIC 99999999V99.
            05  DF-NUMBER-OF-ERRORS     PIC 999.
        
        01  TOTAL-FIELDS.

            05  TF-TOTAL-COSTUME      PIC 9999999V99.
            05  TF-TOTAL-VENDOR       PIC 9999999V99.
            05  TF-TOTAL-WAREHOUSE    PIC 99999999V99.

        01  SIZE-TEXT.
            05           PIC X          VALUE "L".
            05           PIC X          VALUE "M".
            05           PIC X          VALUE "S".
            05           PIC X          VALUE "P".

        01  SIZES-TABLE REDEFINES SIZE-TEXT.
            05  SIZES OCCURS 4 TIMES
                INDEXED BY SIZES-INDEX.
                10  SIZES-ID     PIC X.

       01  HEADING-ONE.
            
            05  FILLER                  PIC X(34)  VALUE SPACES.
            05                          PIC X(11)  VALUE 'ASHRALS LTD'.
            05  FILLER                  PIC X(26)  VALUE SPACES.

        01  HEADING-TWO.

            05  FILLER              PIC X(10)   VALUE SPACES.
            05  H1-MONTH            PIC 99/.
            05  H1-DAY              PIC 99/.
            05  H1-YEAR             PIC 9999.
            05  FILLER              PIC X(12)  VALUE SPACES.
            05                      PIC X(16)  VALUE 'INVENTORY REPORT'.
            05  FILLER              PIC X(23)   VALUE SPACES.
            05                      PIC X(3)  VALUE 'XXX'.


        01  HEADING-THREE.

            05                     PIC X(13)  VALUE '     VENDOR: '.
            05  VENDOR-HEADER      PIC X(13).
            05  FILLER             PIC X(45)  VALUE SPACES.

        01  HEADING-FOUR.

            05                     PIC X(13)  VALUE '  WAREHOUSE: '.
            05  WAREHOUSE-HEADER   PIC X(11).
            05  FILLER             PIC X(47)  VALUE SPACES.
            
        01  HEADING-FIVE.

            05  FILLER             PIC X(10)  VALUE SPACES.
            05                     PIC X(7)  VALUE 'COSTUME'.
            05  FILLER             PIC X(7)  VALUE SPACES.
            05                     PIC X(7)  VALUE 'COSTUME'.
            05  FILLER             PIC X(4)  VALUE SPACES.
            05                     PIC X(7)  VALUE 'COSTUME'.
            05  FILLER             PIC X(3)  VALUE SPACES.
            05                     PIC X(6)  VALUE 'QTY IN'.
            05  FILLER             PIC X(5)  VALUE SPACES.
            05                     PIC X(5)  VALUE 'TOTAL'.

        01  HEADING-SIX.
            
            05  FILLER             PIC X(13)  VALUE SPACES.
            05                     PIC X(4)   VALUE 'NAME'.
            05  FILLER             PIC X(9)  VALUE SPACES.
            05                     PIC X(4)   VALUE 'SIZE'.
            05  FILLER             PIC X(7)   VALUE SPACES.
            05                     PIC X(4)   VALUE 'TYPE'.
            05  FILLER             PIC X(5)   VALUE SPACES.
            05                     PIC X(5)   VALUE 'STOCK'.
            05  FILLER             PIC X(6)   VALUE SPACES.
            05                     PIC X(4)  VALUE 'COST'.

        01  DETAIL-LINE.

            05  FILLER             PIC X(10)  VALUE SPACES.
            05  DL-COSTUME-NAME    PIC X(9).
            05  FILLER             PIC X(5)  VALUE SPACES.
            05  DL-COSTUME-SIZE    PIC X(8).  
            05  FILLER             PIC X(5)  VALUE SPACES.
            05  DL-COSTUME-TYPE    PIC X(5).
            05  FILLER             PIC X(5)  VALUE SPACES.
            05  DL-QTY-IN-STOCK    PIC ZZ9.
            05  FILLER             PIC X(4)  VALUE SPACES.
            05  DL-TOTAL-COST      PIC $ZZ,ZZ9.99.              

        01  COSTUME-TOTAL-LINE.
          
            05  FILLER             PIC X(43)  VALUE SPACES.
            05                     PIC X(10)  VALUE 'TOTAL:    '.
            05  CTL-TOTAL-COST     PIC $ZZZ,ZZ9.99.

        01  WAREHOUSE-TOTAL-LINE.

            05  FILLER            PIC X(14)  VALUE SPACES. 
            05                PIC X(22)  VALUE 'TOTAL FOR WAREHOUSE:  '.
            05  WTL-WAREHOUSE-NAME  PIC X(11).
            05  FILLER            PIC X(4)  VALUE SPACES. 
            05  WTL-TOTAL-COST    PIC $Z,ZZZ,ZZ9.99.

        01  VENDOR-TOTAL-LINE.

            05  FILLER            PIC X(17)  VALUE SPACES.
            05                   PIC X(19)  VALUE 'TOTAL FOR VENDOR:  '.
            05  VTL-VENDOR-NAME   PIC X(13).
            05  FILLER            PIC X(1)  VALUE SPACES.
            05  VTL-TOTAL-COST    PIC $ZZ,ZZZ,ZZ9.99.
        
        01  GRAND-TOTAL-LINE.
 
            05  FILLER           PIC X(29)  VALUE SPACES. 
            05                   PIC X(17)  VALUE 'GRAND TOTAL COST:'.
            05  FILLER           PIC X(3)  VALUE SPACES.             
            05  GTL-TOTAL-COST   PIC $ZZZ,ZZZ,ZZZ.99.

        
       PROCEDURE DIVISION.
       
       100-CONTROL-MODULE.

           PERFORM 150-SORTMERGE-INVENTORY-FILE
           PERFORM 200-HOUSEKEEPING-ROUTINE
           PERFORM 300-PROCESS-MERGED-DATA
           PERFORM 375-OPEN-CORRECT-INVENTORY
           PERFORM 450-PROCESS-CORRECT-DATA
           PERFORM 1500-END-OF-FILE-ROUTINE

           .
       
       150-SORTMERGE-INVENTORY-FILE.

      *CODE YOUR SORT AND MERGE ROUTINES HERE

           SORT SORT-FILE1
              ON ASCENDING KEY SORT-VENDOR-ID1
              ON ASCENDING KEY SORT-WAREHOUSE-ID1
              ON ASCENDING KEY SORT-COSTUME-ID1
              USING UNSORTED-FILE1
              GIVING INVENTORY-FILE1

           SORT SORT-FILE2
              ON ASCENDING KEY SORT-VENDOR-ID2
              ON ASCENDING KEY SORT-WAREHOUSE-ID2
              ON ASCENDING KEY SORT-COSTUME-ID2
              USING UNSORTED-FILE2
              GIVING INVENTORY-FILE2
        
           SORT SORT-FILE3
              ON ASCENDING KEY SORT-VENDOR-ID3
              ON ASCENDING KEY SORT-WAREHOUSE-ID3
              ON ASCENDING KEY SORT-COSTUME-ID3
              USING UNSORTED-FILE3
              GIVING INVENTORY-FILE3

           MERGE MERGEFILE
              ON ASCENDING KEY MERGE-VENDOR-ID
              ON ASCENDING KEY MERGE-WAREHOUSE-ID
              ON ASCENDING KEY MERGE-COSTUME-ID
              USING INVENTORY-FILE1, INVENTORY-FILE2, INVENTORY-FILE3
              GIVING MERGED-INVENTORY-FILE

       .

       200-HOUSEKEEPING-ROUTINE.
    
           OPEN INPUT  MERGED-INVENTORY-FILE
                       
                OUTPUT 
                       INVENTORY-ERROR-REPORT
                       CORRECT-INVENTORY-REPORT 
                       DETAILED-SUMMARY-REPORT 
                        
           
           ACCEPT WS-DATE FROM DATE YYYYMMDD 
           MOVE WS-DATE(1:4) TO H1-YEAR
           MOVE WS-DATE(5:2) TO H1-MONTH
           MOVE WS-DATE(7:2) TO H1-DAY


           PERFORM 400-HEADER-ROUTINE
        
           
       .
       
       300-PROCESS-MERGED-DATA.

           PERFORM UNTIL NO-MORE-DATA
               READ MERGED-INVENTORY-FILE
                  AT END
                     MOVE 'N' TO EOF-FLAG
                  NOT AT END
                    PERFORM 350-VALIDATION-ROUTINE
                        
                END-READ
            END-PERFORM

            MOVE ' ' TO EOF-FLAG


            .
       350-VALIDATION-ROUTINE.
           
                        EVALUATE TRUE 
                        
                           
                          WHEN MI-VENDOR-ID ="LA10" OR 
                           MI-VENDOR-ID ="CH20" OR 
                           MI-VENDOR-ID ="NY30"
                              MOVE "YES" TO VENDOR-CHECK

                              

                           WHEN OTHER
                              MOVE MERGED-INVENTORY-REC TO 
                                ERROR-REPORT-RECORD  
                              WRITE ERROR-REPORT-RECORD 
                                AFTER ADVANCING 1 LINES
                              MOVE "NO" TO VENDOR-CHECK
                              


                         END-EVALUATE

                       EVALUATE TRUE  
                         
                         WHEN MI-WAREHOUSE-ID = "BHM" OR 
                         MI-WAREHOUSE-ID = "HUN"
                            MOVE "YES" TO WAREHOUSE-CHECK

                         WHEN OTHER
                           MOVE MERGED-INVENTORY-REC TO 
                              ERROR-REPORT-RECORD  
                           WRITE ERROR-REPORT-RECORD 
                              AFTER ADVANCING 1 LINES
                           MOVE "NO" TO WAREHOUSE-CHECK
                           
                         

                        END-EVALUATE
                        
                        EVALUATE TRUE 
                           
                           WHEN VENDOR-CHECK = WAREHOUSE-CHECK 
                              MOVE MERGED-INVENTORY-REC TO 
                                CORRECT-INVENTORY-FILE  
                              IF FIRST-CORRECT-ITEM = "YES" 
                                 WRITE CORRECT-INVENTORY-FILE 
                                  BEFORE ADVANCING PAGE
                                 MOVE "NO" TO FIRST-CORRECT-ITEM
                              ELSE  
                                 WRITE CORRECT-INVENTORY-FILE
                                  AFTER ADVANCING 1 LINES
                             
                        END-EVALUATE  


                        
        .
       
       375-OPEN-CORRECT-INVENTORY.
           
           CLOSE 
                 CORRECT-INVENTORY-REPORT 

           OPEN INPUT CORRECT-INVENTORY-REPORT
                
        .
       400-HEADER-ROUTINE.
           
           IF FIRST-RECORD = "YES"
              WRITE REPORT-RECORD FROM HEADING-ONE 
                 AFTER ADVANCING 1 LINES
          
             
           
           END-IF 
           
           MOVE 1 TO PROPER-SPACING
           MOVE HEADING-TWO TO REPORT-RECORD 
           PERFORM 600-WRITE-A-LINE
           MOVE 2 TO PROPER-SPACING
           .
  
       450-PROCESS-CORRECT-DATA.
           
           PERFORM UNTIL NO-MORE-DATA  
               READ CORRECT-INVENTORY-REPORT
                  AT END
                     MOVE 'N' TO EOF-FLAG
                  NOT AT END
                    PERFORM 500-DATA-INPUT-ROUTINE 
                END-READ
            END-PERFORM

            
            .
       500-DATA-INPUT-ROUTINE.
           
           EVALUATE TRUE
                   WHEN FIRST-RECORD = 'YES'
                       MOVE 'NO' TO FIRST-RECORD
                       MOVE CI-WAREHOUSE-ID TO DF-WAREHOUSE-ID
                       MOVE CI-VENDOR-ID TO DF-VENDOR-ID  
                       
                    EVALUATE TRUE
                       WHEN CI-VENDOR-ID = "LA10" 
                           MOVE "LOS ANGELES" TO VENDOR-NAME-TMP 
                       WHEN CI-VENDOR-ID = "CH20" 
                           MOVE "CHICAGO" TO VENDOR-NAME-TMP 
                       WHEN CI-VENDOR-ID = "NY30" 
                           MOVE "NEW YORK" TO VENDOR-NAME-TMP 
                    END-EVALUATE

                    EVALUATE TRUE
                       WHEN CI-WAREHOUSE-ID = "BHM" 
                           MOVE "BIRMINGHAM" TO WAREHOUSE-NAME-TMP 
                       WHEN CI-WAREHOUSE-ID = "HUN" 
                           MOVE "HUNTSVILLE" TO WAREHOUSE-NAME-TMP 

                    END-EVALUATE

     
                       

                   PERFORM 700-PRINT-VENDOR-HEADER
                   PERFORM 900-PRINT-WAREHOUSE-HEADER
                   PERFORM 1050-PRINT-COSTUME-HEADER
                   MOVE WAREHOUSE-NAME-TMP TO WTL-WAREHOUSE-NAME 
                       
                       
                   WHEN CI-VENDOR-ID NOT = DF-VENDOR-ID 
                        MOVE 2 TO ADVANCING-PAGE-TMP
                        PERFORM 800-VENDOR-BREAK

                        PERFORM 700-PRINT-VENDOR-HEADER
                        PERFORM 900-PRINT-WAREHOUSE-HEADER
                        PERFORM 1050-PRINT-COSTUME-HEADER
                           
                    WHEN CI-WAREHOUSE-ID NOT = DF-WAREHOUSE-ID
                        PERFORM 1000-WAREHOUSE-BREAK

                        PERFORM 900-PRINT-WAREHOUSE-HEADER
                        PERFORM 1050-PRINT-COSTUME-HEADER

                   WHEN CI-COSTUME-ID NOT = DF-COSTUME-ID
                        PERFORM 1100-COSTUME-BREAK
                        PERFORM 1050-PRINT-COSTUME-HEADER
             
            END-EVALUATE     


           
           PERFORM VARYING SUB 
                  FROM 1 BY 1 UNTIL SUB > 6
               
                MOVE CI-COSTUME-NAME(SUB) TO COSTUME-NAME-TMP
                MOVE CI-COSTUME-TYPE(SUB) TO DL-COSTUME-TYPE
                MOVE CI-QTY-IN-STOCK(SUB) TO DL-QTY-IN-STOCK
               
                IF SUB = 1 
                    MOVE COSTUME-NAME-TMP TO DL-COSTUME-NAME
                ELSE
                    MOVE SPACES TO DL-COSTUME-NAME 

                END-IF   

            SET SIZES-INDEX TO 1
                       SEARCH SIZES 
                           AT END MOVE "BAD--" TO DL-COSTUME-SIZE

                            WHEN SIZES-ID(SIZES-INDEX) = 
                            CI-COSTUME-SIZE(SUB)
                               EVALUATE TRUE
                                  WHEN SIZES-ID(SIZES-INDEX) = "L"
                                     MOVE "LARGE" TO DL-COSTUME-SIZE
                                  WHEN SIZES-ID(SIZES-INDEX) = "M"
                                     MOVE "MEDIUM" TO DL-COSTUME-SIZE
                                  WHEN SIZES-ID(SIZES-INDEX) = "S"
                                     MOVE "SMALL" TO DL-COSTUME-SIZE
                                  WHEN SIZES-ID(SIZES-INDEX) = "P"
                                     MOVE "PLUS" TO DL-COSTUME-SIZE
                              END-EVALUATE  

                        END-SEARCH
                        
            EVALUATE TRUE
                  WHEN CI-COSTUME-TYPE(SUB) = "A"
                      MOVE "Adult" TO DL-COSTUME-TYPE
                  WHEN CI-COSTUME-TYPE(SUB) = "C"
                      MOVE "Child" TO DL-COSTUME-TYPE

                  WHEN OTHER
                      STRING 
                         "BAD- " DELIMITED BY " " 
                         " " DELIMITED BY SIZE
                         CI-COSTUME-SIZE(SUB) DELIMITED BY " "
                         INTO DL-COSTUME-SIZE

                END-EVALUATE

                
               
            
   

                IF CI-QTY-IN-STOCK(SUB) IS NUMERIC
                   MOVE CI-QTY-IN-STOCK(SUB) TO DL-QTY-IN-STOCK,
                   DF-QTY-IN-STOCK 
                
                ELSE 
                   MOVE '000' TO DF-QTY-IN-STOCK,DL-QTY-IN-STOCK

                   
               END-IF

               IF CI-COSTUME-PRICE(SUB) IS NUMERIC
                   MOVE CI-COSTUME-PRICE(SUB) TO DF-COSTUME-PRICE       
               ELSE 
                   MOVE ZEROES TO DF-COSTUME-PRICE
                   
               END-IF
                


                MULTIPLY DF-QTY-IN-STOCK BY DF-COSTUME-PRICE
                GIVING DL-TOTAL-COST, TOTAL-PRICE-TMP

                ADD TOTAL-PRICE-TMP TO TF-TOTAL-WAREHOUSE,
                TF-TOTAL-VENDOR, TF-TOTAL-COSTUME, GRAND-TOTAL-TMP


               MOVE DETAIL-LINE TO REPORT-RECORD
               PERFORM 600-WRITE-A-LINE
           




          END-PERFORM
           
        .
       600-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING 2 LINES
  

        .
       700-PRINT-VENDOR-HEADER.
           
           MOVE VENDOR-NAME-TMP  TO VENDOR-HEADER
           WRITE REPORT-RECORD FROM HEADING-THREE
               AFTER ADVANCING 2 LINES

       .

       800-VENDOR-BREAK.
           
           MOVE TF-TOTAL-VENDOR TO VTL-TOTAL-COST

           MOVE 2 TO PROPER-SPACING
           PERFORM 1000-WAREHOUSE-BREAK
           MOVE VENDOR-NAME-TMP TO VTL-VENDOR-NAME
           MOVE VENDOR-TOTAL-LINE TO REPORT-RECORD
           PERFORM 600-WRITE-A-LINE
           MOVE "YES" TO FIRST-RECORD
           
           EVALUATE TRUE
               WHEN CI-VENDOR-ID = "LA10" 
                    MOVE "LOS ANGELES" TO VENDOR-NAME-TMP 
               WHEN CI-VENDOR-ID = "CH20" 
                    MOVE "CHICAGO" TO VENDOR-NAME-TMP 
               WHEN CI-VENDOR-ID = "NY30" 
                    MOVE "NEW YORK" TO VENDOR-NAME-TMP
           END-EVALUATE 


           EVALUATE TRUE 
               

               WHEN CI-VENDOR-ID ="LA10" OR 
                 CI-VENDOR-ID ="CH20" OR 
                 CI-VENDOR-ID ="NY30"
                    MOVE "YES" TO VENDOR-CHECK 
                
               WHEN OTHER
                   MOVE MERGED-INVENTORY-REC TO 
                    ERROR-REPORT-RECORD  
                   WRITE ERROR-REPORT-RECORD 
                    AFTER ADVANCING 1 LINES
                   MOVE "NO" TO VENDOR-CHECK        
                    

           END-EVALUATE
           
          
           MOVE CI-VENDOR-ID TO DF-VENDOR-ID  
           MOVE ZEROS TO TF-TOTAL-VENDOR
           MOVE 3 TO PROPER-SPACING
           PERFORM 400-HEADER-ROUTINE
           

          
           .

       900-PRINT-WAREHOUSE-HEADER.

           MOVE WAREHOUSE-NAME-TMP TO WAREHOUSE-HEADER
           WRITE REPORT-RECORD FROM HEADING-FOUR
               AFTER ADVANCING 2 LINES

       .

       1000-WAREHOUSE-BREAK.

           MOVE TF-TOTAL-WAREHOUSE TO WTL-TOTAL-COST
           
           
           EVALUATE TRUE

              WHEN CI-WAREHOUSE-ID = "BHM" OR CI-WAREHOUSE-ID = "HUN"
                 MOVE "YES" TO WAREHOUSE-CHECK
              WHEN OTHER
                MOVE MERGED-INVENTORY-REC TO ERROR-REPORT-RECORD  
                WRITE ERROR-REPORT-RECORD 
                  AFTER ADVANCING 1 LINES
                MOVE "NO" TO WAREHOUSE-CHECK
           END-EVALUATE


           
           MOVE 2 TO PROPER-SPACING
           PERFORM 1100-COSTUME-BREAK
           MOVE WAREHOUSE-TOTAL-LINE TO REPORT-RECORD
           PERFORM 600-WRITE-A-LINE

           EVALUATE TRUE
              WHEN CI-WAREHOUSE-ID = "BHM" 
                 MOVE "BIRMINGHAM" TO WAREHOUSE-NAME-TMP 
              WHEN CI-WAREHOUSE-ID = "HUN" 
                 MOVE "HUNTSVILLE" TO WAREHOUSE-NAME-TMP 
           END-EVALUATE

           MOVE WAREHOUSE-NAME-TMP TO WTL-WAREHOUSE-NAME
           MOVE ZEROS TO TF-TOTAL-WAREHOUSE
           MOVE MI-WAREHOUSE-ID  TO DF-WAREHOUSE-ID  
           

          .
       
       1050-PRINT-COSTUME-HEADER.
            
           WRITE REPORT-RECORD FROM HEADING-FIVE
               AFTER ADVANCING 3 LINES
           WRITE REPORT-RECORD FROM HEADING-SIX
               AFTER ADVANCING 1 LINES

       .
       1100-COSTUME-BREAK.

           MOVE TF-TOTAL-COSTUME TO CTL-TOTAL-COST
           

           MOVE 2 TO PROPER-SPACING
           MOVE COSTUME-TOTAL-LINE TO REPORT-RECORD
           PERFORM 600-WRITE-A-LINE

           MOVE MI-COSTUME-ID TO DF-COSTUME-ID
           MOVE ZEROS TO TF-TOTAL-COSTUME
           
          .

       1200-GRAND-TOTAL-BREAK.

           MOVE 0 TO WS-PAGE-NUMBER
           MOVE VENDOR-HEADER   TO VTL-VENDOR-NAME 
           MOVE TF-TOTAL-VENDOR TO VTL-TOTAL-COST

           MOVE 2 TO PROPER-SPACING
           PERFORM 1000-WAREHOUSE-BREAK
           MOVE VENDOR-TOTAL-LINE TO REPORT-RECORD
           PERFORM 600-WRITE-A-LINE
         
           MOVE ZEROS TO TF-TOTAL-VENDOR 
           

       .

       1300-END-OF-JOB-ROUTINE.

           PERFORM 1200-GRAND-TOTAL-BREAK
           PERFORM 1400-GRAND-TOTAL-ROUTINE

          .

       1400-GRAND-TOTAL-ROUTINE.
          
           MOVE GRAND-TOTAL-TMP TO GTL-TOTAL-COST
           WRITE REPORT-RECORD FROM GRAND-TOTAL-LINE
               AFTER ADVANCING 2 LINES
       .


       1500-END-OF-FILE-ROUTINE.

           PERFORM 1300-END-OF-JOB-ROUTINE

           CLOSE MERGED-INVENTORY-FILE
               DETAILED-SUMMARY-REPORT
               INVENTORY-ERROR-REPORT
               CORRECT-INVENTORY-REPORT 
           STOP RUN
           .
