       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CALURegistration.
       AUTHOR.        Dusty Williams.
       DATE-WRITTEN.  04/17/2018.
      *-----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
       
       SELECT student-file
           ASSIGN TO "studentC.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
          
       SELECT course-file
           ASSIGN TO "courseC.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT registration-file
           ASSIGN TO "registerC.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       SELECT report-file
           ASSIGN TO "report.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       
       DATA DIVISION.
       
       FILE SECTION.
       
       FD student-file
           LABEL RECORDS ARE OMITTED.
       
       01 student-record.
           05 StudentNumberIn      PIC 99999.
           05 FILLER               PIC X VALUE SPACES.
           05 StudentLastNameIn    PIC X(10).
           05 FILLER               PIC X VALUE SPACES.
           05 StudentFirstNameIn   PIC X(11).
           05 FILLER               PIC X VALUE SPACES.
           05 StudentMajorIn       PIC X(7).
           05 FILLER               PIC X VALUE SPACES.
           05 StudentGPAIn         PIC 9v99.

       FD course-file
           LABEL RECORDS ARE OMITTED.
           
       01 course-record.
           05 CourseNumberIn       PIC X(6).
           05 FILLER               PIC X VALUE SPACES.
           05 CourseNameIn         PIC X(15).
           05 FILLER               PIC X VALUE SPACES.
           05 CourseDaysIn         PIC X(3).
           05 FILLER               PIC X VALUE SPACES.
           05 CourseTimeIn         PIC X(7).
           05 FILLER               PIC X VALUE SPACES.
           05 ProfLastNameIn       PIC X(10).
           
       FD registration-file
           LABEL RECORDS ARE OMITTED.
           
       01 registration-record.
           05 RegStuNumIn          PIC 99999.
           05 FILLER               PIC X VALUE SPACES.
           05 RegCourNumIn         PIC X(6).
       
       FD report-file
           LABEL RECORDS ARE OMITTED.
       
       01 report-record.
           05 FILLER               PIC X(80).  
       
       WORKING-STORAGE SECTION.
       
       77 EOF                      PIC x(3).
              
       01 input-data.
           05 MenuChoice           PIC 99.
           05 ReportMenuChoice     PIC 99.
           05 MajorChoice          PIC X(25).
           05 ProfChoice           PIC X(25).
       
       01 student-tables.
           05 StudentNumber        PIC 99999 OCCURS 100 TIMES.
           05 StudentLastName      PIC X(10) OCCURS 100 TIMES.
           05 StudentFirstName     PIC X(11) OCCURS 100 TIMES.
           05 StudentMajor         PIC X(7)  OCCURS 100 TIMES.
           05 StudentGPA           PIC 9v99  OCCURS 100 TIMES.
           
       01 course-tables.
           05 CourseNumber         PIC X(6)  OCCURS 100 TIMES.
           05 CourseName           PIC X(15) OCCURS 100 TIMES.
           05 CourseDays           PIC X(3)  OCCURS 100 TIMES.
           05 CourseTime           PIC X(7)  OCCURS 100 TIMES.
           05 ProfLastName         PIC X(10) OCCURS 100 TIMES.

       01 registration-tables.
           05 RegStuNum            PIC 99999 OCCURS 100 TIMES.
           05 RegCourNum           PIC X(6)  OCCURS 100 TIMES.
       
       01 iterators.
           05 StudentCount         PIC 999 VALUE 0.
           05 CourseCount          PIC 999 VALUE 0.
           05 RegCount             PIC 999 VALUE 0.
           05 I                    PIC 999 VALUE 0.
   
       01 student-record-heading.
           05 FILLER               PIC X(2) VALUE "ID".
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE "LNAME".
           05 FILLER               PIC X(6) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE "FNAME".
           05 FILLER               PIC X(7) VALUE SPACES.               
           05 FILLER               PIC X(5) VALUE "MAJOR".
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE "GPA".

       01 student-record-disp.
           05 StudentNumberDisp    PIC 99999.
           05 FILLER               PIC X VALUE SPACES.
           05 StudentLastNameDisp  PIC X(10).
           05 FILLER               PIC X VALUE SPACES.
           05 StudentFirstNameDisp PIC X(11).
           05 FILLER               PIC X VALUE SPACES.
           05 StudentMajorDisp     PIC X(7).
           05 FILLER               PIC X VALUE SPACES.
           05 StudentGPADisp       PIC 9.99.
       
       01 course-record-heading.
           05 FILLER               PIC X(2)  VALUE "ID".
           05 FILLER               PIC X(5)  VALUE SPACES.
           05 FILLER               PIC X(4)  VALUE "NAME".
           05 FILLER               PIC X(12) VALUE SPACES.
           05 FILLER               PIC X(3)  VALUE "DAY".
           05 FILLER               PIC X(1)  VALUE SPACES.              
           05 FILLER               PIC X(4)  VALUE "TIME".
           05 FILLER               PIC X(4)  VALUE SPACES.
           05 FILLER               PIC X(6)  VALUE "PROFLN".
       
       01 course-record-disp.
           05 CourseNumberDisp     PIC X(6).
           05 FILLER               PIC X VALUE SPACES.
           05 CourseNameDisp       PIC X(15).
           05 FILLER               PIC X VALUE SPACES.
           05 CourseDaysDisp       PIC X(3).
           05 FILLER               PIC X VALUE SPACES.
           05 CourseTimeDisp       PIC X(7).
           05 FILLER               PIC X VALUE SPACES.
           05 ProfLastNameDisp     PIC X(10).
       
       01 registration-record-heading.
           05 FILLER               PIC X(5) VALUE "STUID".
           05 FILLER               PIC X VALUE SPACES.
           05 FILLER               PIC X(6) VALUE "COURID".
       
       01 registration-record-disp.
           05 RegStuNumDisp        PIC 99999.
           05 FILLER               PIC X(1) VALUE SPACES.
           05 RegCourNumDisp       PIC X(6).

       01 report-header.
           05 FILLER               PIC X(9) VALUE "---------".
           05 FILLER               PIC X VALUE SPACES.
           05 ReportSectionTitle   PIC X(20).
           05 FILLER               PIC X VALUE SPACES.
           05 FILLER               PIC X(9) VALUE "---------".
           
       01 report-searchterms.
           05 FILLER               PIC X(13) VALUE "Searched for:".
           05 FILLER               PIC X VALUE SPACES.
           05 ReportSearchTerm     PIC X(50).

      *-----------------------------------------------------------------
       
       PROCEDURE DIVISION.
       
       MAIN-PROGRAM.
           
           PERFORM Read_student.
           PERFORM Read_course.
           PERFORM Read_register. 
           OPEN OUTPUT report-file.
           
           PERFORM Menu UNTIL MenuChoice IS EQUAL 5.

           STOP RUN.
      
      *
      * Display main menu, accept user input
      *
       Menu.
       
           DISPLAY " ".
           DISPLAY "    CALU Registration System".
           DISPLAY " ".
           DISPLAY "1.  Add/Modify Student Information".
           DISPLAY "2.  Add/Modify Course Information".
           DISPLAY "3.  Register Students in Courses".
           DISPLAY "4.  Report Section".
           DISPLAY "5.  Exit CALU System".
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
           ACCEPT MenuChoice.
           
           PERFORM Process_menu_selection.
      
      *
      * Perform paragraph based on menu choice
      *
       Process_menu_selection.
       
           IF      MenuChoice = 1 PERFORM Modify_student
           ELSE IF MenuChoice = 2 PERFORM Modify_course
           ELSE IF MenuChoice = 3 PERFORM Modify_register
           ELSE IF MenuChoice = 4 PERFORM Report_section
           ELSE IF MenuChoice = 5 PERFORM End_program
           END-IF.
      
      * 
      * Read student data from file
      *
       Read_student.

           DISPLAY "Reading student file... "
               WITH NO ADVANCING.
               
           MOVE "no" TO EOF.
           MOVE 0 TO StudentCount.
           
           OPEN INPUT student-file.
           READ student-file
               AT END MOVE "yes" TO EOF.
               
           PERFORM Process_student 
               UNTIL EOF IS EQUAL "yes".
           
           CLOSE student-file.
           DISPLAY "Done.".
       
      *
      * Pull student data into tables
      *
       Process_student.

           ADD StudentCount 1 GIVING StudentCount.
           
           MOVE StudentNumberIn    TO StudentNumber(StudentCount).
           MOVE StudentLastNameIn  TO StudentLastName(StudentCount).
           MOVE StudentFirstNameIn TO StudentFirstName(StudentCount).
           MOVE StudentMajorIn     TO StudentMajor(StudentCount).
           MOVE StudentGPAIn       TO StudentGPA(StudentCount).

           READ student-file
               AT END MOVE "yes" TO EOF.
       
      *
      * Read course data from file
      *
       Read_course. 

           DISPLAY "Reading course file... "
               WITH NO ADVANCING.
               
           MOVE "no" TO EOF.
           MOVE 0 TO CourseCount.
           
           OPEN INPUT course-file.
           READ course-file
               AT END MOVE "yes" TO EOF.
               
           PERFORM Process_course 
               UNTIL EOF IS EQUAL "yes".
               
           CLOSE course-file.
           DISPLAY "Done.".
      
      *
      * Pull course data into tables
      *
       Process_course.
       
           ADD CourseCount 1 GIVING CourseCount.
       
           MOVE CourseNumberIn TO CourseNumber(CourseCount).
           MOVE CourseNameIn   TO CourseName(CourseCount).
           MOVE CourseDaysIn   TO CourseDays(CourseCount).
           MOVE CourseTimeIn   TO CourseTime(CourseCount).
           MOVE ProfLastNameIn TO ProfLastName(CourseCount).
           
           READ course-file
               AT END MOVE "yes" TO EOF.
      
      *
      * Read registration data from file
      *
       Read_register.
       
           DISPLAY "Reading registration file... "
               WITH NO ADVANCING.
               
           MOVE "no" TO EOF.
           MOVE 0 TO RegCount.
           
           OPEN INPUT registration-file.
           READ registration-file
               AT END MOVE "yes" TO EOF.
               
           PERFORM Process_register 
               UNTIL EOF IS EQUAL "yes".
           
           CLOSE registration-file.
           DISPLAY "Done.".
      
      *
      * Pull registration data into tables
      *
       Process_register.
       
           ADD RegCount 1 GIVING RegCount.
       
           MOVE RegStuNumIn  TO RegStuNum(RegCount).
           MOVE RegCourNumIn TO RegCourNum(RegCount).
       
           READ registration-file
               AT END MOVE "yes" TO EOF.

       Modify_student.
           
           DISPLAY " ".
           DISPLAY "This is the modify student stub".
                      
       Modify_course.
           
           DISPLAY " ".
           DISPLAY "This is the modify course stub".
           
       Modify_register.
       
           DISPLAY " ".
           DISPLAY "This is the modify register stub".

      *
      * Display reporting menu, accept user input
      *
       Report_menu.
           
           DISPLAY " ".
           DISPLAY "    CALU Report Menu".
           
           DISPLAY " ".
           DISPLAY " 1.  Student Master List".
           DISPLAY " 2.  Course Master List".
           DISPLAY " 3.  Register Master List".
           DISPLAY " 4.  List of Students by Major".
           DISPLAY " 5.  List of Courses by Professor".
           DISPLAY " 6.  Honor Students".
           DISPLAY " 7.  Student Schedule For One Student".
           DISPLAY " 8.  Course Roster For One Course".
           DISPLAY " 9.  Course Roster For All Courses".
           DISPLAY "10.  Report 10".
           DISPLAY "11.  Exit Report Menu".
           
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
               
           ACCEPT ReportMenuChoice.
           
           PERFORM Process_Report_Menu_Selection.
      
      *
      * Perform paragraph based on menu choice
      *
       Process_Report_Menu_Selection.

           IF      ReportMenuChoice = 1  PERFORM Student_master_list
           ELSE IF ReportMenuChoice = 2  PERFORM Course_master_list
           ELSE IF ReportMenuChoice = 3  PERFORM Register_master_list
           ELSE IF ReportMenuChoice = 4  PERFORM Students_by_major
           ELSE IF ReportMenuChoice = 5  PERFORM Courses_by_professor
           ELSE IF ReportMenuChoice = 6  PERFORM Honor_students
           ELSE IF ReportMenuChoice = 7  PERFORM Student_schedule
           ELSE IF ReportMenuChoice = 8  PERFORM Course_roster
           ELSE IF ReportMenuChoice = 9  PERFORM Multi_course_roster
           ELSE IF ReportMenuChoice = 10 PERFORM Report_10
           END-IF.

      *
      * Run the report sub-section paragraphs
      *
       Report_section.

           PERFORM Report_menu UNTIL ReportMenuChoice IS EQUAL 11.
           MOVE 0 TO ReportMenuChoice.
       
      *
      * Display the entire list of students
      * Write to report file
      *
       Student_master_list.

           MOVE "Student Master List" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.
           
           DISPLAY student-record-heading.
           WRITE report-record FROM student-record-heading.
           
           PERFORM Display_student_table_line
               VARYING I FROM 1 BY 1
               UNTIL I > StudentCount.
           
      *
      * Display a record from the student table indexed by I
      * Write to report file
      *
       Display_student_table_line.

           MOVE StudentNumber(I)    TO StudentNumberDisp.
           MOVE StudentLastName(I)  TO StudentLastNameDisp.
           MOVE StudentFirstName(I) TO StudentFirstNameDisp.
           MOVE StudentMajor(I)     TO StudentMajorDisp.
           MOVE StudentGPA(I)       TO StudentGPADisp.
           
           DISPLAY student-record-disp.
           WRITE report-record FROM student-record-disp.
      
      *
      * Display the entire list of courses
      * Write to report file
      *
       Course_master_list.

           MOVE "Course Master List" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.
           
           DISPLAY course-record-heading.
           WRITE report-record FROM course-record-heading.
           
           PERFORM Display_course_table_line
               VARYING I FROM 1 BY 1
               UNTIL I > CourseCount.
       
      *
      * Display a record from the course table indexed by I
      * Write to report file
      *
       Display_course_table_line.
       
           MOVE CourseNumber(I) TO CourseNumberDisp.
           MOVE CourseName(I)   TO CourseNameDisp.
           MOVE CourseDays(I)   TO CourseDaysDisp.
           MOVE CourseTime(I)   TO CourseTimeDisp.
           MOVE ProfLastName(I) TO ProfLastNameDisp.
       
           DISPLAY course-record-disp.
           WRITE report-record FROM course-record-disp.
       
      *
      * Display the entire list of registrations
      * Write to report file
      *
       Register_master_list.

           MOVE "Reg Master List" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.
           
           DISPLAY registration-record-heading.
           WRITE report-record FROM registration-record-heading.
           
           PERFORM Display_register_table_line
               VARYING I FROM 1 BY 1
               UNTIL I > RegCount.
       
      *
      * Display a record from the registration table indexed by I
      * Write to report file
      *
       Display_register_table_line. 

           MOVE RegStuNum(I)  TO RegStuNumDisp.
           MOVE RegCourNum(I) TO RegCourNumDisp.
           
           DISPLAY registration-record-disp.
           WRITE report-record FROM registration-record-disp.

      *
      * Accept a major to search student table for
      * Write to report file
      *
       Students_by_major.
       
           MOVE "Students By Major" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.

           DISPLAY "Please enter a major to search for: "
               WITH NO ADVANCING.
               
           ACCEPT MajorChoice.
           
           MOVE MajorChoice TO ReportSearchTerm.
           WRITE report-record FROM report-searchterms.
           
           DISPLAY student-record-heading.
           WRITE report-record FROM student-record-heading.
           
           PERFORM Search_for_major
               VARYING I FROM 1 BY 1
               UNTIL I > StudentCount.
      
      *
      * Display a major if it matches selected major
      * Write to report file
      *
       Search_for_major.
           
           IF StudentMajor(I) IS EQUAL MajorChoice
               MOVE StudentNumber(I)    TO StudentNumberDisp
               MOVE StudentLastName(I)  TO StudentLastNameDisp
               MOVE StudentFirstName(I) TO StudentFirstNameDisp
               MOVE StudentMajor(I)     TO StudentMajorDisp
               MOVE StudentGPA(I)       TO StudentGPADisp
               
               DISPLAY student-record-disp
               WRITE report-record FROM student-record-disp
           END-IF.
      
      *
      * Accept a professor to search courses table for
      * Write to report file
      *
       Courses_by_professor.
      
           MOVE "Courses By Professor" TO ReportSectionTitle.           
           DISPLAY report-header.
           WRITE report-record FROM report-header.

           DISPLAY "Please enter a professor to search for: "
               WITH NO ADVANCING.
               
           ACCEPT ProfChoice.
           
           MOVE ProfChoice TO ReportSearchTerm.
           WRITE report-record FROM report-searchterms.

           DISPLAY course-record-heading.
           WRITE report-record FROM course-record-heading.
           
           PERFORM Search_for_professor
               VARYING I FROM 1 BY 1
               UNTIL I > CourseCount.
      
      *
      * Display a course if it matches selected professor
      * Write to report file
      *
       Search_for_professor.
       
           IF ProfLastName(I) IS EQUAL ProfChoice
               MOVE CourseNumber(I) TO CourseNumberDisp
               MOVE CourseName(I)   TO CourseNameDisp
               MOVE CourseDays(I)   TO CourseDaysDisp
               MOVE CourseTime(I)   TO CourseTimeDisp
               MOVE ProfLastName(I) TO ProfLastNameDisp
               
               DISPLAY course-record-disp
               WRITE report-record FROM course-record-disp
           END-IF.
           
       Honor_students.
       
           DISPLAY "Honor_students".
       
       Student_schedule.
       
           DISPLAY "Student_schedule".
       
       Course_roster.
       
           DISPLAY "Course_roster".
       
       Multi_course_roster.
       
           DISPLAY "Multi_course_roster".
       
       Report_10.
       
           DISPLAY "Report_10".
       
       End_program.
       
           DISPLAY " ".
           DISPLAY "This is the end program stub".
           
       END PROGRAM CALURegistration.