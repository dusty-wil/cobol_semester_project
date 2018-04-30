       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CALURegistration.
       AUTHOR.        Dusty Williams.
       DATE-WRITTEN.  04/17/2018.
      
      *=================================================================
       
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
      
      *=================================================================
       
       DATA DIVISION.
       
       FILE SECTION.
       
       FD student-file
           LABEL RECORDS ARE OMITTED.
       
       01 student-record.
           05 StudentNumberIn          PIC 99999.
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentLastNameIn        PIC X(10).
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentFirstNameIn       PIC X(11).
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentMajorIn           PIC X(7).
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentGPAIn             PIC 9v99.

       FD course-file
           LABEL RECORDS ARE OMITTED.
           
       01 course-record.
           05 CourseNumberIn           PIC X(6).
           05 FILLER                   PIC X VALUE SPACES.
           05 CourseNameIn             PIC X(15).
           05 FILLER                   PIC X VALUE SPACES.
           05 CourseDaysIn             PIC X(3).
           05 FILLER                   PIC X VALUE SPACES.
           05 CourseTimeIn             PIC X(7).
           05 FILLER                   PIC X VALUE SPACES.
           05 ProfLastNameIn           PIC X(10).
           
       FD registration-file
           LABEL RECORDS ARE OMITTED.
           
       01 registration-record.
           05 RegStuNumIn              PIC 99999.
           05 FILLER                   PIC X VALUE SPACES.
           05 RegCourNumIn             PIC X(6).
       
       FD report-file
           LABEL RECORDS ARE OMITTED.
       
       01 report-record.
           05 FILLER                   PIC X(80).  
       
       WORKING-STORAGE SECTION.
       
       77 EOF                          PIC x(3).
              
       01 input-data.
           05 MenuChoice               PIC 99.
           05 ReportMenuChoice         PIC 99.
           05 MajorChoice              PIC X(25).
           05 ProfChoice               PIC X(25).
           05 ModStudentChoice         PIC 9.
           05 ModCourseChoice          PIC 9.
           05 StudentNumberChoice      PIC 9(5).
           05 CourseNumberChoice       PIC X(6).
           05 BinaryConfirmChoice      PIC 9.
       
       01 student-tables.
           05 StudentNumber            PIC 99999 OCCURS 100 TIMES.
           05 StudentLastName          PIC X(10) OCCURS 100 TIMES.
           05 StudentFirstName         PIC X(11) OCCURS 100 TIMES.
           05 StudentMajor             PIC X(7)  OCCURS 100 TIMES.
           05 StudentGPA               PIC 9v99  OCCURS 100 TIMES.
           
       01 new-student.
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 NewStudentNumber         PIC 99999.
           05 FILLER                   PIC X VALUE SPACES.
           05 NewStudentLastName       PIC X(10).
           05 FILLER                   PIC X VALUE SPACES.
           05 NewStudentFirstName      PIC X(11).
           05 FILLER                   PIC X VALUE SPACES.
           05 NewStudentMajor          PIC X(7).
           05 FILLER                   PIC X VALUE SPACES.
           05 NewStudentGPA            PIC 9v99.
           
       01 course-tables.
           05 CourseNumber             PIC X(6)  OCCURS 100 TIMES.
           05 CourseName               PIC X(15) OCCURS 100 TIMES.
           05 CourseDays               PIC X(3)  OCCURS 100 TIMES.
           05 CourseTime               PIC X(7)  OCCURS 100 TIMES.
           05 ProfLastName             PIC X(10) OCCURS 100 TIMES.

       01 new-course.
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 NewCourseNumber          PIC X(6).
           05 FILLER                   PIC X VALUE SPACES.
           05 NewCourseName            PIC X(15).
           05 FILLER                   PIC X VALUE SPACES.
           05 NewCourseDays            PIC X(3).
           05 FILLER                   PIC X VALUE SPACES.
           05 NewCourseTime            PIC X(7).
           05 FILLER                   PIC X VALUE SPACES.
           05 NewProfLastName          PIC X(10).

       01 registration-tables.
           05 RegStuNum                PIC 99999 OCCURS 100 TIMES.
           05 RegCourNum               PIC X(6)  OCCURS 100 TIMES.
       
       01 new-registration.
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 NewRegStuNum             PIC 99999.
           05 FILLER                   PIC X VALUE SPACES.
           05 NewRegCourNum            PIC X(6).
       
       01 iterators.
           05 StudentCount             PIC 999 VALUE 0.
           05 CourseCount              PIC 999 VALUE 0.
           05 RegCount                 PIC 999 VALUE 0.
           05 I                        PIC 999 VALUE 0.
           05 J                        PIC 999 VALUE 0.
           05 Loc                      PIC 999 VALUE 0.  
           05 TmpCount                 PIC 999 VALUE 0.
           05 FoundStudent             PIC 9 VALUE 0.                   .
           05 FoundCourse              PIC 9 VALUE 0.
   
       01 student-record-heading.
           05 FILLER                   PIC X(2) VALUE "ID".
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 FILLER                   PIC X(5) VALUE "LNAME".
           05 FILLER                   PIC X(6) VALUE SPACES.
           05 FILLER                   PIC X(5) VALUE "FNAME".
           05 FILLER                   PIC X(7) VALUE SPACES.           
           05 FILLER                   PIC X(5) VALUE "MAJOR".
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 FILLER                   PIC X(3) VALUE "GPA".

       01 student-record-disp.
           05 StudentNumberDisp        PIC 99999.
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentLastNameDisp      PIC X(10).
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentFirstNameDisp     PIC X(11).
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentMajorDisp         PIC X(7).
           05 FILLER                   PIC X VALUE SPACES.
           05 StudentGPADisp           PIC 9.99.
       
       01 course-record-heading.
           05 FILLER                   PIC X(2)  VALUE "ID".
           05 FILLER                   PIC X(5)  VALUE SPACES.
           05 FILLER                   PIC X(4)  VALUE "NAME".
           05 FILLER                   PIC X(12) VALUE SPACES.
           05 FILLER                   PIC X(3)  VALUE "DAY".
           05 FILLER                   PIC X(1)  VALUE SPACES.          
           05 FILLER                   PIC X(4)  VALUE "TIME".
           05 FILLER                   PIC X(4)  VALUE SPACES.
           05 FILLER                   PIC X(6)  VALUE "PROFLN".
       
       01 course-record-disp.
           05 CourseNumberDisp         PIC X(6).
           05 FILLER                   PIC X VALUE SPACES.
           05 CourseNameDisp           PIC X(15).
           05 FILLER                   PIC X VALUE SPACES.
           05 CourseDaysDisp           PIC X(3).
           05 FILLER                   PIC X VALUE SPACES.
           05 CourseTimeDisp           PIC X(7).
           05 FILLER                   PIC X VALUE SPACES.
           05 ProfLastNameDisp         PIC X(10).
       
       01 registration-record-heading.
           05 FILLER                   PIC X(5) VALUE "STUID".
           05 FILLER                   PIC X VALUE SPACES.
           05 FILLER                   PIC X(6) VALUE "COURID".
       
       01 registration-record-disp.
           05 RegStuNumDisp            PIC 99999.
           05 FILLER                   PIC X(1) VALUE SPACES.
           05 RegCourNumDisp           PIC X(6).

       01 report-header.
           05 FILLER                   PIC X(9) VALUE "---------".
           05 FILLER                   PIC X VALUE SPACES.
           05 ReportSectionTitle       PIC X(20).
           05 FILLER                   PIC X VALUE SPACES.
           05 FILLER                   PIC X(9) VALUE "---------".
           
       01 report-searchterms.
           05 FILLER                   PIC X(13) VALUE "Searched for:".
           05 FILLER                   PIC X VALUE SPACES.
           05 ReportSearchTerm         PIC X(50).

      *=================================================================
       
       PROCEDURE DIVISION.
       
       MAIN-PROGRAM.
           
           PERFORM Read_student.
           PERFORM Read_course.
           PERFORM Read_register. 
           OPEN OUTPUT report-file.
           
           PERFORM Menu UNTIL MenuChoice IS EQUAL 5.

           CLOSE report-file.
           STOP RUN.
      
      
      ***************************************
      * Display main menu, accept user input
      *
       Menu.
       
           DISPLAY " ".
           DISPLAY "    CALU Registration System".
           DISPLAY "    ========================".
           DISPLAY "1.  Modify Student Information".
           DISPLAY "2.  Modify Course Information".
           DISPLAY "3.  Register Students in Courses".
           DISPLAY "4.  Report Section".
           DISPLAY "5.  Exit CALU System".
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
           ACCEPT MenuChoice.
           
           PERFORM Process_menu_selection.
      
      
      ****************************************
      * Perform paragraph based on menu choice
      *
       Process_menu_selection.
       
           IF      MenuChoice = 1 PERFORM Modify_student
           ELSE IF MenuChoice = 2 PERFORM Modify_course
           ELSE IF MenuChoice = 3 PERFORM Modify_register
           ELSE IF MenuChoice = 4 PERFORM Report_section
           ELSE IF MenuChoice = 5 PERFORM End_program
           END-IF.
      
      
      ****************************************
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
       
       
      ****************************************
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
       
       
      ****************************************
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
      
      
      ****************************************
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
      
      
      ****************************************
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
      
      
      ****************************************
      * Pull registration data into tables
      *
       Process_register.
       
           ADD RegCount 1 GIVING RegCount.
       
           MOVE RegStuNumIn  TO RegStuNum(RegCount).
           MOVE RegCourNumIn TO RegCourNum(RegCount).
       
           READ registration-file
               AT END MOVE "yes" TO EOF.


      ****************************************
      * Display modify student menu
      *
       Modify_student.
           
           DISPLAY " ".
           DISPLAY "    Modify Student".
           DISPLAY "    --------------".
           DISPLAY "1.  Add Student".
           DISPLAY "2.  Delete Student".
           DISPLAY "3.  Cancel".
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
               
           ACCEPT ModStudentChoice.
           PERFORM Process_mod_student_selection.
      
      
      ****************************************     
      * Take action depending on mod student menu choice
      *
       Process_mod_student_selection.
           
           IF ModStudentChoice IS EQUAL 1
               PERFORM Add_student
           ELSE IF ModStudentChoice IS EQUAL 2
               PERFORM Del_student
           END-IF.
           
           
      ****************************************
      * Read new student information from user input
      *
       Add_student.
       
           DISPLAY " ".
           DISPLAY "    Add Student".
           DISPLAY "    -----------".
           
           DISPLAY "    Student number: "     WITH NO ADVANCING.
           ACCEPT NewStudentNumber.
           
           DISPLAY "    Student last name: "  WITH NO ADVANCING.
           ACCEPT NewStudentLastName.
           
           DISPLAY "    Student first name: " WITH NO ADVANCING.
           ACCEPT NewStudentFirstName.
           
           DISPLAY "    Student major: "      WITH NO ADVANCING.
           ACCEPT NewStudentMajor.
           
           DISPLAY "    Student GPA: "        WITH NO ADVANCING.
           ACCEPT NewStudentGPA.
           
           DISPLAY "    -----------".
           DISPLAY "    Is this data correct?".
           DISPLAY new-student.
           DISPLAY "    -----------".
           DISPLAY "1.  Yes".
           DISPLAY "2.  No".
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
               
           ACCEPT BinaryConfirmChoice.
           
           IF BinaryConfirmChoice IS EQUAL 1
               PERFORM Save_new_student
           ELSE 
               PERFORM Add_student
           END-IF.
       
       
      ****************************************
      * Save student data to table in memory
      * 
       Save_new_student.
           
           ADD StudentCount 1 GIVING StudentCount.
           
           MOVE NewStudentNumber    TO StudentNumber(StudentCount).     
           MOVE NewStudentLastName  TO StudentLastName(StudentCount).   
           MOVE NewStudentFirstName TO StudentFirstName(StudentCount).  
           MOVE NewStudentMajor     TO StudentMajor(StudentCount).
           MOVE NewStudentGPA       TO StudentGPA(StudentCount).
           
           DISPLAY " ".
           DISPLAY "Student added.".
       
       
      ****************************************
      * Find student number to delete in memory table
      * 
       Del_student.
       
           DISPLAY " ".
           DISPLAY "    Delete Student".
           DISPLAY "    -----------".
       
           DISPLAY "    Enter student number to delete: "
               WITH NO ADVANCING.
           
           ACCEPT StudentNumberChoice.

           MOVE 0 TO Loc.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > StudentCount
               IF StudentNumber(I) IS EQUAL StudentNumberChoice
                   MOVE I TO Loc
               END-IF
           END-PERFORM.
               
           IF Loc > 0
               PERFORM Delete_student_at_loc    
           ELSE
               DISPLAY " "
               DISPLAY "Student not found."
           END-IF.
      
      
      *****************************************
      * Delete student from table in memory
      *
       Delete_student_at_loc.

           DISPLAY "    Delete Student ", 
               StudentNumber(Loc), ": ", 
               StudentLastName(Loc), " ",
               StudentFirstName(Loc)
           DISPLAY "    -----------"
           DISPLAY "1.  Yes"
           DISPLAY "2.  No"
           DISPLAY " "
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING
               
           ACCEPT BinaryConfirmChoice.
               
           IF BinaryConfirmChoice IS EQUAL 1
               SUBTRACT 1 FROM StudentCount GIVING TmpCount
               
               PERFORM VARYING I FROM Loc BY 1 UNTIL I > TmpCount
                   ADD 1 I GIVING J
                   
                   MOVE StudentNumber(J)    TO StudentNumber(I)
                   MOVE StudentLastName(J)  TO StudentLastName(I)      
                   MOVE StudentFirstName(J) TO StudentFirstName(I)     
                   MOVE StudentMajor(J)     TO StudentMajor(I)
                   MOVE StudentGPA(J)       TO StudentGPA(I)
               END-PERFORM
               
               SUBTRACT 1 FROM StudentCount
               
               DISPLAY " "
               DISPLAY "Student deleted."
           END-IF.
           

      ****************************************
      * Display modify course menu
      *
       Modify_course.
           
           DISPLAY " ".
           DISPLAY "    Modify Course".
           DISPLAY "    --------------".
           DISPLAY "1.  Add Course".
           DISPLAY "2.  Delete Course".
           DISPLAY "3.  Cancel".
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
               
           ACCEPT ModCourseChoice.
           PERFORM Process_mod_course_selection.
      
      
      ****************************************     
      * Take action depending on mod course menu choice
      *
       Process_mod_course_selection.
           
           IF ModCourseChoice IS EQUAL 1
               PERFORM Add_course
           ELSE IF ModCourseChoice IS EQUAL 2
               PERFORM Del_course
           END-IF.
           
           
      ****************************************
      * Read new course information from user input
      *
       Add_course.
       
           DISPLAY " ".
           DISPLAY "    Add Course".
           DISPLAY "    -----------".
           
           DISPLAY "    Course number: "             WITH NO ADVANCING.
           ACCEPT NewCourseNumber.
           
           DISPLAY "    Course name: "               WITH NO ADVANCING.
           ACCEPT NewCourseName.
           
           DISPLAY "    Course days (MTWRF): "       WITH NO ADVANCING.
           ACCEPT NewCourseDays.
           
           DISPLAY "    Course time (HH:MM AM/PM): " WITH NO ADVANCING.
           ACCEPT NewCourseTime.
           
           DISPLAY "    Prof Last Name: "            WITH NO ADVANCING.
           ACCEPT NewProfLastName.
           
           DISPLAY "    -----------".
           DISPLAY "    Is this data correct?".
           DISPLAY new-course.
           DISPLAY "    -----------".
           DISPLAY "1.  Yes".
           DISPLAY "2.  No".
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
               
           ACCEPT BinaryConfirmChoice.
           
           IF BinaryConfirmChoice IS EQUAL 1
               PERFORM Save_new_course
           ELSE 
               PERFORM Add_course
           END-IF.
      
      
      ****************************************
      * Save course data to table in memory
      * 
       Save_new_course.
           
           ADD CourseCount 1 GIVING CourseCount.

           MOVE NewCourseNumber TO CourseNumber(CourseCount).
           MOVE NewCourseName TO CourseName(CourseCount).
           MOVE NewCourseDays TO CourseDays(CourseCount).
           MOVE NewCourseTime TO CourseTime(CourseCount).
           MOVE NewProfLastName TO ProfLastName(CourseCount).
           
           DISPLAY " ".
           DISPLAY "Course added.".
       
       
      ****************************************
      * Find course number to delete in memory table
      * 
       Del_course.
       
           DISPLAY " ".
           DISPLAY "    Delete Course".
           DISPLAY "    -----------".
       
           DISPLAY "    Enter course number to delete: "
               WITH NO ADVANCING.
           
           ACCEPT CourseNumberChoice.

           MOVE 0 TO Loc.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CourseCount
               IF CourseNumber(I) IS EQUAL CourseNumberChoice
                   MOVE I TO Loc
               END-IF
           END-PERFORM.
               
           IF Loc > 0
               PERFORM Delete_course_at_loc    
           ELSE
               DISPLAY " "
               DISPLAY "Course not found."
           END-IF.
      
      
      *****************************************
      * Delete course from table in memory
      *
       Delete_course_at_loc.

           DISPLAY "    Delete Course ", 
               CourseNumber(Loc), ": ", 
               CourseName(Loc)
           DISPLAY "    -----------"
           DISPLAY "1.  Yes"
           DISPLAY "2.  No"
           DISPLAY " "
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING
               
           ACCEPT BinaryConfirmChoice.
               
           IF BinaryConfirmChoice IS EQUAL 1
               SUBTRACT 1 FROM CourseCount GIVING TmpCount
               
               PERFORM VARYING I FROM Loc BY 1 UNTIL I > TmpCount
                   ADD 1 I GIVING J
                   
                   MOVE CourseNumber(J) TO CourseNumber(I)
                   MOVE CourseName(J) TO CourseName(I)
                   MOVE CourseDays(J) TO CourseDays(I)
                   MOVE CourseTime(J) TO CourseTime(I)
                   MOVE ProfLastName(J) TO ProfLastName(I)
               END-PERFORM
               
               SUBTRACT 1 FROM CourseCount
               
               DISPLAY " "
               DISPLAY "Course deleted."
           END-IF.


      *****************************************
      * Display add registration menu 
      *
       Modify_register.
           
           DISPLAY " ".
           DISPLAY "    Add Registration Info".
           DISPLAY "    --------------".

           DISPLAY "    Student number: " WITH NO ADVANCING.
           ACCEPT NewRegStuNum.
           
           DISPLAY "    Course number: "  WITH NO ADVANCING.
           ACCEPT NewRegCourNum.
           
           DISPLAY "    -----------".
           DISPLAY "    Is this data correct?".
           DISPLAY new-registration.
           DISPLAY "    -----------".
           DISPLAY "1.  Yes".
           DISPLAY "2.  No".
           DISPLAY " ".
           DISPLAY "    Please make your selection: "
               WITH NO ADVANCING.
               
           ACCEPT BinaryConfirmChoice.
           
           IF BinaryConfirmChoice IS EQUAL 1
               PERFORM Save_new_register
           ELSE 
               PERFORM Modify_register
           END-IF.
       
       
      *****************************************
      * Save course data in table memory
      *
       Save_new_register.
           
           MOVE 0 TO FoundStudent.
           MOVE 0 TO FoundCourse.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > StudentCount
               IF StudentNumber(I) IS EQUAL NewRegStuNum
                   MOVE 1 TO FoundStudent
               END-IF
           END-PERFORM.
           
           IF FoundStudent < 1
               DISPLAY " "
               DISPLAY "Invalid student number. Please try again."
               PERFORM Modify_register
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CourseCount
               IF CourseNumber(I) IS EQUAL NewRegCourNum
                   MOVE 1 TO FoundCourse
               END-IF
           END-PERFORM.
           
           IF FoundCourse < 1
               DISPLAY " "
               DISPLAY "Invalid course number. Please try again."
               PERFORM Modify_register
               EXIT PARAGRAPH
           END-IF.
           
           ADD RegCount 1 GIVING RegCount.
       
           MOVE NewRegStuNum  TO RegStuNum(RegCount).
           MOVE NewRegCourNum TO RegCourNum(RegCount).

           DISPLAY " ".
           DISPLAY "Registration info added.".


      *****************************************
      * Display reporting menu, accept user input
      *
       Report_menu.
           
           DISPLAY " ".
           DISPLAY "    CALU Report Menu".
           DISPLAY "    ----------------".
           DISPLAY "1.  Student Master List".
           DISPLAY "2.  Course Master List".
           DISPLAY "3.  Register Master List".
           DISPLAY "4.  List of Students by Major".
           DISPLAY "5.  List of Courses by Professor".
           DISPLAY "6.  Honors Students".
           DISPLAY "7.  Student Schedule For One Student".
           DISPLAY "8.  Course Roster For One Course".
           DISPLAY "9.  Course Roster For All Courses".
           DISPLAY "10. Report 10".
           DISPLAY "11. Exit Report Menu".
           
           DISPLAY " ".
           DISPLAY "     Please make your selection: "
               WITH NO ADVANCING.
               
           ACCEPT ReportMenuChoice.
           
           PERFORM Process_Report_Menu_Selection.
      
      
      *****************************************                         
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


      *****************************************
      * Run the report sub-section paragraphs
      *
       Report_section.

           PERFORM Report_menu UNTIL ReportMenuChoice IS EQUAL 11.
           MOVE 0 TO ReportMenuChoice.
       
       
      *****************************************
      * Display the entire list of students
      * Write to report file
      *
       Student_master_list.

           MOVE "Student Master List" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.
           
           DISPLAY student-record-heading.
           WRITE report-record FROM student-record-heading.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > StudentCount
               PERFORM Display_student_table_line
           END-PERFORM.
          
          
      *****************************************
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
      
      
      *****************************************
      * Display the entire list of courses
      * Write to report file
      *
       Course_master_list.

           MOVE "Course Master List" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.
           
           DISPLAY course-record-heading.
           WRITE report-record FROM course-record-heading.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CourseCount
               PERFORM Display_course_table_line
           END-PERFORM.
       
       
      *****************************************
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
       
       
      *****************************************
      * Display the entire list of registrations
      * Write to report file
      *
       Register_master_list.

           MOVE "Reg Master List" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.
           
           DISPLAY registration-record-heading.
           WRITE report-record FROM registration-record-heading.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RegCount 
               PERFORM Display_register_table_line
           END-PERFORM.
       
       
      *****************************************
      * Display a record from the registration table indexed by I
      * Write to report file
      *
       Display_register_table_line. 

           MOVE RegStuNum(I)  TO RegStuNumDisp.
           MOVE RegCourNum(I) TO RegCourNumDisp.
           
           DISPLAY registration-record-disp.
           WRITE report-record FROM registration-record-disp.


      *****************************************
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
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > StudentCount
               IF StudentMajor(I) IS EQUAL MajorChoice
                   PERFORM Display_student_table_line
               END-IF
           END-PERFORM.
      
      
      *****************************************
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
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CourseCount
               IF ProfLastName(I) IS EQUAL ProfChoice
                   PERFORM Display_course_table_line
               END-IF
           END-PERFORM.
           
      
      *****************************************
      * Display list of honors students (GPA > 3.5)
      *     
       Honor_students.
       
           MOVE "Honors Students" TO ReportSectionTitle.
           DISPLAY report-header.
           WRITE report-record FROM report-header.

           DISPLAY student-record-heading.
           WRITE report-record FROM student-record-heading.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > StudentCount
               IF StudentGPA(I) > 3.5
                   PERFORM Display_student_table_line
               END-IF
           END-PERFORM.
      
      
      *****************************************
      * Ask for student number, show student number and name
      * Show all course information for student's courses
      *
       Student_schedule.
       
           DISPLAY "TODO IMPLEMENT Student_schedule".
      
      
      *****************************************
      * Ask for course number, sho all course info for course
      * Show all students enrolled in course
      *
       Course_roster.
       
           DISPLAY "TODO IMPLEMENT Course_roster".
       
       
      *****************************************
      * For all courses, sho all course info for course
      * Show all students enrolled in each course
      * 
       Multi_course_roster.
       
           DISPLAY "TODO IMPLEMENT Multi_course_roster".
      
      
      *****************************************
      * Make your own report
      *
       Report_10.
       
           DISPLAY "TODO IMPLEMENT Report_10".
      
      
      *****************************************
      * Save data back to student, course and registration files
      * before ending the program
      *
       End_program.
       
           DISPLAY " ".
           DISPLAY "TODO IMPLEMENT End_program".
           
       END PROGRAM CALURegistration.