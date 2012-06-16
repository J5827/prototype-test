Iteration planning test
=======================

[DONE] Iteration (A) - Project initalization
--------------------------------------------

    1. [DONE] The project repository with version control is initalized.
       a) [DONE] A remote repository is created.
       b) [DONE] The remote repository is connected to a local repository.
       c) [DONE] An inital commit has been made successfully.
    2. [DONE] The application skeleton is created.
       a) [DONE] The framework skeleton is initialized via command.
       b) [DONE] The skeleton is cleaned up by removal of unneeded code.
       c) [DONE] An ignore for the log files and the compiled files is set.
    3. [DONE] A first request to the application succeeds in a response.
       a) [DONE] The application code compiles.
       b) [DONE] The application code can be run and reached by a web browser.
       c) [DONE] The initial project state is committed to the remote repository.


[DONE] Iteration (B) - User management
--------------------------------------
    
    1. [DONE] The authentication library is integrated.
    2. [DONE] The login and registration forms are created.
       a) [DONE] Main controller routes a not authenticated user to a login page.
       b) [DONE] A login handler dummy is created.
       c) [DONE] The login data is created.
       d) [DONE] The login handler functionality is completed.
       d) [DONE] A login form is created.
       e) [DONE] A registration form is created (linked from the login form).
    3. [DONE] The authentication is implemented.
       a) [DONE] Login data is created (for tutor and for student).
       b) [DONE] Wrong login attempt fails.
       c) [DONE] Correct login attempt succeeds.
    4. [DONE] The user is routed to a role specific page after login.
       a) [DONE] A tutor handler is created.
       b) [DONE] A tutor page is created.
       e) [DONE] An authenticated tutor is routed to the tutor page.
       c) [DONE] A student handler is created.
       d) [DONE] A student page is created.
       b) [DONE] A student is routed to a student 
       e) [DONE] An authenticated student is routed to the student page.
    5. [DONE] Authenticated users can logout via a link.
    6. [DONE] Students are denied access to tutors page and the other way around.


Iteration (C) - Course management
---------------------------------

    1. [DONE] A persistent library is integrated.
    2. [DONE] A tutor can create a course.
       a) [DONE] The tutor can set a name.
       b) [NO] The tutor can select a semester.
       c) [DONE] The tutor can set a cource capacity.
       d) [DONE] The tutor can save the course into the persistent store.
    2. [DONE] A tutor can see a list of his courses.
       a) [DONE] On the tutor page a list of courses he created is visible.
    3. A student must be able to see a list of courses.
       a) On the student page a list of all courses at the school is visible.
    4. A student can enrol in a course.
       a) A student can enrol in active courses (for a current semester).
       b) A student must not already be enrolled in a course and the course
          must not already be full in order to succeed the enrolment.


Iteration (D) - Task management
-------------------------------

    1. A tutor can create a configured task.
       a) TaskTree
       b) TaskDescription
       c) Config
       d) Verification
       e) Save
    2. [DONE] A tutor can see a list of tasks he created.
       a) [DONE] tutor page ...
    3. A tutor can assign a configured task to a course
       a) ...
    4. A student can see tasks that are assigned to h


Iteration (E) - User interface design
-------------------------------------

    1. bootstrap toolkit
    2. bring order to the templates
    3. custom css


Iteration (F) - Permission system
---------------------------------

    1. start + end date to tasks
    2. number of tasks to course
    3. number of required tasks to course
    4. progress bar and status to student page


Iteration (X) - Schools
-----------------------

    1. User can select a school
