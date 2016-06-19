Installation
============

1. Copy `config.R.example` to `config.R` and customize the values.
2. Fetch the dependencies listed in `install_dependencies.R`. Source this script in an interactive session to select the mirror.
3. Fetch the database dump and list of courses using `update.R`.

Ready!

Updating the list of courses
============================

Course names can be fetched using the script available here:

https://github.com/mluszczyk/listcourses

Clone repository to course-correlation-app/

Install the requirements and run the script to get the list of course names.

Platform
========

We have experienced problems in dplyr when using this package on OS X. Linux is the recommended platform.
