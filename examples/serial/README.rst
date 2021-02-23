********************************************
FyTest demonstration for testing serial code
********************************************

This folder shows how to test serial Fortran code via FyTest. It contains
a fully working minimal project, which you can take as template when starting
your project.

The current template uses the CMake tool for building the library and the tests,
but it should be easily adaptable for any other build systems, provided they
support the usage of preprocessors for Fortran projects.

The main components are:

* ``CMakeLists.txt``: The main build configuration file.

* ``lib/``: The directory containing the library to be tested. This is where
  you develop and store your code.

* ``test/``: The directory containing the unit tests for your library. This
  were you put all the code for the unit testing.

* ``test/fytest``: Directory containing the minimal FyTest redistributable
  code. (In your project, you would just copy the ``fytest`` directory from
  the FyTest project into your project.) It is recommended to distribute
  FyTest with your project (only 2 files) to make sure, it is build with
  the same compiler and compiler flags as your project.
