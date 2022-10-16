*************************************
FyTest — Instant Fortran unit testing
*************************************

**Note:** FyTest is deprecated and will not be developed further. You may
consider to use the `Fortuno <https://github.com/aradi/fortuno>`_ unit testing
system instead, which offers similar functionality but in a more robust
and more user-friendly framework.

FyTest is a lightweight unit testing framework for Fortran. Thanks to its
header-only design, it can be easily bundled with any Fortran project without
creating extra dependencies. It provides a versatile unit testing solution out
of the box. FyTest enables programmers to concentrate on the essential parts of
their unit tests by automatically creating the test framework using the powerful
`Fypp preprocessor <http://github.com/aradi/fypp>`_.

FyTest currently supports following main features:

* Unit tests
* Test fixtures with initializers and finalizers
* Parameterized test fixtures
* Test suites with initializers and finalizers
* Testing of MPI-parallel codes

At the current early stage of development, there is no extensive documentation
available yet. Have a look at the examples in the `examples/ <examples>`_
folder for demonstration of usage.

The project is `hosted on github <http://github.com/aradi/fytest>`_.

FyTest is released under the *BSD 2-clause license*.
