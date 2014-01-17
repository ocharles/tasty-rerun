# `tasty-rerun`

Often when we are building large projects in Haskell we only change a small part
of the system at a time. With `tasty`, you get the ability to specify
*patterns*, which let you filter the test tree down to a smaller size. However,
this is a manual process, and it doesn't necessarily have to be that way.

`tasty-rerun` allows you to run your tests using a normal `tasty` `Ingredient`,
but also filtering the test tree based on previous runs. `tasty-rerun` gives you
the option to:

- Run tests that failed or threw exceptions in the last test run
- Run tests that have been added since the last saved test run
- Run tests that passed in the last test run

`tasty-rerun` works by watching the result of a test run and saving a state file
(if you ask it to). Future test runs can use this log file to determine what
subtree(s) to execute.
