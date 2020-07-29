# Assignment - Implementation of a 2-layered CNN in Scala using Functional Programming

This folder contains my solution for assignment of on-campus Principles of Programming Languages (CS F301) course. The task is to implement a two layered convolutional neural network (CNN) in Scala using functional programming. The detailed instructions on implementing the CNN and provided test-cases are mentioned in `Assignment.pdf`. 

To install scala on Ubuntu 16.04, run the command:
```sh
sudo apt-get install scala
```

To compile the code (i.e `F2016A7PS0150P.scala` and `driver.scala`), run the command:
```sh
scalac F2016A7PS0150P.scala driver.scala
```

The subfolder `test_cases` contains three test cases: `t0`, `t1` and `t2`. To run your code on test-case `t0` (let's say), run the command:
```sh
scala pplAssignment.Driver test_cases/t0.in > t0.check
```

 To compare the program output with the given output, run the command:
```sh
diff test_cases/t0.out t0.check
```

