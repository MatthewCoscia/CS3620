# Options Trading

This trading DSL gives you the abiltiy to test complex options trades before execution in your broker. With plenty of different option trading presets and error checking, the package makes sure you're staying safe financially while trading. 

Here's an example program

```
#lang racket

(require minipeg)
```

## Installing and running

Check out this Git repository, change directory into it, and run:


```
raco pkg install
```

Then import as

```
(require trading)
```

Once installed, you can access the documentation via:

```
raco docs trading
```

Finally, you can run the tests with:

```
raco test -p trading
```
