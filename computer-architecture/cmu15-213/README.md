# CMU CS 15-213: Introduction to Computer Systems

[Using the curriculum from Fall 2015](https://www.cs.cmu.edu/afs/cs/academic/class/15213-f15/www/schedule.html)

## Environment Setup

### Interactive GCC

[Interactive GCC](https://github.com/alexandru-dinu/igcc) is a cool interactive C repl

- If you type any wrong line you need to undo it with `.u` or it will forever error
- Type `.h` for help

## Labs

[Lab Assignments](http://csapp.cs.cmu.edu/3e/labs.html)

### Datalab

Troubleshooting `fatal error: gnu/stubs-32.h: No such file or directory`

- Install the 32-bit libc dev pacakge and gcc libs: [see Stack Overflow](https://stackoverflow.com/a/7412698)

```shell
$ sudo pacman -S lib32-glibc lib32-gcc-libs
```

