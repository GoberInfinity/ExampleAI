
#

#ExampleAI <a href="https://github.com/syl20bnr/spacemacs"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" /></a>

Examples and exercises with Lisp and an application to AI.

#Introduction

For the project i'm going to work with (SBCL) a high performance Common Lisp compiler and Spacemacs editor (you will want a development environment which is more human friendly than the basic SBCL).


#Prerequisities

* Install ["SBCL"](http://www.sbcl.org/platform-table.html) <br />
* Install ["SLIME"](https://github.com/slime/slime) <br />
* Install ["Spacemacs"](https://github.com/syl20bnr/spacemacs)

#Usage 
1. Open your Spacemacs. <br>
2. Press ```ctrl x ``` and type ```slime```.  <br>
3. Open a lisp file.  <br>
4. Press  ```ctrl-c-p``` after function to compile it: ```(defun Hello() (list 'Hellow_World))```.
5. Press again ```ctrl-c-p``` after the usage of function: ```(Hello)```.
6. See the result. ```(HELLOW_WORLD)```.

#Authors
* **Reyes Fragoso Roberto** - [Portfolio](http://robertoreyes.me)
