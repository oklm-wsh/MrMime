MrMime
======

[![Build Status](https://travis-ci.org/oklm-wsh/MrMime.svg)](https://travis-ci.org/oklm-wsh/MrMime)

MrMime is a pure implementation of 7 standards about the email. You can see an example to extract an image from an multipart email:

<p align="center">
  <img src="http://img.isomorphis.me/2jToU.gif" alt="Mr. Mime" />
</p>

You can see [the abstract](http://din.osau.re/mrmime.pdf) about this project for the ICFP.

### Example

You can see an example of MrMime to the `test.ml` file. It's a step-by-step program to extract an image from an email. You can execute this program with `utop` like:

```
$ utop test.ml > mrmime.gif
```

After a `make install` of this project.

--

This work was funded in part by the EU FP7/2007-2013 User Centric Networking project, grant agreement no 611001.
