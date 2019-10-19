# watch - Watch State of Function Environment

## Overview

Tools to watch the state of a function environment as it evaluates each of its
component 'top-level' calls.

This is a very experimental package built and tested primarily against a handful
of functions, so it is likely there are many unaddressed corner cases and
general fragility.  In particular, we rely on empirically observed but
undocumented structure of the `getParseData` output.

The API may change completely in future iterations if there are any.  Or this
project could be abandoned.

Feel free to file issues.  I may or may not respond to them.

## Usage

A watched function will return it's result with a `watch.data` attribute
attached.

```{r}

```


## Possible Improvements

* Handle nested/recursive calls.  Conceptually this isn't too difficult: we just
  need to add a call that checks `.res` for the `watch.*` attributes after each
  top level statement.  Simplification / visualization will be another matter.
* 
