# clj-utils

Unrelated Clojure utilities

## Usage

~~~.clj
[acyclic/utils "0.0.2"]
~~~

### acyclic.util.cli

Define ```cli-options``` as per ```clojure.tools.cli``` and a function ```doit```
that takes a key-value map and returns some result.  Then
~~~.clj
  (defn -main [& args] (edn-app args cli-options doit)
~~~
If you do not specify corresponding options yourself, then you'll get for free:

* --log LOGLEVEL  (e.g. "debug", "info", etc., which will be ```keyword```ed and passed to ```timbre```
* --help  (prints out an unfriendly but useful dump of option definitions)
* --id SOMESTRING  (Anything to uniquely identify this invocation)
* --hang SECS  (Sleep for SECS before exiting)
* --opts EDNMAP  (The entire options map in EDN parseable form, e.g. ```{:yournum 3 :id \"XYZ\"}```

Options will be processed into key-value pairs (as per ```clojure.tools.cli``` again)
and the return value of doit will be wrapped as
~~~.clj
{:result return-value-of-doit
 :exception nil-or-stack-trace
 :log nil-or-vector
 :id the-id-you-specified-or-X
}
~~~
and printed to standard output in EDN parseable form.

### acyclic.utils.pinhole

See http://blog.podsnap.com/pinhole.html

### acyclic.utils.log

A few handy functions that I and probably nobody else will use in concert with
```taoensso.timbre```.

* ```(strack-trace e)``` - returns a full stack trace from an ```Exception``` as a vector of strings
* ```(fname f)``` - returns the qualified name of a clojure function, as a string
* ```(set-logging level)``` - Sets the ```timbre``` logging level as specified and
  installs an appender that
 * accrues log events in a vector
 * prints them to stderr as well
* ```(with-accrued-log m)``` - places the vector of accrued events into map ```m```, keyed by ```:log```.


## License

Copyright © 2014 Peter Fraenkel

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
