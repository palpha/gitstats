GitStats
========

This is a work in progress. It might be usable in its current state,
but it lacks proper error handling and might output weird stuff when
you least expect it.

Usage
-----
```GitStats.exe --help```

The program will output Markdown-formatted statistics that will probably
look all wrong -- tuning of the options is needed to get the numbers you
expect. ```--include-ext``` and ```--exclude-ext``` are your friends,
because you're probably not interested in statistics for your XML files.
```--ignore``` will help you get rid of ```lib``` folders full of jQuery
you don't want to count as contributions from your colleagues.

If you're running this on a large repo, it will take a very long time.
Running with ```--log-only``` is pretty fast, but you'll miss out on the
current status of things -- who owns most of the current code? Don't give
that retired dude credit for writing a lot of code that has since been
replaced.

TL;DR: ```--help```

Code Quality Disclaimer
-----------------------
This program was authored as part of learning F#. Its style does not
always conform to industry standards, and I'm pretty sure one can find
ridiculously stupid ways of doing things if one were to review the code.
If you do find something ridiculously stupid, let me know! :D

Licence
-------

Copyright 2013 Niklas Bergius (<niklas@bergius.org>)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this program except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
