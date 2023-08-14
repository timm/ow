<img align=right width=300 src="docs/img/banner.png">

# ows: our web site

Simple static web site generator (you only need to download one file).

Written out of frustration with Jekyll, Hykell, 11tweenty, Hugo, etc etc.  

Given source code in lisp or Python or LUA format, generate one web page per file. If the files contain multi-line comments,
render those use Python's Markdown with the following exxtesions:

        -x toc -x codehilite -x tables -x fenced_code -x footnotes -x attr_list -x admonition


That's all folks.

```
ows: our web site, built from markdown in multi-line comments
(c)2023, Tim Menzies, <timm@ieee.org> BSD-2

USAGE:
  sh ows [-Fdht] files

OPTIONS:
  -F            force recomple of all
  -d  dir       output directory       = docs
  -h            show help 
  -t  template  template               = default
```
