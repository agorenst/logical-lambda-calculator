#/bin/bash
/usr/lib/noweb/markup book.nw |
sed "/^@use /s/_/\\_/g;/^@defn /s/_/\\_/g" |
python mynoweb.py |
/usr/lib/noweb/totex -noindex -delay