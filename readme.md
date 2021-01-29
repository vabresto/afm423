# AFM 423

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/vabresto/afm423/HEAD)

Using a public git repo so I can use https://mybinder.org/ 

## Compile to PDF

If running from binder, produce a PDF by running

```
/srv/conda/envs/notebook/bin/pandoc +RTS -K512m -RTS a1/a1.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output a1/a1.pdf --lua-filter /srv/rlibs/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /srv/rlibs/rmarkdown/rmarkdown/lua/latex-div.lua --self-contained --highlight-style tango --variable graphics --variable 'geometry:margin=1in'
```

and replacing `a1/a1.md` and `a1/a1.pdf` as appropriate.

This should run from the git top level folder.
