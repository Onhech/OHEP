# OHEP Demo

Use `Demo/build/index.html` as the review entrypoint.

This folder is a UI prototype for page flow, hierarchy, and interaction patterns. It is not a source of truth for backend architecture, analytics logic, or final calculation design.

If you need to regenerate the compiled demo bundle, run:

```sh
Rscript Demo/scripts/build_demo.R
```

The build script depends on the sibling `ohepR/` package in this repository and is not fully standalone outside the repo.
