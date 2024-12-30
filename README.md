# Stamps (WIP)

Stamps is an annotation tool that can also be used for note-taking, it
is meant to be used with notes (any file) that are scattered across
multiple directories.

# Features

- Annotate of any kind of file. 
- Provides navigation  between files with annotations.
- Easy annotation of BibTeX bibliography (PDF, media files, URLS).

# Usage

## Annotating PDF

Open a PDF that is included in the Bibtex bibliography (Citar's
`citar-open-files` is very useful this purpose). Create or navigate to
a note, then the command `stamps-annotate-pdf` can be used to create an
annotation in the open note.

## Annotating Media

Open a file or URL, that is included in the Bibtex bibliography, using
`mpv-play` (or citar-open-files if configured to use mpv.el), then the
command `stamps-annotate-mpv` can be used to create an annotation in
an open note.
  
