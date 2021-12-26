.. _gdscript:

======================================================================
The new GDScript parser
======================================================================

The GDScript parser is written using the token based Python parser as a base
due to similarities with the Python language. Some adjustments have been made
for the differences between GDScript and Python. A short list of major
differences:

- Files are classes. All symbols are attributes of the class defined by the
  file.
- There are no functions because all symbols are class attributes, so all
  "functions" are methods.
- Variables are explicitly declared with the `var` keyword.
- Enum, signal and const keywords are added.
