/* Taken from linux/Documentation/doc-guide/kernel-doc.rst */
/**
 * DOC: in-line
 */

/**
 * struct foo - Brief description.
 * @foo: The Foo member.
 */
struct foo {
      int foo;
      /**
       * @bar: The Bar member.
       */
      int bar;
      /**
       * @baz: The Baz member.
       *
       * Here, the member description may contain several paragraphs.
       */
      int baz;
      union {
              /** @foobar: Single line description. */
              int foobar;
      };
      /** @bar2: Description for struct @bar2 inside @foo */
      struct {
              /**
               * @bar2.barbar: Description for @barbar inside @foo.bar2
               */
              int barbar;
      } bar2;
};
