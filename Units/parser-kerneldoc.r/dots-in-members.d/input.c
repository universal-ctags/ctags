/* Taken from linux/Documentation/doc-guide/kernel-doc.rst */
/**
 * DOC: dots in members
 */

/**
 * struct nested_foobar - a struct with nested unions and structs
 * @memb1: first member of anonymous union/anonymous struct
 * @memb2: second member of anonymous union/anonymous struct
 * @memb3: third member of anonymous union/anonymous struct
 * @memb4: fourth member of anonymous union/anonymous struct
 * @bar: non-anonymous union
 * @bar.st1: struct st1 inside @bar
 * @bar.st2: struct st2 inside @bar
 * @bar.st1.memb1: first member of struct st1 on union bar
 * @bar.st1.memb2: second member of struct st1 on union bar
 * @bar.st2.memb1: first member of struct st2 on union bar
 * @bar.st2.memb2: second member of struct st2 on union bar
 */
struct nested_foobar {
  /* Anonymous union/struct*/
  union {
    struct {
      int memb1;
      int memb2;
    };
    struct {
      void *memb3;
      int memb4;
    };
  };
  union {
    struct {
      int memb1;
      int memb2;
    } st1;
    struct {
      void *memb1;
      int memb2;
    } st2;
  } bar;
};
