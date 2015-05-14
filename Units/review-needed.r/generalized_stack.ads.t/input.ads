-- Object-oriented generalized stack.  This illustrates the use of a
-- controlled type, which is one that has construction and destructions.
-- It also shows how to create two related types in the same package so
-- that they can share private declarations.  This sort of thing is
-- accomplished in Java or C++ using nested classes, or using friend
-- declarations in C++.
--
with Ada.Finalization; use Ada.Finalization;

package GenStack is
    -- This is the stack type.
    type Stack is new Controlled with private;

    -- This is the base type for nodes.  Client packages must derive their
    -- nodes from StackData.  Since it comes from Controlled, the user can
    -- override the Initialize, Adjust, and Finalize methods as needed.
    type StackData is new Controlled with null record;

    -- Initialization operations.
    procedure Initialize(S: in out Stack);
    procedure Adjust(S: in out Stack);
    procedure Finalize(S: in out Stack);

    -- Stack operations.
    procedure Push(S: in out Stack; D: StackData'class);
    procedure Pop(S: in out Stack; D: in out StackData'class);
    procedure Top(S: Stack; Data: in out StackData'class);
    function Empty(S: Stack) return Boolean;

    private
    -- Pointer to the node type.
    type Node;
    type Node_Ptr is access Node;

    -- Here is the generalized stack itself.  We would just make it the
    -- pointer itself, but it needs to be a record so it can be in a with.
    type Stack is new Controlled with record
        Head: Node_Ptr;
    end record;

    -- Now, we need a pointer to the data part.
    type Data_Ptr is access StackData'Class;

    -- This is the node type.
    type Node is record
        Data: Data_Ptr;
        Next: Node_Ptr;
    end record;

end GenStack;
