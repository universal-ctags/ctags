--  Taken from #2943 submitted by @koenmeersman
with Ada.Text_IO;

separate (Buffer);

package body Test is
  procedure Inner is separate;
begin
  null;
end Test;

