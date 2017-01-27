[DllImport] public class MyDllimportClass {}

[DllImport("user32.dll", SetLastError=false, ExactSpelling=false)]
[DllImport("user32.dll", ExactSpelling=false, SetLastError=false)]
[DllImport("user32.dll")]

[Conditional("DEBUG"), Conditional("TEST1")] void TraceMethod() {}

using System;
[AttributeUsage(AttributeTargets.Class|AttributeTargets.Struct)]
public class Author : Attribute
{
   public Author(string name) { this.name = name; version = 1.0; }
   public double version;
   string name;
}

[AttributeUsage(AttributeTargets.Class|AttributeTargets.Struct,
   AllowMultiple=true)] // multiuse attribute
public class Author : Attribute
{
}
