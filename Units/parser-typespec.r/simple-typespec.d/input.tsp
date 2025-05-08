import "@typespec/http";
import "./common.tsp";

using TypeSpec;
using TypeSpec.Rest;

@armProviderNamespace
@service(#{ title: "Name.Space" })
@versioned(Versions)
@armCommonTypesVersion(CommonTypes.Versions.v3)
namespace Name.Space;

namespace Inner {
  @doc("API Versions")
  @added(a())
  enum Versions {
    @doc("May 01, 2024 Preview API Version")
    v2024_05_01_preview: "2024-05-01-preview",
  }

  @summary("Example operation summary for demonstration purposes.")
  @doc("""
    Example
    multi-line
    documentation.
    """)
  @returnsDoc("Example return documentation")
  @post
  op OpA<T extends TypeSpec.Reflection.Model> is OpB<
    T & ModelA,
    ModelB<ModelA[]>
  >;

  // single line comment
  interface InterfaceA extends InterfaceB {
    opA is OpA<{
      @doc("The format of the HTTP payload.")
      @header
      contentType: "application/json";

      @clientName("uri", "c\\sharp")
      @doc("Example URL parameter.")
      opB: url;
    }>;

    /**
    * Example operation description
    */
    opB is OpB<
      ModelA,
      {
        ...ModelB
      }
    >;
  }

  @doc("Example union type description.")
  union UnionA {
    string,

    @doc("Example option A description with some technical details that would be typical for this kind of documentation.")
    "unionA",

    @doc("Example option B description with additional technical context that would be common in API documentation.")
    "unionB",
  }
}

@doc("Example model description.")
model ModelA {
  @doc("Example property description.")
  contentA: ModelB;

  @removed(Versions.v1_2)
  @doc("Example boolean property with typical documentation about its purpose and default behavior.")
  contentB?: boolean;

  ...ModelX;
}

@doc("Example model description.")
model ModelB extends ModelA {
  ...ModelX;
}


alias AliasA<
  TParamA extends ModelX,
  TParamB,
  TParamC extends ModelX = {},
  TError = Error
> = ModelB<TParamA, TParamB, TParamC, TError>;
