# Horrible example of how to interface with a C++ engine ... ;-)

{.link: "/usr/lib/libIrrlicht.so".}

{.emit: """
using namespace irr;
using namespace core;
using namespace scene;
using namespace video;
using namespace io;
using namespace gui;
""".}

const
  irr = "<irrlicht/irrlicht.h>"

type
  TDimension2d {.final, header: irr, importc: "dimension2d".} = object
  Tvector3df {.final, header: irr, importc: "vector3df".} = object
  TColor {.final, header: irr, importc: "SColor".} = object
