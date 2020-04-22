#include "input.h"

float area   (ipoint2d &p)
{
  return (float)(p.x * p.y);
}

float area   (fpoint2d &p)
{
  return p.x * p.y;
}
