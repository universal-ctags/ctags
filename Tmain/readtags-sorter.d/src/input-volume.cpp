#include "input.h"

float volume (ipoint3d *p)
{
  return (float)(p->x * p->y * p->z);
}

float volume (fpoint3d *p)
{
  return area (p->parent) * p->z;
}
