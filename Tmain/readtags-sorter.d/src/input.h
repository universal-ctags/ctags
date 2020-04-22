#ifndef INPUT_DATA_H
#define INPUT_DATA_H

struct ipoint2d {
  int x, y;
};

struct ipoint3d {
  int x, y, z;
};

struct fpoint2d {
  float x, y;
};

struct fpoint3d {
  fpoint2d parent;
  float z;
};

float area   (ipoint2d &p);
float area   (fpoint2d &p);
float volume (ipoint3d *p);
float volume (fpoint3d *p);

#endif
