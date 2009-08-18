#include <chipmunk/chipmunk.h>

void
gacela_cpInitChipmunk (void)
{
  cpInitChipmunk ();
}

void
gacela_cpResetShapeIdCounter (void)
{
  cpResetShapeIdCounter ();
}

int
gacela_cpSpaceNew (void)
{
  return cpSpaceNew ();
}

void
gacela_cpSpaceAddBody (int space, int body)
{
  cpSpaceAddBody (space, body);
}

void
gacela_cpSpaceAddShape (int space, int shape)
{
  cpSpaceAddShape (space, shape);
}

void
gacela_cpSpaceFree (int space)
{
  cpSpaceFree (space);
}

int
gacela_cpBodyNew (float mass, float inertia, float infinity)
{
  return cpBodyNew ((mass >= infinity ? INFINITY : mass), (inertia >= infinity ? INFINITY : inertia));
}

float
gacela_cpMomentForCircle (float mass, float r1, float r2, float x, float y)
{
  return cpMomentForCircle (mass, r1, r2, cpv (x, y));
}

void
gacela_cpBodyFree (int space)
{
  cpBodyFree (space);
}

int
gacela_cpCircleShapeNew (int body, float radius, float x, float y)
{
  return cpCircleShapeNew (body, radius, cpv (x, y));
}

int
gacela_cpPolyShapeNew (int body, int numVerts, int verts, float x, float y)
{
  return cpPolyShapeNew (body, numVerts, verts, cpv (x, y));
}

void
gacela_cpShapeFree (int shape)
{
  cpShapeFree (shape);
}

void
set_cp_space_gravity (int space, float x, float y)
{
  cpSpace *s = space;

  s->gravity = cpv (x, y);
}
