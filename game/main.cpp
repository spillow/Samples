
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
using namespace std;

#include <GL/gl.h>
#include <GL/glut.h>

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <typeinfo>

#include "json/json.h"

#include "btBulletDynamicsCommon.h"
#include "BulletCollision/NarrowPhaseCollision/btPersistentManifold.h"

#define RENDER_ANAGLYPH

#define REDBLUE  1
#define REDGREEN 2
#define REDCYAN  3
#define BLUERED  4
#define GREENRED 5
#define CYANRED  6

#define EYE_ZERO_OFFSET 0.2f

int g_GlassesType = REDCYAN;

#define GL_DIRECTIONAL_LIGHT	0.0f
#define GL_POSITIONAL_LIGHT     1.0f

#define ArraySize(x) (sizeof(x) / sizeof(x[0]))

#define SPHERE_MASS	10.0f
#define BOX_MASS	10000000.0f

extern ContactProcessedCallback gContactProcessedCallback;

const int window_width  = 800;
const int window_height = 800;

const float wall_size = 33.0;
const float back_wall_dist = 52.0;

btDiscreteDynamicsWorld* g_dynamicsWorld;

GLfloat g_mat_specular[]   = { 1.0, 1.0, 1.0, 1.0 };
GLfloat g_mat_shininess[]  = { 50.0 };
GLfloat g_light_position_1[] = { 0.0, wall_size/2.0f, -back_wall_dist + wall_size/2.0f, GL_POSITIONAL_LIGHT };
GLfloat g_light_position_2[] = { 0.0, wall_size/2.0f, -back_wall_dist + wall_size, GL_POSITIONAL_LIGHT };
GLfloat g_light_position_3[] = { 0.0, wall_size/2.0f, -back_wall_dist + wall_size/2.0f, GL_POSITIONAL_LIGHT };
GLfloat g_light_ambient[]  = { 0.1, 0.1, 0.1, 1.0 };
GLfloat g_light_diffuse[]  = { 1.0, 1.0, 1.0, 1.0 };

template <class T>
inline std::string to_string (const T& t)
{
std::stringstream ss;
ss << t;
return ss.str();
}

class Point
{
public:
  Point(float x, float y, float z) : _x(x), _y(y), _z(z) {}
  float GetX() { return _x; }
  float GetY() { return _y; }
  float GetZ() { return _z; }
private:
  float _x, _y, _z;
};

class Color
{
public:
  Color(float r, float g, float b) : _r(r), _g(g), _b(b) {}
  float GetR() { return _r; }
  float GetG() { return _g; }
  float GetB() { return _b; }
private:
  float _r, _g, _b;
};

class Box : public btRigidBody
{
public:
  Box(btVector3 initPos, btScalar side_size);
  ~Box();
  void DrawMe();
private:
  Color _color;
  btScalar _side_size;
  btDefaultMotionState* _motion_state;
  btBoxShape* _box_shape;
};

Box::Box(btVector3 initPos, btScalar side_size) :
  btRigidBody(BOX_MASS, _motion_state = new btDefaultMotionState(btTransform(btQuaternion(0,0,0,1), initPos)),
      _box_shape = new btBoxShape(btVector3(side_size/2.0f,side_size/2.0f,side_size/2.0f))),
      _color(Color(1.0,1.0,1.0)), _side_size(side_size)
{
  setCollisionFlags(getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT);
  setActivationState(DISABLE_DEACTIVATION);

  g_dynamicsWorld->addRigidBody(this);
  //setLinearVelocity(btVector3(0,0,-10.0f));
}

Box::~Box()
{
  delete _motion_state;
  delete _box_shape;

  g_dynamicsWorld->removeRigidBody(this);
}

void Box::DrawMe()
{
  translate(btVector3(15,0,0));

  btTransform trans;
  getMotionState()->getWorldTransform(trans);

  GLfloat mat_amb_diff[] = { _color.GetR(), _color.GetG(), _color.GetB(), 1.0 };
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_amb_diff);

  glPushMatrix();
  glTranslatef(trans.getOrigin().getX(), trans.getOrigin().getY(), trans.getOrigin().getZ());
  glutSolidCube(_side_size);
  glPopMatrix();
}

Box* g_MovingBox = 0;

class Sphere : public btRigidBody
{
public:
  Sphere(btVector3 initPos, btVector3 initVelocity, float radius, Color color);
  ~Sphere();
  void SetColor(Color color) { _color = color; }
  void DrawMe();
  bool _hasCollided;
private:
  Color _color;
  btVector3 _pos;
  btVector3 _velocity;
  float _radius;
  btDefaultMotionState* _motion_state;
  btSphereShape* _sphere_shape;
};

Sphere::Sphere(btVector3 initPos, btVector3 initVelocity, float radius, Color color) :
  btRigidBody(SPHERE_MASS, _motion_state = new btDefaultMotionState(btTransform(btQuaternion(0,0,0,1),initPos)),
  _sphere_shape = new btSphereShape(radius)),
  _color(color), _velocity(initVelocity), _pos(initPos), _radius(radius), _hasCollided(false)
{
  setLinearVelocity(initVelocity);

  //applyForce(btVector3(0,0,-5), btVector3(0,0,0));

  g_dynamicsWorld->addRigidBody(this);
}

Sphere::~Sphere()
{
  delete _motion_state;
  delete _sphere_shape;

  g_dynamicsWorld->removeRigidBody(this);
}

void Sphere::DrawMe()
{
  btTransform trans;
  getMotionState()->getWorldTransform(trans);

  GLfloat mat_amb_diff[] = { _color.GetR(), _color.GetG(), _color.GetB(), 1.0 };
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_amb_diff);

  glPushMatrix();
  glTranslatef(trans.getOrigin().getX(), trans.getOrigin().getY(), trans.getOrigin().getZ());
  glutSolidSphere(_radius, 50, 50);
  glPopMatrix();
}

bool contact_collision_callback(btManifoldPoint& cp, void* body0, void* body1)
{
  //printf("contact happened...\n");
  Sphere* obj1 = (Sphere*)body0;
  Sphere* obj2 = (Sphere*)body1;

  if (obj1->getInvMass() == 1.0f / SPHERE_MASS && obj2->getInvMass() == 1.0f / BOX_MASS) {
    obj1->SetColor(Color(0.0,0.3,0.8));
    obj1->_hasCollided = true;
  }
  else if (obj1->getInvMass() == 1.0f / BOX_MASS && obj2->getInvMass() == 1.0f / SPHERE_MASS) {
    obj2->SetColor(Color(0.0,0.3,0.8));
    obj2->_hasCollided = true;
  }

  return true;
}

class WorldInfo_t
{
public:
  static std::vector<Sphere*> spheres;
};

std::vector<Sphere*> WorldInfo_t::spheres;

void process_mouse(int button, int state, int x, int y)
{
  if (button == GLUT_LEFT_BUTTON && state == 1) {
    Sphere* sphere = new Sphere(btVector3(0,wall_size/2.0f,-back_wall_dist+1.5f*wall_size),
	btVector3(((float)x / (float)window_width) * wall_size - wall_size/2.0f,
	  (((float)window_height - (float)y) / (float)window_height) * wall_size,
	  -20.0f), 2.0, Color(0.7,0.7,0.7));
    WorldInfo_t::spheres.push_back(sphere);
  }
}

class Translate_t
{
public:
  static float X;
  static float Y;
  static float Z;
};

float Translate_t::X = 0.0f;
float Translate_t::Y = 0.0f;
float Translate_t::Z = 0.0f;

class Rotate_t
{
public:
  static float X;
  static float Y;
  static float Z;
};

float Rotate_t::X = 0.0f;
float Rotate_t::Y = 0.0f;
float Rotate_t::Z = 0.0f;

void process_key_press(unsigned char key, int x, int y)
{
  switch (key) {
    case 'a':
      Translate_t::X += 1.0f;
    break;
    case 'd':
      Translate_t::X -= 1.0f;
    break;
    case 'w':
      Translate_t::Y -= 1.0f;
    break;
    case 's':
      Translate_t::Y += 1.0f;
    break;
    case 'z':
      Translate_t::Z -= 1.0f;
    break;
    case 'x':
      Translate_t::Z += 1.0f;
    break;

    case 'j':
      Rotate_t::X -= 1.0f;
    break;
    case 'l':
      Rotate_t::X += 1.0f;
    break;
  }
}

void WaitMilliseconds(int seconds)
{
  clock_t endwait;
  endwait = clock () + seconds * (CLOCKS_PER_SEC / 1000);
  while (clock() < endwait);
}

void DrawWall(Point color, Point a, Point b, Point c, Point d)
{
  GLfloat mat_amb_diff[] = { color.GetX(), color.GetY(), color.GetZ(), 1.0 };
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_amb_diff);
  glBegin(GL_QUADS);
  glVertex3f(a.GetX(), a.GetY(), a.GetZ());
  glVertex3f(b.GetX(), b.GetY(), b.GetZ());
  glVertex3f(c.GetX(), c.GetY(), c.GetZ());
  glVertex3f(d.GetX(), d.GetY(), d.GetZ());
  glEnd();
}

float GetRand(float min, float max)
{
  return ((float)rand() / (float)RAND_MAX)*(max-min) + min;
}

void DrawMovingCube()
{
  float cube_size = 6.0f;

  static clock_t clock_val = 0;

  static float x = -3.0f;
  static float y = cube_size/2.0f;
  static float z = -back_wall_dist + wall_size/2.0f;

  int seconds_elapsed = (clock() - clock_val) / CLOCKS_PER_SEC;

  if (seconds_elapsed > 2) {
    x = GetRand(-wall_size/2.0f + cube_size/2.0f, wall_size/2.0f - cube_size/2.0f);
    y = GetRand(cube_size/2.0f, wall_size - cube_size/2.0f);
    z = GetRand(-back_wall_dist + cube_size/2.0f, -back_wall_dist + wall_size/2.0f);

    clock_val = clock();
  }

  g_MovingBox->DrawMe();

  delete g_MovingBox;
  g_MovingBox = new Box(btVector3(x,y,z), 5.0f);
}

void DrawWorld()
{
  static clock_t clock_val = 0;

  Point wall_color(0.2, 0.7, 0.1);
  Point ceil_floor(0.5, 0.2, 0.2);
  Point back_wall(0.3, 0.4, 0.3);

  WaitMilliseconds(10);

  int ms_elapsed = (clock() - clock_val) / (CLOCKS_PER_SEC / 1000);
  clock_val = clock();

  //g_dynamicsWorld->stepSimulation(1/60.f,10);
  g_dynamicsWorld->stepSimulation((float)ms_elapsed/(1000.0f/2.0f),10);

  DrawMovingCube();

  //red back wall
  DrawWall(back_wall,
	   Point(-wall_size/2.0, 0.0,       -back_wall_dist),
	   Point(-wall_size/2.0, wall_size, -back_wall_dist),
	   Point(wall_size/2.0,  wall_size, -back_wall_dist),
	   Point(wall_size/2.0,  0.0,       -back_wall_dist));

 //left wall
  DrawWall(wall_color,
	   Point(-wall_size/2.0, 0.0,       -back_wall_dist + wall_size),
	   Point(-wall_size/2.0, wall_size, -back_wall_dist + wall_size),
	   Point(-wall_size/2.0, wall_size, -back_wall_dist),
	   Point(-wall_size/2.0, 0.0,       -back_wall_dist));

  //right wall
  DrawWall(wall_color,
	   Point(wall_size/2.0, 0.0,       -back_wall_dist + wall_size),
	   Point(wall_size/2.0, wall_size, -back_wall_dist + wall_size),
	   Point(wall_size/2.0, wall_size, -back_wall_dist),
	   Point(wall_size/2.0, 0.0,       -back_wall_dist));

  //floor
  DrawWall(ceil_floor,
	   Point(-wall_size/2.0, 0.0, -back_wall_dist + wall_size),
	   Point(-wall_size/2.0, 0.0, -back_wall_dist),
	   Point(wall_size/2.0,  0.0, -back_wall_dist),
	   Point(wall_size/2.0,  0.0, -back_wall_dist + wall_size));

  //ceiling
  DrawWall(ceil_floor,
	   Point(-wall_size/2.0, wall_size, -back_wall_dist + wall_size),
	   Point(-wall_size/2.0, wall_size, -back_wall_dist),
	   Point(wall_size/2.0,  wall_size, -back_wall_dist),
	   Point(wall_size/2.0,  wall_size, -back_wall_dist + wall_size));

  int collideCount = 0;

  for (std::vector<Sphere*>::iterator sphere = WorldInfo_t::spheres.begin(); sphere != WorldInfo_t::spheres.end(); sphere++) {
    (*sphere)->DrawMe();
    if ((*sphere)->_hasCollided) {
      collideCount++;
    }
  }

  glutSetWindowTitle((char*)("Throw Game | Score: " + to_string<int>(collideCount)).c_str());

  glLightfv(GL_LIGHT0,    GL_POSITION,  g_light_position_1);
  glLightfv(GL_LIGHT1,    GL_POSITION,  g_light_position_2);
  glLightfv(GL_LIGHT2,    GL_POSITION,  g_light_position_3);
}

void render_scene_handler()
{
#ifdef RENDER_ANAGLYPH
  glDrawBuffer(GL_BACK);
  glDrawBuffer(GL_BACK);
#endif
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

#ifdef RENDER_ANAGLYPH
  /* Left eye filter */
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
  switch (g_GlassesType) {
  case REDBLUE:
  case REDGREEN:
  case REDCYAN:
    glColorMask(GL_TRUE,GL_FALSE,GL_FALSE,GL_TRUE);
    break;
  case BLUERED:
    glColorMask(GL_FALSE,GL_FALSE,GL_TRUE,GL_TRUE);
    break;
  case GREENRED:
    glColorMask(GL_FALSE,GL_TRUE,GL_FALSE,GL_TRUE);
    break;
  case CYANRED:
    glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_TRUE);
    break;
  }
#endif

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

#ifdef RENDER_ANAGLYPH
  gluLookAt(-EYE_ZERO_OFFSET,25.0,0.0,
#else
  gluLookAt(0.0,25.0,0.0,
#endif
            0.0,(wall_size / 2.0f) - 0.18*wall_size,-back_wall_dist,
            0.0,1.0,0.0);

  glRotatef(Rotate_t::X,0,1,0);
  glTranslatef(Translate_t::X, Translate_t::Y, Translate_t::Z);

  DrawWorld();

  glFlush();

///////////////////////////////////////////////////////////////////////////

#ifdef RENDER_ANAGLYPH
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);

  glDrawBuffer(GL_BACK);

  glClear(GL_DEPTH_BUFFER_BIT);

  /* Right eye filter */
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
  switch (g_GlassesType) {
  case REDBLUE:
    glColorMask(GL_FALSE,GL_FALSE,GL_TRUE,GL_TRUE);
    break;
  case REDGREEN:
    glColorMask(GL_FALSE,GL_TRUE,GL_FALSE,GL_TRUE);
    break;
  case REDCYAN:
    glColorMask(GL_FALSE,GL_TRUE,GL_TRUE,GL_TRUE);
    break;
  case BLUERED:
  case GREENRED:
  case CYANRED:
    glColorMask(GL_TRUE,GL_FALSE,GL_FALSE,GL_TRUE);
    break;
  }

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

#ifdef RENDER_ANAGLYPH
  gluLookAt(EYE_ZERO_OFFSET,25.0,0.0,
#else
  gluLookAt(0.0,25.0,0.0,
#endif
      0.0,(wall_size / 2.0f) - 0.18*wall_size,-back_wall_dist,
      0.0,1.0,0.0);

  glRotatef(Rotate_t::X,0,1,0);
  glTranslatef(Translate_t::X, Translate_t::Y, Translate_t::Z);

  DrawWorld();

  glFlush();
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
#endif //RENDER_ANAGLYPH

  glutSwapBuffers();
  glutPostRedisplay();
}

void init()
{
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_TEXTURE_2D);
  glDepthFunc(GL_LEQUAL);

  glMaterialfv(GL_LIGHT0, GL_SPECULAR,  g_mat_specular);
  glMaterialfv(GL_LIGHT0, GL_SHININESS, g_mat_shininess);
  glLightfv(GL_LIGHT0,    GL_AMBIENT,   g_light_ambient);
  glLightfv(GL_LIGHT0,    GL_DIFFUSE,   g_light_diffuse);

  glMaterialfv(GL_LIGHT1, GL_SPECULAR,  g_mat_specular);
  glMaterialfv(GL_LIGHT1, GL_SHININESS, g_mat_shininess);
  glLightfv(GL_LIGHT1,    GL_AMBIENT,   g_light_ambient);
  glLightfv(GL_LIGHT1,    GL_DIFFUSE,   g_light_diffuse);

  glMaterialfv(GL_LIGHT2, GL_SPECULAR,  g_mat_specular);
  glMaterialfv(GL_LIGHT2, GL_SHININESS, g_mat_shininess);
  glLightfv(GL_LIGHT2,    GL_AMBIENT,   g_light_ambient);
  glLightfv(GL_LIGHT2,    GL_DIFFUSE,   g_light_diffuse);

  GLfloat spot_direction[] = { 0.0, 0.0, -1.0 };
  glLightfv(GL_LIGHT2, GL_SPOT_DIRECTION, spot_direction);
  glLightf(GL_LIGHT2, GL_SPOT_CUTOFF, 45.0);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  //glEnable(GL_LIGHT1);
  glEnable(GL_LIGHT2);
  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60.0, window_width / window_height, 1.0, 400.0);
}

void reshape_window_handler(GLsizei w, GLsizei h)
{
  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60.0, window_width / window_height, 1.0, 400.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

struct Normal_t
{
  float x;
  float y;
  float z;
};

struct WallInfo_t
{
  Normal_t normal;
  float offset;
  btCollisionShape* shape;
  btDefaultMotionState* motion_state;
  btRigidBody* rigid_body;
};

int main(int argc, char** argv)
{
  json_object* top_obj = json_tokener_parse("{ \"accel\" : [23, 63, 12] }");
  if (is_error(top_obj)) {
    printf("parse error...\n");
  }

  json_object* accel = json_object_object_get(top_obj, "accel");

  for(int i=0; i < json_object_array_length(accel); i++) {
    json_object *obj = json_object_array_get_idx(accel, i);
    //printf("accel[%d]=%d\n", i, json_object_get_int(obj));
  }

  json_object_put(top_obj);

  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
  glutInitWindowSize(window_width, window_height);

  srand(time(NULL));

  gContactProcessedCallback = contact_collision_callback;

  btVector3 worldAabbMin(-10000,-10000,-10000);
  btVector3 worldAabbMax(10000,10000,10000);
  int maxProxies = 1024;
  btAxisSweep3* broadphase = new btAxisSweep3(worldAabbMin,worldAabbMax,maxProxies);

  btDefaultCollisionConfiguration* collisionConfiguration = new btDefaultCollisionConfiguration();
  btCollisionDispatcher* dispatcher = new btCollisionDispatcher(collisionConfiguration);

  btSequentialImpulseConstraintSolver* solver = new btSequentialImpulseConstraintSolver;

  g_dynamicsWorld = new btDiscreteDynamicsWorld(dispatcher,broadphase,solver,collisionConfiguration);

  g_dynamicsWorld->setGravity(btVector3(0,-10,0));

  WallInfo_t walls[] = {
    //left wall
    {{1,0,0}, -wall_size/2.0f, NULL,NULL,NULL},
    //right wall
    {{-1,0,0}, -wall_size/2.0f, NULL,NULL,NULL},
    //ceil
    {{0,-1,0}, -wall_size, NULL,NULL,NULL},
    //floor
    {{0,1,0}, 0, NULL,NULL,NULL},
    //back wall
    {{0,0,1}, -back_wall_dist}
  };

  for (int i=0; i < ArraySize(walls); i++) {
    walls[i].shape = new btStaticPlaneShape(btVector3(walls[i].normal.x, walls[i].normal.y, walls[i].normal.z),walls[i].offset);
    walls[i].motion_state = new btDefaultMotionState();

    btRigidBody::btRigidBodyConstructionInfo groundRigidBodyCI(0,walls[i].motion_state,walls[i].shape,btVector3(0,0,0));
    walls[i].rigid_body = new btRigidBody(groundRigidBodyCI);
    g_dynamicsWorld->addRigidBody(walls[i].rigid_body);
  }

  g_MovingBox = new Box(btVector3(-10,wall_size/2.0f,-back_wall_dist+0.5f*wall_size), 5.0f);

  glutCreateWindow("Throw Game | Score: 0");

  init();

  glutDisplayFunc(render_scene_handler);
  glutReshapeFunc(reshape_window_handler);
  glutKeyboardFunc(process_key_press);
  glutMouseFunc(process_mouse);

  glutMainLoop();

  for (int i=0; i < ArraySize(walls); i++) {
    delete walls[i].motion_state;
    delete walls[i].rigid_body;
    delete walls[i].shape;
  }

  for (std::vector<Sphere*>::iterator sphere = WorldInfo_t::spheres.begin(); sphere != WorldInfo_t::spheres.end(); sphere++) {
    delete *sphere;
  }

  delete g_dynamicsWorld;
  delete solver;
  delete collisionConfiguration;
  delete dispatcher;
  delete broadphase;

  delete g_MovingBox;

  return 0;
}

