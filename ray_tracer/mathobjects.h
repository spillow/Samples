class point;
#include <vector>
using namespace std;


class Vector {
public:
	Vector() {}
  Vector(float x, float y, float z);
  Vector operator*(float scalar);
  Vector operator/(float scalar);
  Vector operator+(Vector& vec2);
  Vector operator+(point& p2);
  Vector operator-(Vector& vec2);
  float  operator[](int index);
  Vector normalize();
  float  length();
  float  dotProduct(Vector& vec2);
  Vector crossProduct(Vector& vec2);
  float px;
  float py;
  float pz;  
	void set(float x, float y, float z);
private:
  float parray[3];
};

class point {
public:
	point();
  point(float x, float y, float z);
  point operator+(Vector& vec2);
  point operator-(Vector& vec2);
  Vector operator-(point& p2);
  point operator*(float scalar);
  float  operator[](int index);
	void set(float x, float y, float z);
  float px;
  float py;
  float pz;  
private:
  float parray[3];
};

class matrix {
public:
  enum MATRIX_INIT {
    ROTATE,
	TRANSLATE,
	SCALE,
	PERSPECTIVE
  };
  matrix(float v0, float v1, float v2, float v3,
	     float v4, float v5, float v6, float v7,
		 float v8, float v9, float v10, float v11,
		 float v12, float v13, float v14, float v15);
  matrix();
  matrix(float* elements);
  matrix(float distance);
  matrix(MATRIX_INIT type, float x, float y, float z);
  float  operator()(int row, int column);
  Vector operator*(Vector& vec);
  point operator*(point& p2);
  matrix operator*(matrix& mat);
  matrix transpose();
  float values[16];
  
private:
};

////////////////////Objects//////////////////////////////

class Color {
public:
	Color() {}
	Color(float red, float green, float blue);
	Color operator*(Color& Color2);
	Color operator+(Color& Color2);
	Color operator*(float scalar);
	float pRed;
	float pGreen;
	float pBlue;
private:
};

class Triangle {
public:
	Triangle() {}
	void set(float p1x, float p1y, float p1z,
		       float p2x, float p2y, float p2z,
					 float p3x, float p3y, float p3z);
	point Vertex[3];
	Vector normal;
private:
};

class Object {
public:
	Object() {}
	float getReflectivity() { return pReflectivity; }
	float getDiffuse() { return pDiffuse; }
	float getRefractivity() { return pRefractivity; }
	Color getColor() { return pColor; }
	Triangle* Triangle_Array;
	void setProperties(float reflectivity, float diffuse, float specular, Color color, float refractivity,
		int numTriangles, Triangle* objectTriangles, float blur, float radius = 0.0, point center = point(0,0,0))
	{
    pReflectivity = reflectivity;
		pDiffuse = diffuse;
		pColor = color;
		pRefractivity = refractivity;
		pNumTriangles = numTriangles;
		//Triangle_Array = new Triangle[numTriangles];
		Triangle_Array = objectTriangles;
		pRadius = radius;
		pCenter = center;
		pSpecularity = specular;
		pBlur = blur;
	}
	int pNumTriangles;
	float getRadius() { return pRadius; }
	point getCenter() { return pCenter; }
	float getSpecular() { return pSpecularity; }
	float getBlur() { return pBlur; }
	bool beenChecked;
	int Object_Index;
private:
	float pReflectivity;
	float pDiffuse;
	float pRefractivity;
	float pSpecularity;
	Color pColor;
	float pRadius;
	point pCenter;
	float pBlur;
};

class Ray {
public:
	Ray(point origin, Vector direction);
	point getOrigin() { return pOrigin; }
	Vector getDirection() { return pDirection; }
  Color rayColor;
	Ray* reflectedRay;
	Ray* refractedRay;
	Ray** distributedRays;
	Ray** refracRays;
  int objectIndex;
	bool inAir;
	Color specularComponent;
private:
	point pOrigin;
  Vector pDirection;
};

class Box {
public:
	Box(float width, float height, float depth, 
		  float transX, float transY, float transZ,
		  float rotX, float rotY, float rotZ);
	void translate(float x, float y, float z);
	void rotateobj(float x, float y, float z);
	//Triangle pTriangles[12];
	Triangle* pTriangles;
private:
	float pTransX;
	float pTransY;
	float pTransZ;
	float pRotX;
	float pRotY;
	float pRotZ;
};

class Cone {
public:
	Cone(float radius, float height, int tess_factor,
		   float transX, float transY, float transZ,
			 float rotX, float rotY, float rotZ);
	void translate(float x, float y, float z);
	void rotateobj(float x, float y, float z);
	Triangle* pTriangles;
	point apex;
private:
	point* triangleStrip;
	point circleCenter;
	int tess_factor;
	float height;
	float pTransX;
	float pTransY;
	float pTransZ;
	float pRotX;
	float pRotY;
	float pRotZ;
};

class Light {
public:
	Light() { areaLightSide = 0.0; }
	Light(point position, Color color, float intensity, bool spot = false, Vector direction = Vector(0,0,0)) 
												{ pPosition = position; pColor = color; pIntensity = intensity;
													pIsSpotLight = spot; pDirection = direction; areaLightSide = 0.0; }
	point getPosition() { 
		if ( areaLightSide > 0.0 )
		{
			float randX = ((rand() / ( (float)RAND_MAX + 1.0 ))*areaLightSide) - (areaLightSide/2.0);
			float randY = ((rand() / ( (float)RAND_MAX + 1.0 ))*areaLightSide) - (areaLightSide/2.0);
			point jitteredPoint(randX,randY,pPosition.pz);
			jitteredPoint.px += pPosition.px;
			jitteredPoint.py += pPosition.py;
			jitteredPoint.pz += pPosition.pz;
			return jitteredPoint;
		}
		return pPosition; 
	}
	Color getColor() { return pColor; }
	float getIntensity() { return pIntensity; }
	bool isSpotLight() { return pIsSpotLight; }
	Vector getDirection() { return pDirection; }
	void set(point position, Color color, float intensity, bool spot = false, Vector direction = Vector(0,0,0)) 
												{ pPosition = position; pColor = color; pIntensity = intensity;
													pIsSpotLight = spot; pDirection = direction; }
	float areaLightSide;
private:
	point pPosition;
	Color pColor;
	float pIntensity;
	bool pIsSpotLight;
	Vector pDirection;
};

class AreaLight {
public:
	AreaLight(point position, float side, int accuracy_factor, float intensity);
	int getNumLights() { return pNumLights; }
	Light* Light_Array;
private:
	int pNumLights;
	float pPosition;
};

class BoundingBox {
public:
	BoundingBox() { for(int i=0; i < 8; i++) {childBoxes[i] = NULL;} numObjects = 0;}
  BoundingBox(point boxMinExtent, point boxMaxExtent)
	{
		this->boxMinExtent = boxMinExtent;
		this->boxMaxExtent = boxMaxExtent;
	}
	void set(point boxMinExtent, point boxMaxExtent)
	{
		this->boxMinExtent = boxMinExtent;
		this->boxMaxExtent = boxMaxExtent;
	}
	vector<Object*> Storage;
	BoundingBox* childBoxes[8];
  int numObjects;
	point boxMinExtent;
	point boxMaxExtent;
private:
};

class Octree {
public:
	Octree() {}
	void OctreeInit(BoundingBox** Box, Object* worldList, int worldNumObjects);
	vector<Object*> getObjects(BoundingBox** Box, Ray ray);
private:
	void traverseOctree(BoundingBox** Box, Ray ray);
	void buildOctree(BoundingBox** Box);
	bool rayBoxIntersection(BoundingBox Box, Ray ray);
	bool boxSphereIntersection(BoundingBox Box, Object sphere);
	bool boxTriangleIntersection(BoundingBox Box, Triangle triangle);
	bool planeBoxOverlap(Vector normal,float d, Vector maxbox);
	//BoundingBox Box;
	int maxObjectsPerBox;
	vector<Object*> objectsToSendBack;
	//int numObjectsToSendBack;
};

class keyFrame {
public:
	keyFrame() {}
	keyFrame(float time, point location, point lastCameraPosition, float lastTime)
	{
    this->time = time;
		this->location = location;
		this->lastCameraPosition = lastCameraPosition;
		this->lastTime = lastTime;
	}
	float time;
	float lastTime;
	point location;
	point lastCameraPosition;
private:
};

class RayTracer {
public:
	RayTracer() { ShadowsOn = true; worldBox = NULL; pSampling = 1; traceDepth = 3; pNumFrames = 1; blurrySamples = 64;
	              blurDepth = 0; }
	~RayTracer() {
		for (int i=0; i < pNumObjects; i++)
		{
			delete Object_Array[i].Triangle_Array;
		}
		delete Object_Array;
		delete Light_Array;
	}
	void rayTrace();
	//void setNumTriangles(int numTriangles) { pNumTriangles = numTriangles; Triangle_Array = new Triangle[pNumTriangles]; }
	void setNumObjects(int numObjects) { pNumObjects = numObjects; Object_Array = new Object[pNumObjects]; }
	void setNumLights(int numLights) { pNumLights = numLights; Light_Array = new Light[pNumLights]; }
	void setResolution(int x, int y) { pResolutionX = x; pResolutionY = y; }
	Triangle findIntersection(Ray ray, bool* missedScene, float* distance, int* objectIndex);
	bool shadowIntersection(Ray ray, float rayDist);
	Color calcLocalLighting(Ray* ray, Triangle triangle, float distance, int objectIndex);
	bool ShadowsOn;
	void setSampling(int sampling) { pSampling = sampling; }
	point screenPoint(point bottomLeftCorner, point topRightCorner, int xPixel, int yPixel);
	void recursiveTrace(Ray** ray, int recursionDepth);
	void traverseRayTree(Ray** node);
	int pNumLights;
	bool loadScene(char* filename);
	bool loadAnimation(char* filename);
	point cameraPosition;
	int traceDepth;
	matrix rotationMatrix;
	int blurrySamples;
	int blurDepth;
private:
	vector<keyFrame> Keys;
	void outputTGA(char* filename);
	Triangle* getMesh(char* filename, int* num_triangles);
	int pNumFrames;
	point bottomLeftCorner;
	point topRightCorner;
	Octree octree;
	BoundingBox* worldBox;
	int pResolutionX;
	int pResolutionY;
	int pSampling;
	//int pNumTriangles;
	int pNumObjects;
	//Triangle* Triangle_Array;
	Object* Object_Array;
	Light* Light_Array;
	bool intersectsTriangle(point vertex, Triangle tri, Vector dir, float* t, float* u, float* v);
	bool intersectsSphere(Ray ray, float* distance, int objectIndex);
	void fillPixel(int column, int row, Color color);
};

