#include "includes.h"

double traverse_time = 0.0;
double traverse_time2 = 0.0;

AreaLight::AreaLight(point position, float side, int accuracy_factor, float intensity)
{
	pNumLights = accuracy_factor * accuracy_factor;

	Light_Array = new Light[pNumLights];

	int lightCount = 0;

	float subSide = side / (float)accuracy_factor;

	//point cornerMiddle((position.px-side/2.0)+subSide/2.0,position.py,(position.pz-side/2.0)+subSide/2.0);
	point cornerMiddle((position.px-side/2.0),position.py,(position.pz-side/2.0));

	srand( time(NULL) );
	float randX, randY;

	for (int i=0; i < accuracy_factor; i++)  //z move
	{
		for (int j=0; j < accuracy_factor; j++) //x move
		{
			randX = ( rand() / ( (float)RAND_MAX + 1.0 ) ) * subSide;
			randY = ( rand() / ( (float)RAND_MAX + 1.0 ) ) * subSide;

		  Light_Array[lightCount].set(point(cornerMiddle.px+((float)j)*subSide + randX,
			                         cornerMiddle.py,
															 cornerMiddle.pz+((float)i)*subSide + randY),
															 Color(1,1,1),intensity / ((float)pNumLights));
			Light_Array[lightCount].areaLightSide = subSide;
			lightCount++;
		}
	}
}

Ray::Ray(point origin, Vector direction)
{
	pOrigin = origin;
	pDirection = direction;
	reflectedRay = NULL;
	refractedRay = NULL;
	distributedRays = NULL;
	refracRays = NULL;
	rayColor = Color(0,0,0);
	objectIndex = -2;
	specularComponent.pRed = 0.0;
  specularComponent.pGreen = 0.0;
	specularComponent.pBlue = 0.0;
	//inAir = true;
}

Color::Color(float red, float green, float blue)
{
  pRed = red;
	pGreen = green;
	pBlue = blue;
}

Color Color::operator*(Color& Color2) {
	return Color(pRed*Color2.pRed, pGreen*Color2.pGreen, pBlue*Color2.pBlue);
}

Color Color::operator+(Color& Color2) {
  return Color(pRed+Color2.pRed, pGreen+Color2.pGreen, pBlue+Color2.pBlue);
}

Color Color::operator*(float scalar) {
	return Color(scalar*pRed, scalar*pGreen, scalar*pBlue);
}

void Triangle::set(float p1x, float p1y, float p1z,
		   float p2x, float p2y, float p2z,
		   float p3x, float p3y, float p3z)
{
  Vertex[0].set(p1x,p1y,p1z);
  Vertex[1].set(p2x,p2y,p2z);
  Vertex[2].set(p3x,p3y,p3z);

	normal = ((Vertex[1]-Vertex[0]).crossProduct(Vertex[2]-Vertex[0])).normalize();
}

Box::Box(float width, float height, float depth,
				 float transX, float transY, float transZ,
				 float rotX, float rotY, float rotZ)
{
  float w = width/2.0;
  float h = height/2.0;
  float d = depth/2.0;

	pTriangles = new Triangle[12];

  pTriangles[0].set(-w,-h,d,
		   w,-h,d,
		   -w,h,d);
  pTriangles[1].set(-w,h,d,
		   w,-h,d,
			 w,h,d);
  pTriangles[2].set(-w,-h,-d,
		   -w,h,-d,
		   w,-h,-d);
  pTriangles[3].set(-w,h,-d,
		   w,h,-d,
		   w,-h,-d);
  pTriangles[4].set(-w,-h,-d,
		   -w,-h,d,
			 -w,h,-d);
  pTriangles[5].set(-w,h,-d,
		   -w,-h,d,
			 -w,h,d);
  pTriangles[6].set(w,-h,-d,
		   w,h,-d,
		   w,-h,d);
  pTriangles[7].set(w,h,-d,
		   w,h,d,
		   w,-h,d);
  pTriangles[8].set(-w,h,d,
		   w,h,d,
			 -w,h,-d);
  pTriangles[9].set(-w,h,-d,
		   w,h,d,
			 w,h,-d);
  pTriangles[10].set(-w,-h,d,
		    -w,-h,-d,
		    w,-h,d);
  pTriangles[11].set(-w,-h,-d,
		    w,-h,-d,
		    w,-h,d);

  /*pTriangles[0].set(-w,-h,d,
		   -w,h,d,
		   w,-h,d);
  pTriangles[1].set(-w,h,d,
		   w,h,d,
		   w,-h,d);
  pTriangles[2].set(-w,-h,-d,
		   -w,h,-d,
		   w,-h,-d);
  pTriangles[3].set(-w,h,-d,
		   w,h,-d,
		   w,-h,-d);
  pTriangles[4].set(-w,-h,-d,
		   -w,h,-d,
		   -w,-h,d);
  pTriangles[5].set(-w,h,-d,
		   -w,h,d,
		   -w,-h,d);
  pTriangles[6].set(w,-h,-d,
		   w,h,-d,
		   w,-h,d);
  pTriangles[7].set(w,h,-d,
		   w,h,d,
		   w,-h,d);
  pTriangles[8].set(-w,h,d,
		   -w,h,-d,
		   w,h,d);
  pTriangles[9].set(-w,h,-d,
		   w,h,-d,
		   w,h,d);
  pTriangles[10].set(-w,-h,d,
		    -w,-h,-d,
		    w,-h,d);
  pTriangles[11].set(-w,-h,-d,
		    w,-h,-d,
		    w,-h,d);*/

	rotateobj(rotX,rotY,rotZ);
	translate(transX,transY,transZ);
}

Cone::Cone(float radius, float height, int tess_factor,
		       float transX, float transY, float transZ,
			     float rotX, float rotY, float rotZ)
{
  triangleStrip = new point[tess_factor];
	this->tess_factor = tess_factor;
	this->height = height;

  float angle_subtended = 2.0*3.14159 / (float)tess_factor;

	apex.set(0.0,0.0,height);
	circleCenter.set(0.0,0.0,0.0);
  for (int i=0; i < tess_factor; i++)
    triangleStrip[i].set(radius*cos((float)i*angle_subtended), radius*sin((float)i*angle_subtended), 0.0);

	rotateobj(rotX,rotY,rotZ);
	translate(transX,transY,transZ);

	//now turn the points into triangles to pass to the Object_Array
  pTriangles = new Triangle[tess_factor*2];

	int triangleNum = 0;

  for (int i=0; i < tess_factor-1; i++)
	{
    pTriangles[triangleNum].set(triangleStrip[i].px,triangleStrip[i].py,triangleStrip[i].pz,
			                          triangleStrip[i+1].px,triangleStrip[i+1].py,triangleStrip[i+1].pz,
																apex.px,apex.py,apex.pz);
		triangleNum++;
    pTriangles[triangleNum].set(triangleStrip[i].px,triangleStrip[i].py,triangleStrip[i].pz,
			                          triangleStrip[i+1].px,triangleStrip[i+1].py,triangleStrip[i+1].pz,
																circleCenter.px,circleCenter.py,circleCenter.pz);
		triangleNum++;
	}

	pTriangles[triangleNum].set(triangleStrip[tess_factor-1].px,triangleStrip[tess_factor-1].py,triangleStrip[tess_factor-1].pz,
			                          triangleStrip[0].px,triangleStrip[0].py,triangleStrip[0].pz,
																apex.px,apex.py,apex.pz);
	triangleNum++;
  pTriangles[triangleNum].set(triangleStrip[tess_factor-1].px,triangleStrip[tess_factor-1].py,triangleStrip[tess_factor-1].pz,
	                            triangleStrip[0].px,triangleStrip[0].py,triangleStrip[0].pz,
															circleCenter.px,circleCenter.py,circleCenter.pz);

	delete triangleStrip;
}

/*
void Cone::Draw()
{
	if (!myCone.isVisible)
		return;

  for (int i=0; i < tess_factor-1; i++)
    {
      glBegin(GL_TRIANGLES);
      glVertex3f(triangleStrip[i].px, triangleStrip[i].py, triangleStrip[i].pz);
      glVertex3f(triangleStrip[i+1].px, triangleStrip[i+1].py, triangleStrip[i+1].pz);
      glVertex3f(apex.px,apex.py,apex.pz);
      glEnd();

      glBegin(GL_TRIANGLES);
      glVertex3f(triangleStrip[i].px, triangleStrip[i].py, triangleStrip[i].pz);
      glVertex3f(triangleStrip[i+1].px, triangleStrip[i+1].py, triangleStrip[i+1].pz);
      glVertex3f(circleCenter.px,circleCenter.py,circleCenter.pz);
      glEnd();
    }  

	  glBegin(GL_TRIANGLES);
    glVertex3f(triangleStrip[tess_factor-1].px, triangleStrip[tess_factor-1].py, triangleStrip[tess_factor-1].pz);
    glVertex3f(triangleStrip[0].px, triangleStrip[0].py, triangleStrip[0].pz);
    glVertex3f(apex.px,apex.py,apex.pz);
    glEnd();

		glBegin(GL_TRIANGLES);
    glVertex3f(triangleStrip[tess_factor-1].px, triangleStrip[tess_factor-1].py, triangleStrip[tess_factor-1].pz);
		glVertex3f(triangleStrip[0].px, triangleStrip[0].py, triangleStrip[0].pz);
		glVertex3f(circleCenter.px, circleCenter.py, circleCenter.pz);
		glEnd();
}
*/

void Box::translate(float x, float y, float z)
{
  matrix trans(matrix::TRANSLATE,x,y,z);
  for (int i=0; i < 12; i++)
	{
    pTriangles[i].Vertex[0] = trans * pTriangles[i].Vertex[0];
		pTriangles[i].Vertex[1] = trans * pTriangles[i].Vertex[1];
		pTriangles[i].Vertex[2] = trans * pTriangles[i].Vertex[2];
	}
}

void Box::rotateobj(float x, float y, float z)
{
	matrix rot(matrix::ROTATE,x,y,z);
  for (int i=0; i < 12; i++)
	{
    pTriangles[i].Vertex[0] = rot * pTriangles[i].Vertex[0];
		pTriangles[i].Vertex[1] = rot * pTriangles[i].Vertex[1];
		pTriangles[i].Vertex[2] = rot * pTriangles[i].Vertex[2];
	}
}

void Cone::translate(float x, float y, float z)
{
	matrix trans(matrix::TRANSLATE,x,y,z);
	apex = trans * apex;
	circleCenter = trans * circleCenter;
	for (int i=0; i < tess_factor; i++)
	  triangleStrip[i] = trans * triangleStrip[i];
}

void Cone::rotateobj(float x, float y, float z)
{
	matrix rot(matrix::ROTATE,x,y,z);

	apex = rot * apex;
	circleCenter = rot * circleCenter;
  for (int i=0; i < tess_factor; i++)
	  triangleStrip[i] = rot * triangleStrip[i];
}

void Octree::OctreeInit(BoundingBox** Box, Object* worldList, int worldNumObjects)
{
	(*Box) = new BoundingBox;
  //(*Box)->set(boxMinExtent, boxMaxExtent);
	//(*Box)->Storage = new Object*[worldNumObjects];
	(*Box)->Storage.resize(worldNumObjects);

	for (int i=0; i < worldNumObjects; i++)
	{
    (*Box)->Storage[i] = &worldList[i];
	}

	//(*Box)->numObjects = worldNumObjects;
	maxObjectsPerBox = 10;

	//initialize the box bounds by going through the objects in the scene and finding the outer bounds
	float xmin = 5000.0;
	float xmax = -5000.0;
	float ymin = 5000.0;
	float ymax = -5000.0;
	float zmin = 5000.0;
	float zmax = -5000.0;

	Vector sphereDispx(1.0,0.0,0.0);
	Vector sphereDispy(0.0,1.0,0.0);
	Vector sphereDispz(0.0,0.0,1.0);

	for (int i=0; i < worldNumObjects; i++)
	{
		if ( worldList[i].getRadius() > 0.0 )
		{
      point xplus = (worldList[i].getCenter() + sphereDispx);
			point xminus = xplus*-1.0;
			point yplus = (worldList[i].getCenter() + sphereDispy);
			point yminus = yplus*-1.0;
			point zplus = (worldList[i].getCenter() + sphereDispz);
			point zminus = zplus*-1.0;
			if ( xplus.px > xmax )
				xmax = xplus.px;
			else if ( xminus.px < xmin )
				xmin = xminus.px;

			if ( yplus.py > ymax )
				ymax = yplus.py;
			else if ( yminus.py < ymin )
				ymin = yminus.py;

			if ( zplus.pz > zmax )
				zmax = zplus.pz;
			else if ( zminus.pz < zmin )
				zmin = zminus.pz;
		}
		else
		{
			for (int j=0; j < worldList[i].pNumTriangles; j++)
			{
				for (int k=0; k < 3; k++)
				{
					point currVertex = worldList[i].Triangle_Array[j].Vertex[k];
					if ( currVertex.px > xmax )
						xmax = currVertex.px;
					else if ( currVertex.px < xmin )
						xmin = currVertex.px;

          if ( currVertex.py > ymax )
						ymax = currVertex.py;
					else if ( currVertex.py < ymin )
						ymin = currVertex.py;

					if ( currVertex.pz > zmax )
						zmax = currVertex.pz;
					else if ( currVertex.pz < zmin )
						zmin = currVertex.pz;
				}
			}
		}
	}

	(*Box)->set(point(xmin,ymin,zmin), point(xmax,ymax,zmax));

	//printf("min: %.2f %.2f %.2f\n max: %.2f %.2f %.2f\n", xmin,ymin,zmin,xmax,ymax,zmax);

  buildOctree(Box);
}

bool Octree::boxSphereIntersection(BoundingBox Box, Object sphere)
{
/*
dmin = 0;
            for( i = 0; i < n; i++ ) {
                if( C[i] < Bmin[i] ) dmin += SQR(C[i] - Bmin[i] ); else
                if( C[i] > Bmax[i] ) dmin += SQR( C[i] - Bmax[i] );     
                }
            if( dmin <= r2 ) return( TRUE );
*/

	float dmin = 0.0;
	float r2 = sphere.getRadius()*sphere.getRadius();

	//X case
  if ( sphere.getCenter().px < Box.boxMinExtent.px )
		dmin += (sphere.getCenter().px - Box.boxMinExtent.px)*(sphere.getCenter().px - Box.boxMinExtent.px);
	else if ( sphere.getCenter().px > Box.boxMaxExtent.px )
    dmin += (sphere.getCenter().px - Box.boxMaxExtent.px)*(sphere.getCenter().px - Box.boxMaxExtent.px);

	//Y case
	if ( sphere.getCenter().py < Box.boxMinExtent.py )
		dmin += (sphere.getCenter().py - Box.boxMinExtent.py)*(sphere.getCenter().py - Box.boxMinExtent.py);
	else if ( sphere.getCenter().py > Box.boxMaxExtent.py )
    dmin += (sphere.getCenter().py - Box.boxMaxExtent.py)*(sphere.getCenter().py - Box.boxMaxExtent.py);

	//Z case
	if ( sphere.getCenter().pz < Box.boxMinExtent.pz )
		dmin += (sphere.getCenter().pz - Box.boxMinExtent.pz)*(sphere.getCenter().pz - Box.boxMinExtent.pz);
	else if ( sphere.getCenter().pz > Box.boxMaxExtent.pz )
    dmin += (sphere.getCenter().pz - Box.boxMaxExtent.pz)*(sphere.getCenter().pz - Box.boxMaxExtent.pz);

	if ( dmin <= r2 )
		return true;

	return false;
}

/*======================== X-tests ========================*/
#define AXISTEST_X01(a, b, fa, fb)			   \
	p0 = a*v0.py - b*v0.pz;			       	   \
	p2 = a*v2.py - b*v2.pz;			       	   \
        if(p0<p2) {min=p0; max=p2;} else {min=p2; max=p0;} \
	rad = fa * boxHalfSize.py + fb * boxHalfSize.pz;   \
	if(min>rad || max<-rad) return false;

#define AXISTEST_X2(a, b, fa, fb)			   \
	p0 = a*v0.py - b*v0.pz;			           \
	p1 = a*v1.py - b*v1.pz;			       	   \
        if(p0<p1) {min=p0; max=p1;} else {min=p1; max=p0;} \
	rad = fa * boxHalfSize.py + fb * boxHalfSize.pz;   \
	if(min>rad || max<-rad) return false;

/*======================== Y-tests ========================*/
#define AXISTEST_Y02(a, b, fa, fb)			   \
	p0 = -a*v0.px + b*v0.pz;		      	   \
	p2 = -a*v2.px + b*v2.pz;	       	       	   \
        if(p0<p2) {min=p0; max=p2;} else {min=p2; max=p0;} \
	rad = fa * boxHalfSize.px + fb * boxHalfSize.pz;   \
	if(min>rad || max<-rad) return false;

#define AXISTEST_Y1(a, b, fa, fb)			   \
	p0 = -a*v0.px + b*v0.pz;		      	   \
	p1 = -a*v1.px + b*v1.pz;	     	       	   \
        if(p0<p1) {min=p0; max=p1;} else {min=p1; max=p0;} \
	rad = fa * boxHalfSize.px + fb * boxHalfSize.pz;   \
	if(min>rad || max<-rad) return false;

/*======================== Z-tests ========================*/

#define AXISTEST_Z12(a, b, fa, fb)			   \
	p1 = a*v1.px - b*v1.py;			           \
	p2 = a*v2.px - b*v2.py;			       	   \
        if(p2<p1) {min=p2; max=p1;} else {min=p1; max=p2;} \
	rad = fa * boxHalfSize.px + fb * boxHalfSize.py;   \
	if(min>rad || max<-rad) return false;

#define AXISTEST_Z0(a, b, fa, fb)			   \
	p0 = a*v0.px - b*v0.py;				   \
	p1 = a*v1.px - b*v1.py;			           \
        if(p0<p1) {min=p0; max=p1;} else {min=p1; max=p0;} \
	rad = fa * boxHalfSize.px + fb * boxHalfSize.py;   \
	if(min>rad || max<-rad) return false;

#define FINDMINMAX(x0,x1,x2,min,max) \
  min = max = x0;   \
  if(x1<min) min=x1;\
  if(x1>max) max=x1;\
  if(x2<min) min=x2;\
  if(x2>max) max=x2;

bool Octree::planeBoxOverlap(Vector normal, float d, Vector maxbox)
{
	//int q;
  //float vmin[3],vmax[3];
	Vector vmin, vmax;
  /*for(q=0;q<=2;q++)
  {
    if(normal[q]>0.0f)
    {
      vmin[q]=-maxbox[q];
      vmax[q]=maxbox[q];
    }
    else
    {
      vmin[q]=maxbox[q];
      vmax[q]=-maxbox[q];
    }
  }*/

	//X case
	if(normal.px > 0.0)
    {
      vmin.px=-maxbox.px;
      vmax.px=maxbox.px;
    }
    else
    {
      vmin.px=maxbox.px;
      vmax.px=-maxbox.px;
    }

	//Y case
	if(normal.py > 0.0)
    {
      vmin.py=-maxbox.py;
      vmax.py=maxbox.py;
    }
    else
    {
      vmin.py=maxbox.py;
      vmax.py=-maxbox.py;
    }

	//Z case
	if(normal.pz > 0.0)
    {
      vmin.pz=-maxbox.pz;
      vmax.pz=maxbox.pz;
    }
    else
    {
      vmin.pz=maxbox.pz;
      vmax.pz=-maxbox.pz;
    }

  /*if(DOT(normal,vmin)+d>0.0f) 
		return false;

  if(DOT(normal,vmax)+d>=0.0f) 
		return true;*/

	if ( normal.dotProduct(vmin) + d > 0.0 )
		return false;

	if ( normal.dotProduct(vmax) + d >= 0.0 )
		return true;
  
  return false;
}

bool Octree::boxTriangleIntersection(BoundingBox Box, Triangle triangle)
{
  //This algorithm is a composition of 13 tests to determine intersection
	//if all pass, there is overlap

	float min,max,d,p0,p1,p2,rad,fex,fey,fez;

  //center the box at (0,0,0) with respect to the triangle

	//translate triangle vertices
	Vector v0,v1,v2;
	//edges of new triangle
	Vector e0,e1,e2;

	//Vector that goes half the diagonal of the bounding box
	Vector boxHalfSize = (Box.boxMaxExtent - Box.boxMinExtent) / 2.0;

	point boxCenter( (Box.boxMinExtent.px + Box.boxMaxExtent.px)/2.0,
                   (Box.boxMinExtent.py + Box.boxMaxExtent.py)/2.0,
									 (Box.boxMinExtent.pz + Box.boxMaxExtent.pz)/2.0 );

	/*v0.set(triangle.Vertex[0].px-boxCenter.px,triangle.Vertex[0].py-boxCenter.py,triangle.Vertex[0].pz-boxCenter.pz);
	v1.set(triangle.Vertex[1].px-boxCenter.px,triangle.Vertex[1].py-boxCenter.py,triangle.Vertex[1].pz-boxCenter.pz);
	v2.set(triangle.Vertex[2].px-boxCenter.px,triangle.Vertex[2].py-boxCenter.py,triangle.Vertex[2].pz-boxCenter.pz);*/

	v0 = triangle.Vertex[0] - boxCenter;
	v1 = triangle.Vertex[1] - boxCenter;
	v2 = triangle.Vertex[2] - boxCenter;

  e0 = v1 - v0;
	e1 = v2 - v1;
	e2 = v0 - v2;

	//the first 9 tests
   fex = fabs(e0.px);
   fey = fabs(e0.py);
   fez = fabs(e0.pz);
   AXISTEST_X01(e0.pz, e0.py, fez, fey);
   AXISTEST_Y02(e0.pz, e0.px, fez, fex);
   AXISTEST_Z12(e0.py, e0.px, fey, fex);

   fex = fabs(e1.px);
   fey = fabs(e1.py);
   fez = fabs(e1.pz);
   AXISTEST_X01(e1.pz, e1.py, fez, fey);
   AXISTEST_Y02(e1.pz, e1.px, fez, fex);
   AXISTEST_Z0(e1.py, e1.px, fey, fex);

   fex = fabs(e2.px);
   fey = fabs(e2.py);
   fez = fabs(e2.pz);
   AXISTEST_X2(e2.pz, e2.py, fez, fey);
   AXISTEST_Y1(e2.pz, e2.px, fez, fex);
   AXISTEST_Z12(e2.py, e2.px, fey, fex);

	 //now the 3 tests
	 //X test
   FINDMINMAX(v0.px,v1.px,v2.px,min,max);
   if(min>boxHalfSize.px || max<-boxHalfSize.px) 
		 return false;

   //Y test
   FINDMINMAX(v0.py,v1.py,v2.py,min,max);
   if(min>boxHalfSize.py || max<-boxHalfSize.py) 
		 return false;

   //Z test
   FINDMINMAX(v0.pz,v1.pz,v2.pz,min,max);
   if(min>boxHalfSize.pz || max<-boxHalfSize.pz) 
		 return false;

	Vector normal = e0.crossProduct(e1);
	d = -(normal.dotProduct(v0));

	if(!planeBoxOverlap(normal,d,boxHalfSize)) 
		return false;

	return true;
}

void Octree::buildOctree(BoundingBox** Box)
{
  //if we get below the threshold number of objects in a box, don't subdivide any longer
	if ( (*Box)->Storage.size() <= maxObjectsPerBox )
		return;

	//(*Box)->childBoxes = new BoundingBox*[8];



	for (int i=0; i < 8; i++)
	{
		(*Box)->childBoxes[i] = new BoundingBox;
	}

	int count = 0;

	Vector displacement;

	point boxCenter( ((*Box)->boxMinExtent.px + (*Box)->boxMaxExtent.px)/2.0,
                   ((*Box)->boxMinExtent.py + (*Box)->boxMaxExtent.py)/2.0,
									 ((*Box)->boxMinExtent.pz + (*Box)->boxMaxExtent.pz)/2.0 );

	Vector diagonal = ((*Box)->boxMaxExtent - (*Box)->boxMinExtent) / 2.0;

	//if not, subdivide box into eight equal octants
  for (int z=0; z < 2; z++)
	{
		for (int y=0; y < 2; y++)
		{
			for (int x=0; x < 2; x++)
			{
				displacement.set(diagonal.px*(float)x, diagonal.py*(float)y, diagonal.pz*(float)z);
        (*Box)->childBoxes[count++]->set((*Box)->boxMinExtent+displacement, boxCenter+displacement);
			}
		}
	}

	//now, see whether the children lie inside the child boxes
  //this requires a triangle-box intersection and sphere-box intersection tests
	for (int i=0; i < (*Box)->Storage.size(); i++)
	{
		for (int k=0; k < 8; k++)
		{
		  if ( (*Box)->Storage[i]->getRadius() > 0.0 )
		  {
				if ( boxSphereIntersection(*((*Box)->childBoxes[k]), *((*Box)->Storage[i])) )
				{
					//(*Box)->childBoxes[k]->numObjects++;
					(*Box)->childBoxes[k]->Storage.push_back((*Box)->Storage[i]);
				}
		  }
		  else
		  {
			  for (int j=0; j < (*Box)->Storage[i]->pNumTriangles; j++)
		    {
		      if ( boxTriangleIntersection(*((*Box)->childBoxes[k]), (*Box)->Storage[i]->Triangle_Array[j]) )
					{
					  //add obj inc numObj, break
            (*Box)->childBoxes[k]->Storage.push_back((*Box)->Storage[i]);
						//(*Box)->childBoxes[k]->numObjects++;
					  break;
					}
		    }
		  }
		}
	}

	//now, recursively go to each of the children and see if they need to be subdivided
	for (int i=0; i < 8; i++)
	{
		buildOctree(&((*Box)->childBoxes[i]));
	}
}

bool Octree::rayBoxIntersection(BoundingBox Box, Ray ray)
{
	float tnear = -5000;
	float tfar = 5000;

	float tIntersectMin, tIntersectMax;

	//three tests will be done for each pair of planes (X,Y,Z)

	//X tests
	if ( ray.getDirection().px < 0.0001 && ray.getDirection().px > -0.0001 ) //ray parallel to yz-plane
	{
		if ( (ray.getOrigin().px < Box.boxMinExtent.px) || (ray.getOrigin().px > Box.boxMaxExtent.px) )
			return false;
	}

  tIntersectMin = ( Box.boxMinExtent.px - ray.getOrigin().px ) / ray.getDirection().px;
	tIntersectMax = ( Box.boxMaxExtent.px - ray.getOrigin().px ) / ray.getDirection().px;

	if ( tIntersectMin  > tIntersectMax )
	{
		float temp = tIntersectMin;
		tIntersectMin = tIntersectMax;
		tIntersectMax = temp;
	}

  if ( tIntersectMin > tnear )
		tnear = tIntersectMin;

	if ( tIntersectMax < tfar )
		tfar = tIntersectMax;

	if ( tnear > tfar )
		return false;

	if ( tfar < 0.0 )
		return false;

	//Y tests
	if ( ray.getDirection().py < 0.0001 && ray.getDirection().py > -0.0001 ) //ray parallel to yz-plane
	{
		if ( (ray.getOrigin().py < Box.boxMinExtent.py) || (ray.getOrigin().py > Box.boxMaxExtent.py) )
			return false;
	}

  tIntersectMin = ( Box.boxMinExtent.py - ray.getOrigin().py ) / ray.getDirection().py;
	tIntersectMax = ( Box.boxMaxExtent.py - ray.getOrigin().py ) / ray.getDirection().py;

	if ( tIntersectMin  > tIntersectMax )
	{
		float temp = tIntersectMin;
		tIntersectMin = tIntersectMax;
		tIntersectMax = temp;
	}

  if ( tIntersectMin > tnear )
		tnear = tIntersectMin;

	if ( tIntersectMax < tfar )
		tfar = tIntersectMax;

	if ( tnear > tfar )
		return false;

	if ( tfar < 0.0 )
		return false;

	//Z tests
	if ( ray.getDirection().pz < 0.0001 && ray.getDirection().pz > -0.0001 ) //ray parallel to yz-plane
	{
		if ( (ray.getOrigin().pz < Box.boxMinExtent.pz) || (ray.getOrigin().pz > Box.boxMaxExtent.pz) )
			return false;
	}

  tIntersectMin = ( Box.boxMinExtent.pz - ray.getOrigin().pz ) / ray.getDirection().pz;
	tIntersectMax = ( Box.boxMaxExtent.pz - ray.getOrigin().pz ) / ray.getDirection().pz;

	if ( tIntersectMin  > tIntersectMax )
	{
		float temp = tIntersectMin;
		tIntersectMin = tIntersectMax;
		tIntersectMax = temp;
	}

  if ( tIntersectMin > tnear )
		tnear = tIntersectMin;

	if ( tIntersectMax < tfar )
		tfar = tIntersectMax;

	if ( tnear > tfar )
		return false;

	if ( tfar < 0.0 )
		return false;

	//if it can get through all the tests, then we have intersection
	return true;
}

void Octree::traverseOctree(BoundingBox** Box, Ray ray)
{
  if ( (*Box) == NULL )
		return;

	//ray box intersection (if ray does not intersect box, return)
	clock_t start2, finish2;
	start2 = clock();
	bool rayBox = rayBoxIntersection(**Box, ray);
	finish2 = clock();
  traverse_time2 += (finish2-start2) / (double)CLOCKS_PER_SEC;
  //if ( !rayBoxIntersection(**Box, ray) )
	if ( !rayBox )
	{
		return;
	}

	if ( (*Box)->childBoxes[0] == NULL ) //then this is a leaf node
	{
    //numObjectsToSendBack += (*Box)->numObjects;
    //add to the pot (if the object isn't tagged as already been added)
    for (int i=0; i < (*Box)->Storage.size(); i++)
		{
			if ( !((*Box)->Storage[i]->beenChecked) )
			{
				objectsToSendBack.push_back( (*Box)->Storage[i] );
				(*Box)->Storage[i]->beenChecked = true;
			}
		}
    return;
	}
  
	for (int i=0; i < 8; i++)
	{
		traverseOctree(&((*Box)->childBoxes[i]),ray);
	}
}

vector<Object*> Octree::getObjects(BoundingBox** Box, Ray ray)
{
	objectsToSendBack.resize(0);

	clock_t start1, finish1;

	start1 = clock();

	traverseOctree(Box, ray);

	finish1 = clock();

	traverse_time += (finish1-start1) / (double)CLOCKS_PER_SEC;

	//printf("octime: %lf\n", (finish-start) / (double)CLOCKS_PER_SEC);

	//printf("sending: %d\n", objectsToSendBack.size());

	return objectsToSendBack;
}