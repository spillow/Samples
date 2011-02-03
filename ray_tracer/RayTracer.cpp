#include "includes.h"

const float GLASS = 1.5;
const float AIR = 1.00;
const float ambient_coeff = 0.1;
const float normal_flipper = 1.0;

GLubyte* mybuffer = NULL;

extern double traverse_time;
extern double traverse_time2;
double traverse_time3 = 0.0;

extern GLUI_Checkbox* check_output_file;
extern GLUI_EditText* Output_Textbox;

extern bool animLoaded;

point screenPoint(point bottomLeftCorner, point topRightCorner, int xPixel, int yPixel)
{
  return point(0,0,0);
}

void RayTracer::rayTrace()
{
	/* Basic Algorithm:
  For each pixel
	Construct ray from camera through pixel
	Find first primitive hit by ray
	Determine color at intersection point
	Draw color
	*/

	//initialize scene with just box for now
	/*Box tempBox(2.0,2.0,2.0,
      -2.0,0.0,-4.0,
		  3.14159/4.0,3.14159/4.0,3.14159/4.0);

	Box tempBox1(2.0,2.0,2.0,
      2.0,1.0,-6.0,
		  3.14159/4.0,3.14159/4.0,3.14159/4.0);

	Box tempBox2(20.0,0.1,20.0,
      0.0,-2.0,-6.0,
		  3.14159/8.0,0,0);*/

	/*Box tempBox(2.0,2.0,2.0,
      -2.0,0.0,-10.0,
		  0,-3.14159/8.0,0);

	Box tempBox1(2.0,2.0,2.0,
      2.0,0.0,-10.0,
		  0,3.14159/4.0,0);

	Box tempBox2(20.0,0.1,30.0,
      0.0,-1.0,-0.0,
		  0,0,0);

	Box tempBox3(20.0,20.0,0.1,
		0.0,0.0,-13.0,0,0,0);

	Box tempBox4(2.0,2.0,2.0,
		-3.0,0.0,-5.0,0,0,0);*/

	//pNumObjects = 3;
	//pNumLights = 3;

	//Light light1(point(-5.0,0.0,-9.0),Color(1,1,1), 1.0);
	//Light light2(point(0.0,1.0,-7.0),Color(1,1,1), 7.0);
	//Light light3(point(5.0,0.0,-9.0),Color(1,1,1), 1.0);

	//Light light1(point(-5.0,0.0,-9.0),Color(1,1,1), 1.0);
	//Light light2(point(0.0,1.0,-7.0),Color(1,1,1), 7.0);
	//Light light3(point(5.0,0.0,-9.0),Color(1,1,1), 1.0);

  //AreaLight bigLight(point(0,100.0,-5.6),4.0,4,0.01);
	//Light light2(point(-2.0,2.0,-2.5),Color(1,1,1), 3.0);
	//Light light3(point(1.0,0.5,-2.0),Color(1,1,1), 5.0,true, Vector(1,-1,-1));
	//Light light3(point(0.0,1.0,-1.0),Color(1,1,1), 4.0);
	//Light light4(point(0.0,2.0,-5.0),Color(1,1,1), 10.0);

	//Object_Array[0].Triangle_Array = tempBox.pTriangles;
	//even if object is sphere, just generate triangles

	/*Object_Array[0].setProperties(0.0,1.0,0.0,Color(.8,.1,0),0.0,12,tempBox.pTriangles);
	Object_Array[1].setProperties(0.0,1.0,0.0,Color(0,.8,.1),0.0,12,tempBox1.pTriangles);
	Object_Array[2].setProperties(0.0,1.0,0.0,Color(0,0,1),0.0,12,tempBox2.pTriangles);
	Object_Array[3].setProperties(1.0,1.0,2.0,Color(1,1,0),0.0,1,new Triangle(),1.0,point(1.0,0.0,-7.5));
	Object_Array[4].setProperties(0.0,1.0,0.0,Color(0,1,1),0.0,12,tempBox3.pTriangles);
	Object_Array[5].setProperties(1.0,1.0,3.0,Color(1,1,0),0.0,1,new Triangle(),1.0,point(2.0,0.0,-4.5));
	//Object_Array[6].setProperties(0.0,1.0,0.0,Color(1,1,0),1.0,1,new Triangle(),1.0,point(1.3,0.0,-3.0));
	Object_Array[6].setProperties(0.0,1.0,0.0,Color(.6,.2,.4),0.0,1,new Triangle(),1.0,point(0.0,0.0,-4.0));
  Object_Array[7].setProperties(0.0,1.0,0.0,Color(.5,.5,.5),0.0,12,tempBox4.pTriangles);
	Light_Array[0] = light1;
  Light_Array[1] = light2;
	Light_Array[2] = light3;*/

	/*for (int i=3; i < 19; i++)
	{
		Light_Array[i] = bigLight.Light_Array[i-3];
	}*/
	//Light_Array[3] = light4;

	//the viewing plane will arbitrarily be set to [(-10 10), (10 -10)]

	//point cameraPosition(3.0,0.0,5.0);
	//matrix rotMatrix(matrix::ROTATE,-3.14159/32.0,0.0,0.0);

	//specifies bottom left corner of viewing plane in world coordinates
	//point bottomLeftCorner(-5.0,-5.0,0.0);
	//point topRightCorner(5.0,5.0,0.0);

	//calculate the step between each pixel
	float dx = (topRightCorner.px - bottomLeftCorner.px) / (float)pResolutionX;
	float dy = (topRightCorner.py - bottomLeftCorner.py) / (float)pResolutionY;

	int subPixelStep = (int)sqrt((float)pSampling);

	float subPixelLengthX = dx / (float)subPixelStep;
  float subPixelLengthY = dy / (float)subPixelStep;

	srand( time(NULL) );

	//initialize the octree
	//Octree octree(&worldBox,Object_Array,pNumObjects);

	clock_t start, finish;

	start = clock();
			  
	octree.OctreeInit(&worldBox,Object_Array,pNumObjects);

	finish = clock();
	//printf("octime: %lf\n", (finish-start) / (double)CLOCKS_PER_SEC);

	char currFile[255];

	int keyFramePosition = 0;

	//point currBotCorner = bottomLeftCorner;
	//point currTopCorner = topRightCorner;

	Vector bottomCornerDisp = bottomLeftCorner - cameraPosition;
	Vector topCornerDisp = topRightCorner - cameraPosition;

	for (int k=0; k < pNumFrames; k++) 
	{
	  //set the new camera position
		Vector disp;
    if ( ((float)k / 25.0) > Keys[keyFramePosition].time )
		{
			keyFramePosition++;
		}

		if ( Keys[keyFramePosition].time > 0.0 )
		{
			disp = (Keys[keyFramePosition].location - Keys[keyFramePosition].lastCameraPosition) *
			 (( ((float)k/25.0) - Keys[keyFramePosition].lastTime ) / (Keys[keyFramePosition].time - Keys[keyFramePosition].lastTime));

      cameraPosition = Keys[keyFramePosition].lastCameraPosition + disp;
			bottomLeftCorner = cameraPosition + bottomCornerDisp;
			topRightCorner = cameraPosition + topCornerDisp;
		}


		//now go through for loop and generate a ray to go through each pixel
		for (int i=0; i < pResolutionX; i++)
		{
			for (int j=0; j < pResolutionY; j++)
			{
				Color pixelColor(0,0,0);
				for (int x=0; x < subPixelStep; x++)
				{
					for (int y=0; y < subPixelStep; y++)
					{
						//printf("X: %d Y: %d\n", i, j);
						//point centerPixel;
						//centerPixel.set(bottomLeftCorner.px + 0.5*dx + (float)i*dx,
			    									//bottomLeftCorner.py + 0.5*dy + (float)j*dy, 0.0);
						point centerPixel;
						if ( pSampling > 4 ) {
							float randX = ( (float)rand() / ((float)RAND_MAX + 1.0) )*(subPixelLengthX/2.0) - (subPixelLengthX/4.0);
							float randY = ( (float)rand() / ((float)RAND_MAX + 1.0) )*(subPixelLengthY/2.0) - (subPixelLengthY/4.0);

							/*point centerPixel(bottomLeftCorner.px + (float)i*dx + subPixelLengthX*(float)x + randX,
															bottomLeftCorner.py + (float)j*dy + subPixelLengthY*(float)y + randY, topRightCorner.pz);*/
							centerPixel.set(bottomLeftCorner.px + (float)i*dx + subPixelLengthX*(float)x + subPixelLengthX/2.0 + randX,
															bottomLeftCorner.py + (float)j*dy + subPixelLengthY*(float)y + subPixelLengthY/2.0 + randY, topRightCorner.pz);
						}
						else {
							/*float distFromCenter = (point(bottomLeftCorner.px + (float)i*dx + subPixelLengthX*(float)x + subPixelLengthX/2.0,
																					 bottomLeftCorner.py + (float)j*dy + subPixelLengthY*(float)y + subPixelLengthY/2.0,
																					 topRightCorner.pz) - centerPixel).length();*/

							centerPixel.set(bottomLeftCorner.px + (float)i*dx + subPixelLengthX*(float)x + subPixelLengthX/2.0,
															bottomLeftCorner.py + (float)j*dy + subPixelLengthY*(float)y + subPixelLengthY/2.0, topRightCorner.pz);
						}

		  			Vector direction = (centerPixel - cameraPosition).normalize();

						direction = rotationMatrix * direction;

						//direction = rotMatrix * direction;

		  			//Ray ray(centerPixel, direction);
						Ray* ray = new Ray(cameraPosition, direction);
						ray->inAir = true;

						//Ray ray(cameraPosition, direction);

						//initialize children
						ray->reflectedRay = NULL;
						ray->refractedRay = NULL;

						recursiveTrace(&ray, traceDepth);

						traverseRayTree(&ray);

						if (ray->rayColor.pRed > 1.0) ray->rayColor.pRed = 1.0;
						if (ray->rayColor.pGreen > 1.0) ray->rayColor.pGreen = 1.0;
						if (ray->rayColor.pBlue > 1.0) ray->rayColor.pBlue = 1.0;

						//float filterRadius = (subPixelLengthX*subPixelLengthX + subPixelLengthY*subPixelLengthY) / 2.0;
						//Color filteredColor = ray->rayColor*((1.0/filterRadius)*(cos(2.0*3.14159*distFromCenter/filterRadius)+1.0));

						pixelColor = pixelColor + ray->rayColor;
						//pixelColor = pixelColor + filteredColor;

						delete ray;
					}
				}
				fillPixel(i,j,pixelColor*(1.0/(float)pSampling));
			}
		}
		//sprintf(currFile, "C:\\Documents and Settings\\spillow\\Desktop\\images\\image%05d.tga", k);
		sprintf(currFile, "%simage%05d.tga",Output_Textbox->get_text(),k);
		if ( check_output_file->get_int_val() || animLoaded )
			outputTGA(currFile);

		printf("%.2f Complete\n", ((float)k / (float)pNumFrames)*100.0);
	}
	//printf("traverse time: %lf\n", traverse_time);
	//printf("rayBox time: %lf\n", traverse_time2);
	//printf("getObject time: %lf\n", traverse_time3);
	printf("done!\n");
	
}

void RayTracer::traverseRayTree(Ray** node)
{
	//just a check
  if ( *node == NULL )
		return;

	//if ( (*node)->reflectedRay != NULL )
	//{
		//printf("moving in\n");
    traverseRayTree(&((*node)->reflectedRay));
	//}

  //if ( (*node)->refractedRay != NULL )
    traverseRayTree(&((*node)->refractedRay));

		if ( (*node)->distributedRays != NULL )
		{
			for (int i=0; i < blurrySamples; i++)
			{
				traverseRayTree(&((*node)->distributedRays[i]));
			}
		}

		if ( (*node)->refracRays != NULL )
		{
			for (int i=0; i < blurrySamples; i++)
			{
				traverseRayTree(&((*node)->refracRays[i]));
			}
		}

	//if leaf node, local lighting already calculated and no refraction or reflection
	if ( ((*node)->reflectedRay == NULL) && ((*node)->refractedRay == NULL) )
	{
		(*node)->rayColor = (*node)->rayColor + (*node)->specularComponent;
		return;
	}

	//scaled local lighting and ambient
  (*node)->rayColor = (*node)->rayColor * (1.0 - Object_Array[(*node)->objectIndex].getReflectivity() 
			                       - Object_Array[(*node)->objectIndex].getRefractivity()); //+ Object_Array[(*node)->objectIndex].getColor()*ambient_coeff;

	//add specularity
	(*node)->rayColor = (*node)->rayColor + (*node)->specularComponent;

  //if not a leaf node
  if ( ((*node)->reflectedRay != NULL) )
	{
		//printf("adding reflection\n");
		if ( (*node)->distributedRays == NULL )
      (*node)->rayColor = (*node)->rayColor + ((*node)->reflectedRay->rayColor * Object_Array[(*node)->objectIndex].getReflectivity());
		else
      (*node)->rayColor = (*node)->rayColor + ((*node)->reflectedRay->rayColor*(1.0/(float)(blurrySamples+1))* Object_Array[(*node)->objectIndex].getReflectivity());

		delete (*node)->reflectedRay;

		//if reflecting, may also be blurry
		if ( (*node)->distributedRays != NULL )
		{
			for (int i=0; i < blurrySamples; i++)
			{
				(*node)->rayColor = (*node)->rayColor + ((*node)->distributedRays[i]->rayColor*(1.0/(float)(blurrySamples+1)) * 
					Object_Array[(*node)->objectIndex].getReflectivity());
				delete (*node)->distributedRays[i];
			}
			delete (*node)->distributedRays;
		}
	}

	if ( ((*node)->refractedRay != NULL) )
	{
		if ( (*node)->refracRays == NULL )
			(*node)->rayColor = (*node)->rayColor + ((*node)->refractedRay->rayColor * Object_Array[(*node)->objectIndex].getRefractivity());
		else
      (*node)->rayColor = (*node)->rayColor + ((*node)->refractedRay->rayColor*(1.0/(float)(blurrySamples+1)) * Object_Array[(*node)->objectIndex].getRefractivity());

		delete (*node)->refractedRay;

		//if refracting, may also be blurry
		if ( (*node)->refracRays != NULL )
		{
			for (int i=0; i < blurrySamples; i++)
			{
				(*node)->rayColor = (*node)->rayColor + ((*node)->refracRays[i]->rayColor*(1.0/(float)(blurrySamples+1)) * 
					Object_Array[(*node)->objectIndex].getRefractivity());
				delete (*node)->refracRays[i];
			}
			delete (*node)->refracRays;
		}
	}
}

void RayTracer::recursiveTrace(Ray** ray, int recursionDepth)
{
/*
					recursiveTrace(ray, recursionDepth):
					  if recursionDepth = 0
					    return
					we have a ray
					check it for nearest object intersection
					if missed scene
						return
					else
					  find intersection point
            if object hit reflection_coeff > 0
						  left child gets reflected ray
							reflect ray left, right = null
						  recursiveTrace(rayReflected, --recursionDepth)

						if object hit refraction_coeff > 0
						  right child gets refracted ray
						  refracted ray left, right = null
						  recursiveTrace(rayRefracted, --recursionDepth)

						return
					*/

  bool missedScene;
  Triangle intersectedTriangle;

  float distance = 5000.0;
  int objectIndex = -1;

  intersectedTriangle = findIntersection(**ray, &missedScene, &distance, &objectIndex);

	(*ray)->objectIndex = objectIndex;

	//float diffuseAmount = 1.0 - Object_Array[objectIndex].getReflectivity() - Object_Array[objectIndex].getRefractivity();

	if (!missedScene)// && ( diffuseAmount != 0.0 ) )
	{
		(*ray)->rayColor = calcLocalLighting(*ray, intersectedTriangle, distance, objectIndex);
			                 //+ Object_Array[objectIndex].getColor()*ambient_coeff;
	}

	if ( missedScene || (recursionDepth == 0)  || ( (Object_Array[objectIndex].getReflectivity() == 0.0)  && 
		                                             (Object_Array[objectIndex].getRefractivity() == 0.0) ) ) 
    return;

	if ( Object_Array[objectIndex].getReflectivity() > 0.0 )
	{
		//point intersection_point = (*ray)->getOrigin() + (*ray)->getDirection()*(distance*0.999);
		point intersection_point = (*ray)->getOrigin() + (*ray)->getDirection()*distance;
		Vector normal;

		if ( Object_Array[objectIndex].getRadius() > 0.0 ) {
		  normal = (intersection_point-Object_Array[objectIndex].getCenter()).normalize();

			//normal needs to go other way if reflecting off inside of object
			if ( !((*ray)->inAir) )
				normal = normal*-1.0;

			//move intersection to avoid floating-point error
			intersection_point = intersection_point + normal*0.001;
		}
		else {
		  /*normal = ((intersectedTriangle.Vertex[1]-intersectedTriangle.Vertex[0]).crossProduct(
			                 intersectedTriangle.Vertex[2]-intersectedTriangle.Vertex[0])*normal_flipper).normalize();*/
			normal = intersectedTriangle.normal;
			if ( normal.dotProduct((*ray)->getDirection()) > 0.0 )
				normal = normal*-1.0;

			/*if ( !((*ray)->inAir) )
				normal = normal*-1.0;*/

			//floating check
			intersection_point = intersection_point + normal*0.001;
		}

		Vector reflection = ( (*ray)->getDirection() - normal*((((*ray)->getDirection()).dotProduct(normal))*2.0)).normalize();
		//(*ray)->reflectedRay = new Ray(intersection_point,( (*ray)->getDirection() - normal*((((*ray)->getDirection()).dotProduct(normal))*2.0)).normalize());
		(*ray)->reflectedRay = new Ray(intersection_point,reflection);
		(*ray)->reflectedRay->reflectedRay = NULL;
		(*ray)->reflectedRay->refractedRay = NULL;
		(*ray)->reflectedRay->inAir = (*ray)->inAir;
		recursionDepth--;
		if ( ((recursionDepth+1) >= (traceDepth-blurDepth)) && Object_Array[objectIndex].getBlur() > 0.0 )
		{
			(*ray)->distributedRays = new Ray*[blurrySamples];
			for (int i=0; i < blurrySamples; i++)
			{
				//generate a jittered ray in the traced cone
				//make two perpendicular vectors to the reflected vector
				float coneRadius = Object_Array[objectIndex].getBlur();
				float xscale;
				float yscale;
				Vector Perp1(reflection.pz,reflection.py,-reflection.px);
				Vector Perp2 = reflection.crossProduct(Perp1);

				do {
					xscale = ( rand() / ( (float)RAND_MAX + 1.0 ) ) * coneRadius;
					yscale = ( rand() / ( (float)RAND_MAX + 1.0 ) ) * coneRadius;
				}while( (xscale*xscale + yscale*yscale) > ( coneRadius*coneRadius ) );

				Vector coneRay = reflection + Perp1*xscale + Perp2*yscale*coneRadius;
				coneRay.normalize();
				(*ray)->distributedRays[i] = new Ray(intersection_point,coneRay);
				//recursiveTrace
				recursiveTrace(&((*ray)->distributedRays[i]), recursionDepth);
			}
		}
		recursiveTrace(&((*ray)->reflectedRay), recursionDepth);
		recursionDepth++;
	}

	//printf("getting past it\n");
	if ( Object_Array[objectIndex].getRefractivity() > 0.0 ) 
	{
		//printf("we have ref\n");
		//need to extend intersection to make sure ray pushes through the glass
		//point intersection_point = (*ray)->getOrigin() + (*ray)->getDirection()*(distance*1.001);
		point intersection_point = (*ray)->getOrigin() + (*ray)->getDirection()*distance;
		Vector normal;
		float u1,u2; //refractive indices

    if ( Object_Array[objectIndex].getRadius() > 0.0 ) {
		  //normal = (intersection_point-Object_Array[objectIndex].getCenter()).normalize();
			//u1 = GLASS; u2 = AIR;
			normal = (Object_Array[objectIndex].getCenter()-intersection_point).normalize();
			if ( normal.dotProduct((*ray)->getDirection()) < 0.0 )
				normal = normal*-1.0;

			if ( (*ray)->inAir )
			{
				u1 = AIR; u2 = GLASS;
				//normal = (Object_Array[objectIndex].getCenter()-intersection_point).normalize();
				//(*ray)->inAir = false;
			}
			else
			{
        u1 = GLASS; u2 = AIR;
				//normal = (intersection_point-Object_Array[objectIndex].getCenter()).normalize();
				//(*ray)->inAir = true;
			}
		}
		else {
		  /*normal = ((intersectedTriangle.Vertex[1]-intersectedTriangle.Vertex[0]).crossProduct(
			                 intersectedTriangle.Vertex[2]-intersectedTriangle.Vertex[0])*normal_flipper).normalize();*/
			normal = intersectedTriangle.normal;
			if ( normal.dotProduct((*ray)->getDirection()) < 0.0 )
				normal = normal*-1.0;
			//u1 = GLASS; u2 = AIR;
			if ( (*ray)->inAir )
			{
				u1 = AIR; u2 = GLASS;
				//(*ray)->inAir = false;
			}
			else
			{
        u1 = GLASS; u2 = AIR;
				//normal = normal*-1.0;
				//(*ray)->inAir = true;
			}
			/*if ( (*ray)->inAir )
			{
				u1 = AIR; u2 = GLASS;
				normal = normal*-1.0;
			}*/
		}
    Vector n1 = (*ray)->getDirection();
		float dot = n1.dotProduct(normal);
		float underRoot = u2*u2 - u1*u1 + dot*dot;
		//Vector refRay = n1 - normal*(dot) + normal*(sqrt(u2*u2 - u1*u1 + dot*dot));
		if ( underRoot < 0.0 )
			return;

		Vector refRay = n1 - normal*(dot) + normal*(sqrt(underRoot));
		//an approximation
		//Vector refRay = n1 + normal*(u2-u1);

		//floating back
    intersection_point = intersection_point + normal*0.001;

    (*ray)->refractedRay = new Ray(intersection_point, refRay);
		(*ray)->refractedRay->reflectedRay = NULL;
		(*ray)->refractedRay->refractedRay = NULL;
		(*ray)->refractedRay->inAir = !(*ray)->inAir;
	  recursionDepth--;

		if ( ((recursionDepth+1) >= (traceDepth-blurDepth)) && Object_Array[objectIndex].getBlur() > 0.0 )
		{
			(*ray)->refracRays = new Ray*[blurrySamples];
			for (int i=0; i < blurrySamples; i++)
			{
				//generate a jittered ray in the traced cone
				//make two perpendicular vectors to the reflected vector
				float coneRadius = Object_Array[objectIndex].getBlur();
				float xscale;
				float yscale;
				Vector Perp1(refRay.pz,refRay.py,-refRay.px);
				Vector Perp2 = refRay.crossProduct(Perp1);

				do {
					xscale = ( rand() / ( (float)RAND_MAX + 1.0 ) ) * coneRadius;
					yscale = ( rand() / ( (float)RAND_MAX + 1.0 ) ) * coneRadius;
				}while( (xscale*xscale + yscale*yscale) > ( coneRadius*coneRadius ) );

				Vector coneRay = refRay + Perp1*xscale + Perp2*yscale*coneRadius;
				coneRay.normalize();
				(*ray)->refracRays[i] = new Ray(intersection_point,coneRay);
				(*ray)->refracRays[i]->inAir = !(*ray)->inAir;
				//recursiveTrace
				recursiveTrace(&((*ray)->refracRays[i]), recursionDepth);
			}
		}

		recursiveTrace(&((*ray)->refractedRay), recursionDepth);
	}

	return;
}

bool RayTracer::intersectsTriangle(point vertex, Triangle tri, Vector dir, float* t, float* u, float* v)
{
	const float EPSILON = 0.0000001;
	Vector edge1 = tri.Vertex[1] - tri.Vertex[0];
	Vector edge2 = tri.Vertex[2] - tri.Vertex[0];
	Vector pVec = dir.crossProduct(edge2);
	float det = edge1.dotProduct(pVec);

	//printf("edge1: %.2f\n", edge1.px, edge1.py, edge1.pz);
	//printf("pvec: %.2f %.2f %.2f\n", pVec.px, pVec.py, pVec.pz);

	//printf("%.2f\n", det);

	//if (det < EPSILON)
	if ( det < EPSILON && det > -EPSILON)
		return false;

  float inv_det = 1.0 / det;

	Vector tVec = vertex - tri.Vertex[0];

	*u = tVec.dotProduct(pVec) * inv_det;

	if ( *u < 0.0 || *u > 1.0 )
		return false;

	Vector qVec = tVec.crossProduct(edge1);

	*v = dir.dotProduct(qVec) * inv_det;

  if ( *v < 0.0 || *u + *v > 1.0 )
		return false;

  *t = edge2.dotProduct(qVec)*inv_det;

	return true;
}

bool RayTracer::intersectsSphere(Ray ray, float* distance, int objectIndex)
{
  //solve intersection by quadratic equation (find coeffs)
  Vector centerToOrigin = ray.getOrigin()-Object_Array[objectIndex].getCenter();

	//float a = ray.getDirection().dotProduct(ray.getDirection());
	//float a = 1.0;
	float b = ((ray.getDirection()).dotProduct(centerToOrigin))*2.0;
	float c = (centerToOrigin.dotProduct(centerToOrigin)) - (Object_Array[objectIndex].getRadius()*Object_Array[objectIndex].getRadius());

	float discriminant = (b*b)-(4.0*c);

	//discriminant tells how many intersections
	//< 0 => no intersections
	//0 => 1 intersection
	//>0 => 2 intersections (entry and exit of sphere)

	float t0, t1;

	if ( discriminant < 0.0 )
	  return false;
	else 
	{
    t0 = (-b + sqrt(discriminant)) / (2.0);
		t1 = (-b - sqrt(discriminant)) / (2.0);
	}

	//need to modify for refraction
	/*if ( t0 > 0.0 )
	{
		if ( t0 <= t1 )
		  *distance = t0;
	}
	else if ( t1 > 0.0 )
	{
		if ( t1 < t0 )
		  *distance = t1;
	}*/

	if ( t1 > 0.0 )
		*distance = t1;
	else
		*distance = t0;

	//*distance = t1;

	return true;
}

Triangle RayTracer::findIntersection(Ray ray, bool* missedScene, float* distance, int* objectIndex)
{
  //find first primitive hit by ray
	//for now go through each of the objects and check its triangles for intersection.  Keep closest triangle.
	const float EPSILON = 0.0000001;
  float shortestDistance = 5000.0;
	Triangle TriangleIntersected;
	float t,u,v;
	//printf("%d\n", Object_Array[0].pNumTriangles);

	//initialize the objects to having not been checked yet in octree run
	/*for (int i=0; i < pNumObjects; i++)
	{
		Object_Array[i].beenChecked = false;
	}

	clock_t start3, finish3;

	start3 = clock();
	vector<Object*> Object_Traversal = octree.getObjects(&worldBox, ray);
	finish3 = clock();

  traverse_time3 += (finish3-start3) / (double)CLOCKS_PER_SEC;

	for (int i=0; i < Object_Traversal.size(); i++)
	{
		if (Object_Traversal[i]->getRadius() > 0.0)  //if object is a sphere
		{
      if ( intersectsSphere(ray,&t,Object_Traversal[i]->Object_Index) )
			{
				if ( (t > 0.0) && (t < shortestDistance) )
				{
					shortestDistance = t;
					*objectIndex = Object_Traversal[i]->Object_Index;
				}
			}
			continue;
		}
		else
		{
		  for (int j=0; j < Object_Traversal[i]->pNumTriangles; j++)
	  	{
        if ( intersectsTriangle(ray.getOrigin(),Object_Traversal[i]->Triangle_Array[j],ray.getDirection(),&t,&u,&v) )
		    {
	  	    if ( (t > 0.0) && (t < shortestDistance) )
  		    {
			      TriangleIntersected = Object_Traversal[i]->Triangle_Array[j]; //shouldn't this be j?, yes
			      shortestDistance = t;
			  		*objectIndex = Object_Traversal[i]->Object_Index;
		      }
	  	  }
  		}
		}
	}*/

	for (int i=0; i < pNumObjects; i++)
	{
		if (Object_Array[i].getRadius() > 0.0)  //if object is a sphere
		{
      if ( intersectsSphere(ray,&t,i) )
			{
				if ( (t > 0.0) && (t < shortestDistance) )
				{
					shortestDistance = t;
					*objectIndex = i;
				}
			}
			continue;
		}
		else
		{
		  for (int j=0; j < Object_Array[i].pNumTriangles; j++)
		  {
        if ( intersectsTriangle(ray.getOrigin(),Object_Array[i].Triangle_Array[j],ray.getDirection(),&t,&u,&v) )
  		  {
		      if ( (t > 0.0) && (t < shortestDistance) )
		      {
	  		    TriangleIntersected = Object_Array[i].Triangle_Array[j];
  			    shortestDistance = t;
					  *objectIndex = i;
		      }
		    }
		  }
		}
	}
	if ( abs(shortestDistance - 5000.0) < EPSILON )
	{
		//ray went through scene and hit background
		*missedScene = true;
		Triangle test;
		test.set(1,1,1,1,1,1,1,1,1);
		return test;
	}
	*missedScene = false;
	*distance = shortestDistance;
	return TriangleIntersected;
}

bool RayTracer::shadowIntersection(Ray ray, float rayDist)
{
  //find first primitive hit by ray
	float t,u,v;

	//initialize the objects to having not been checked yet in octree run
	/*for (int i=0; i < pNumObjects; i++)
	{
		Object_Array[i].beenChecked = false;
	}

	vector<Object*> Object_Traversal = octree.getObjects(&worldBox, ray);

  for (int i=0; i < Object_Traversal.size(); i++)
	{
		//if object is refractive, light can get through
		if (Object_Traversal[i]->getRefractivity() > 0.0)
			continue;

		if (Object_Traversal[i]->getRadius() > 0.0)  //if object is a sphere
		{
      if ( intersectsSphere(ray,&t,Object_Traversal[i]->Object_Index) )
			{
        if ( (t > 0.0) && (t < rayDist) )
			    return true;
			}
			continue;
		}
		else
		{
		  for (int j=0; j < Object_Traversal[i]->pNumTriangles; j++)
		  {
        if ( intersectsTriangle(ray.getOrigin(),Object_Traversal[i]->Triangle_Array[j],ray.getDirection(),&t,&u,&v) )
	  	  {
  		    if ( (t > 0.0) && (t < rayDist) )
		      {
            return true;
		      }
	  	  }
  		}
		}
	}*/

	for (int i=0; i < pNumObjects; i++)
	{
		//if object is refractive, light can get through
		if (Object_Array[i].getRefractivity() > 0.0)
			continue;

		if (Object_Array[i].getRadius() > 0.0)  //if object is a sphere
		{
      if ( intersectsSphere(ray,&t,i) )
			{
        if ( (t > 0.0) && (t < rayDist) )
			    return true;
			}
			continue;
		}
		for (int j=0; j < Object_Array[i].pNumTriangles; j++)
		{
      if ( intersectsTriangle(ray.getOrigin(),Object_Array[i].Triangle_Array[j],ray.getDirection(),&t,&u,&v) )
		  {
		    if ( (t > 0.0) && (t < rayDist) )
		    {
          return true;
		    }
		  }
		}
	}

	return false;
}

void RayTracer::fillPixel(int column, int row, Color color)
{
  mybuffer[row*512*3 + column*3  ] = (int)(color.pRed*255.0);
  mybuffer[row*512*3 + column*3+1] = (int)(color.pGreen*255.0);
  mybuffer[row*512*3 + column*3+2] = (int)(color.pBlue*255.0);

	/*mybuffer[row*pResolutionX*3 + column*3  ] = (int)(color.pRed*255.0);
  mybuffer[row*pResolutionX*3 + column*3+1] = (int)(color.pGreen*255.0);
  mybuffer[row*pResolutionX*3 + column*3+2] = (int)(color.pBlue*255.0);*/
}

Color RayTracer::calcLocalLighting(Ray* ray, Triangle triangle, float distance, int objectIndex)
{
  //find intersection point
	//Vector step_back = ray.getDirection()*(-1.0*0.001);
	//point intersection_point = ray.getOrigin() + ( ray.getDirection()*distance + step_back ); //scaled back to try and avoid self shadows (bit of a hack for now)
	point intersection_point = ray->getOrigin() + ray->getDirection()*(distance*0.999);
	//point intersection_point = ray->getOrigin() + ray->getDirection()*distance;
	Color calculatedColor(0,0,0);

	/*Vector normal;
	if ( Object_Array[objectIndex].getRadius() > 0.0 ) {
	  normal = (intersection_point-Object_Array[objectIndex].getCenter()).normalize();
	}
	else {
		normal = triangle.normal;
	}
	if ( ray->getDirection().dotProduct(normal) > 0.0 )
	  normal = normal*-1.0;*/

	//floating back
	//intersection_point = intersection_point + normal*0.001;

	for (int i=0; i < pNumLights; i++)
	{
		//the shadow ray
		Vector lightRay = (Light_Array[i].getPosition() - intersection_point).normalize();
		float light_distance = (Light_Array[i].getPosition() - intersection_point).length();

    //If some object blocks the point from the current light, the light does not contribute
		if ( ShadowsOn ) {
		  if ( shadowIntersection( Ray(intersection_point,lightRay), light_distance) ) { continue; }
		}

		Vector normal;
		if ( Object_Array[objectIndex].getRadius() > 0.0 ) {
		  normal = (intersection_point-Object_Array[objectIndex].getCenter()).normalize();
		}
		else {
			normal = triangle.normal;
		}

		if (Object_Array[objectIndex].getDiffuse() > 0.0 && Object_Array[objectIndex].getReflectivity() != 1.0
			  && Object_Array[objectIndex].getRefractivity() != 1.0)
		{
			float dotProduct;

			if ( ray->getDirection().dotProduct(normal) > 0.0 )
				normal = normal*-1.0;

			if ( !Light_Array[i].isSpotLight() )
			  dotProduct = normal.dotProduct(lightRay) / (light_distance*light_distance);
			else
        dotProduct = -normal.dotProduct(Light_Array[i].getDirection()) / (light_distance*light_distance);

			if (dotProduct > 0.0)
			{
				calculatedColor = calculatedColor + (Object_Array[objectIndex].getColor()*Light_Array[i].getColor()*
					dotProduct*Object_Array[objectIndex].getDiffuse()*Light_Array[i].getIntensity());
			}
		}
		//now calculate the specular contribution
		if (Object_Array[objectIndex].getSpecular() > 0.0)
		{
			Vector reflectedVector = (lightRay - normal*(lightRay.dotProduct(normal))*2.0).normalize();
			float dotProduct = (ray->getDirection().dotProduct(reflectedVector));
			if ( dotProduct > 0.0 )
			{
				ray->specularComponent = (Light_Array[i].getColor()*powf(dotProduct,20.0)*Object_Array[objectIndex].getSpecular());
				//calculatedColor = calculatedColor + (Light_Array[i].getColor()*powf(dotProduct,20.0)*Object_Array[objectIndex].getSpecular());
			}
		}
	}

	//add in some ambient light
	calculatedColor = calculatedColor + Object_Array[objectIndex].getColor()*ambient_coeff;

	//normalize if the sum is too intense
	if (calculatedColor.pRed > 1.0) calculatedColor.pRed = 1.0;
	if (calculatedColor.pGreen > 1.0) calculatedColor.pGreen = 1.0;
	if (calculatedColor.pBlue > 1.0) calculatedColor.pBlue = 1.0;

  return calculatedColor;
}

Triangle* RayTracer::getMesh(char* filename, int* num_triangles)
{
  FILE* file_ptr = fopen(filename, "r");

	Triangle* Triangles_Out = NULL;
	vector<Triangle> Triangle_Data;
	float linePoints[12];
	int pointsReturned = 0;

	if ( file_ptr == NULL )
	{
		printf("couldn't load mesh\n");
		*num_triangles = 0;
		return NULL;
	}

	Triangle triangle;

  while ( (pointsReturned = fscanf(file_ptr, "%f %f %f %f %f %f %f %f %f %f %f %f\n", &linePoints[0],
		&linePoints[1], &linePoints[2], &linePoints[3], &linePoints[4], &linePoints[5], &linePoints[6],
		&linePoints[7], &linePoints[8], &linePoints[9], &linePoints[10], &linePoints[11]) ) != EOF )
	{
		if ( pointsReturned == 9 ) //triangluar face
		{
			triangle.set(linePoints[0], linePoints[1], linePoints[2], linePoints[3], linePoints[4], 
				linePoints[5], linePoints[6], linePoints[7], linePoints[8]);

		  Triangle_Data.push_back(triangle);
			(*num_triangles)++;
		}
		else if ( pointsReturned == 12 ) //quad face
		{
			triangle.set(linePoints[0], linePoints[1], linePoints[2], linePoints[3], 
				linePoints[4], linePoints[5], linePoints[6], linePoints[7], linePoints[8]);

      Triangle_Data.push_back(triangle);

      triangle.set(linePoints[0], linePoints[1], linePoints[2], linePoints[9], linePoints[10], 
				linePoints[11], linePoints[6],linePoints[7], linePoints[8]);

      Triangle_Data.push_back(triangle);

			(*num_triangles) += 2;
		}
	}

	//now convert vector to Triangle*
  Triangles_Out = new Triangle[*num_triangles];
	for (int i=0; i < *num_triangles; i++)
	{
    Triangles_Out[i] = Triangle_Data[i];
	}

	fclose(file_ptr);

	printf("finished reading mesh\n");

	return Triangles_Out;
}

bool RayTracer::loadScene(char* filename)
{
	FILE* file_ptr = fopen(filename, "r");

	if ( file_ptr == NULL )
		return false;

	fscanf(file_ptr, "objects:%d\n", &pNumObjects);
	fscanf(file_ptr, "lights:%d\n", &pNumLights);

	setNumLights(pNumLights);
	setNumObjects(pNumObjects);

	char object[255];

	//storage for the read in values
	point dimensions;
	point location;
	point rotation;

	float reflect;
	float diffuse;
	float specular;
  float refract;
	float intensity;
	Color color;
	float radius;
	float height;
  float size;
	float blur;
	char meshFile[255];

	int object_count = 0;
	int light_count = 0;

	//read first object

	int output = -1;

  while ( (output = fscanf(file_ptr, "%s ", &object)) != EOF ) 
	{
		printf("%s\n", object);
	  if ( !strcmp(object, "box") )
	  {
		  fscanf(file_ptr, "dimensions(%f,%f,%f),location(%f,%f,%f),rotation(%f,%f,%f)\n",
		  	&dimensions.px, &dimensions.py, &dimensions.pz,
		  	&location.px, &location.py, &location.pz,
		  	&rotation.px, &rotation.py, &rotation.pz);

	  	fscanf(file_ptr, "texture reflect(%f),diffuse(%f),specular(%f),color(%f,%f,%f),refract(%f),blur(%f)\n",
	  		&reflect, &diffuse, &specular, &color.pRed, &color.pGreen, &color.pBlue, &refract, &blur);

  		Box myBox(dimensions.px, dimensions.py, dimensions.pz,
  			location.px, location.py, location.pz,
  			rotation.px, rotation.py, rotation.pz);

			//printf("Box: %.2f %.2f %.2f\n", dimensions.px, dimensions.py, dimensions.pz);

  		Object_Array[object_count].setProperties(reflect,diffuse,specular,color,refract,12,myBox.pTriangles,blur);
			Object_Array[object_count].Object_Index = object_count;
			object_count++;
  	}
		else if ( !strcmp(object, "cone") )
	  {
		  fscanf(file_ptr, "height(%f),radius(%f),location(%f,%f,%f),rotation(%f,%f,%f)\n",
		  	&height, &radius,
		  	&location.px, &location.py, &location.pz,
		  	&rotation.px, &rotation.py, &rotation.pz);

			int tess_factor = 30;

	  	fscanf(file_ptr, "texture reflect(%f),diffuse(%f),specular(%f),color(%f,%f,%f),refract(%f),blur(%f)\n",
	  		&reflect, &diffuse, &specular, &color.pRed, &color.pGreen, &color.pBlue, &refract, &blur);

			Cone myCone(radius,height,tess_factor,location.px,location.py,location.pz,rotation.px,rotation.py,rotation.pz);

			//printf("Box: %.2f %.2f %.2f\n", dimensions.px, dimensions.py, dimensions.pz);

  		Object_Array[object_count].setProperties(reflect,diffuse,specular,color,refract,(tess_factor-1)*2+1,myCone.pTriangles,blur);
			Object_Array[object_count].Object_Index = object_count;
			object_count++;
  	}
  	else if ( !strcmp(object, "sphere") )
  	{
  		fscanf(file_ptr, "location(%f,%f,%f),radius(%f)\n",
			  &location.px, &location.py, &location.pz,
			  &radius);

		  fscanf(file_ptr, "texture reflect(%f),diffuse(%f),specular(%f),color(%f,%f,%f),refract(%f),blur(%f)\n",
			//fscanf(file_ptr, "texture reflect(%f),diffuse(%f),specular(%f),color(%f,%f,%f),refract(%f)\n",
	  		&reflect, &diffuse, &specular, &color.pRed, &color.pGreen, &color.pBlue, &refract, &blur);
				//&reflect, &diffuse, &specular, &color.pRed, &color.pGreen, &color.pBlue, &refract);

			//blur = 0.0;
			Object_Array[object_count].setProperties(reflect,diffuse,specular,color,refract,1,new Triangle(),blur,radius,location);
			Object_Array[object_count].Object_Index = object_count;
			object_count++;
  	}
	  else if ( !strcmp(object, "light") )
	  {
      fscanf(file_ptr, "location(%f,%f,%f),color(%f,%f,%f),intensity(%f)\n",
        &location.px, &location.py, &location.pz,
			  &color.pRed, &color.pGreen, &color.pBlue,
		  	&intensity);

			Light_Array[light_count].set(location,color,intensity);
			light_count++;
	  }
		else if ( !strcmp(object, "arealight") )
	  {
      fscanf(file_ptr, "size(%f),location(%f,%f,%f),color(%f,%f,%f),intensity(%f)\n",
				&size,
        &location.px, &location.py, &location.pz,
			  &color.pRed, &color.pGreen, &color.pBlue,
		  	&intensity);

			int accuracy_factor = 8;

			pNumLights += accuracy_factor*accuracy_factor-1;

			AreaLight theAreaLight(location,size,accuracy_factor,intensity);

			//resize Light_Array to add another accuracy_factor^2-1 spots
			Light* newLight_Array = new Light[pNumLights+(accuracy_factor*accuracy_factor-1)];

			for (int i=0; i < light_count; i++)
			{
        newLight_Array[i] = Light_Array[i];
			}

			delete Light_Array;
			
			Light_Array = newLight_Array;

			for (int i=0; i < accuracy_factor*accuracy_factor; i++)
			{
				Light_Array[light_count+i] = theAreaLight.Light_Array[i];
			}

			//Light_Array[light_count].set(location,color,intensity);
			light_count += accuracy_factor * accuracy_factor;
	  }
		else if ( !strcmp(object, "camera") )
	  {
      fscanf(file_ptr, "location(%f,%f,%f),rotation(%f,%f,%f)\n",
        &location.px, &location.py, &location.pz, &rotation.px, &rotation.py, &rotation.pz);

			float aspectRatio = (float)pResolutionY / (float)pResolutionX;

			cameraPosition.set(location.px,location.py,location.pz);
			bottomLeftCorner.set(-5.0,-5.0,0.0);
			//topRightCorner.set(5.0,5.0,0.0);
			topRightCorner.set(5.0,-5.0+10.0*aspectRatio,0.0);
			
			//translate the screen so the camera is still in the center
			matrix trans(matrix::TRANSLATE,location.px,location.py,0.0);
			matrix rotMatrix(matrix::ROTATE,rotation.px, rotation.py, rotation.pz);

			this->rotationMatrix = rotMatrix;

			bottomLeftCorner = trans * bottomLeftCorner;
			topRightCorner = trans * topRightCorner;
	  }
		else if ( !strcmp(object, "file") )
	  {
			for (int i=0;;i++)
			{
        meshFile[i] = fgetc(file_ptr);
				if ( meshFile[i] == '\n' )
				{
					meshFile[i] = '\0';
					break;
				}
			}
			printf("file is: %s\n", meshFile);
			int num_triangles = 0;
			Triangle* meshData = NULL;

			fscanf(file_ptr, "texture reflect(%f),diffuse(%f),specular(%f),color(%f,%f,%f),refract(%f),blur(%f)\n",
	  		&reflect, &diffuse, &specular, &color.pRed, &color.pGreen, &color.pBlue, &refract, &blur);

      meshData = getMesh(meshFile, &num_triangles);

			if ( meshData == NULL )
				continue;

			Object_Array[object_count].setProperties(reflect,diffuse,specular,color,refract,num_triangles,meshData,blur);
			Object_Array[object_count].Object_Index = object_count;
			object_count++;
		}
	}

	fclose(file_ptr);

	return true;
}

bool RayTracer::loadAnimation(char* filename)
{
	//we'll be doing 25 frames/second

  FILE* file_ptr = fopen(filename, "r");

	point location;
	float currTime;

	//kill all the keys from a previous run
	Keys.resize(0);

	if ( file_ptr == NULL )
	{
		pNumFrames = 1;
		Keys.push_back(keyFrame(0.0,cameraPosition,cameraPosition,0.0));
		return false;
	}

	point lastCameraPosition = cameraPosition;

	if ( fscanf(file_ptr, "t=%f location(%f,%f,%f)\n", &currTime, &location.px, &location.py, &location.pz) != EOF )
    Keys.push_back(keyFrame(currTime,location,lastCameraPosition,0.0));

  while ( fscanf(file_ptr, "t=%f location(%f,%f,%f)\n", &currTime, &location.px, &location.py, &location.pz) != EOF )
	{
		keyFrame prevFrame = Keys[Keys.size()-1];
		point prevCamera = prevFrame.location;
		float lastTime = prevFrame.time;
		Vector disp = location - prevCamera;
    Keys.push_back(keyFrame(currTime,location,prevCamera,lastTime));
	}

	//printf("how many? %d\n", Keys.size());

	pNumFrames = (int)( Keys[Keys.size()-1].time * 25.0 );

	fclose(file_ptr);

	return true;
}

void RayTracer::outputTGA(char* filename)
{
  FILE* file_ptr = fopen(filename, "wb");

	if ( file_ptr == NULL )
	{
		printf("couldn't open file for writing\n");
		return;
	}

	putc(0,file_ptr);
  putc(0,file_ptr);
  putc(2,file_ptr);                         /* uncompressed RGB */
	putc(0,file_ptr); putc(0,file_ptr);
	putc(0,file_ptr); putc(0,file_ptr);
	putc(0,file_ptr);
	putc(0,file_ptr); putc(0,file_ptr);           /* X origin */
	putc(0,file_ptr); putc(0,file_ptr);           /* y origin */
	//putc((width & 0x00FF),file_ptr);
	//putc((width & 0xFF00) / 256,file_ptr);
	//putc((height & 0x00FF),file_ptr);
	//putc((height & 0xFF00) / 256,file_ptr);
	//putc((width & 0x00FF),file_ptr);
	//putc((width & 0xFF00) / 256,file_ptr);
	//putc((height & 0x00FF),file_ptr);
	//putc((height & 0xFF00) / 256,file_ptr);
	putc((pResolutionX & 0x00FF),file_ptr);
	putc((pResolutionX & 0xFF00) / 256,file_ptr);
	putc((pResolutionY & 0x00FF),file_ptr);
	putc((pResolutionY & 0xFF00) / 256,file_ptr);
	putc(32,file_ptr);                        /* 24 bit bitmap */
	putc(0,file_ptr);
	for (int j=0; j < pResolutionY; j++)
	{
	  for (int i=0; i < pResolutionX; i++) 
	  {	
			putc(mybuffer[j*512*3 + i*3+2],file_ptr);//b
		  putc(mybuffer[j*512*3 + i*3+1],file_ptr);//g
			putc(mybuffer[j*512*3 + i*3  ],file_ptr);//r
			putc(255,file_ptr);//a
	  }
	}
	/*for (int i=0;i<512*512;i++) {
      putc(255,file_ptr);
      putc(0,file_ptr);
      putc(0,file_ptr);
      putc(255,file_ptr);
   }*/
	//printf("file out\n");
	fclose(file_ptr);
}