#include <math.h>
#include <stdio.h>
#include "mathobjects.h"

/******3D Vector class******/

Vector::Vector(float x, float y, float z) {
  px = x;
  py = y;
  pz = z;
  parray[0] = x;
  parray[1] = y;
  parray[2] = z;
}

Vector Vector::operator*(float scalar) {
  return Vector(scalar*px, scalar*py, scalar*pz);
}

Vector Vector::operator/(float scalar) {
  return Vector(px/scalar, py/scalar, pz/scalar);
}

Vector Vector::operator+(Vector& vec2) {
  return Vector(px + vec2.px, py + vec2.py, pz + vec2.pz);
}

Vector Vector::operator+(point& p2) {
  return Vector(px + p2.px, py + p2.py, pz + p2.pz);
}

Vector Vector::operator-(Vector& vec2) {
  return Vector(px - vec2.px, py - vec2.py, pz - vec2.pz);
}

float Vector::operator[](int index) {
  if ( (index < 0) || (index > 2) ) {
    printf("Index out of bounds\n");
    return 0.0;
  }
  else
    return parray[index];
}

Vector Vector::normalize() {
  float magnitude = sqrt(px*px + py*py + pz*pz);
  return Vector(px / magnitude, py / magnitude, pz / magnitude);
}

float Vector::length() {
  return sqrt(px*px + py*py + pz*pz);
}

float Vector::dotProduct(Vector& vec2) {
  return (px*vec2.px + py*vec2.py + pz*vec2.pz);
}

Vector Vector::crossProduct(Vector& vec2) {
  return Vector(py*vec2.pz - pz*vec2.py, pz*vec2.px - px*vec2.pz, px*vec2.py - py*vec2.px);
}

void Vector::set(float x, float y, float z)
{
	px = x;
	py = y;
	pz = z;
}

/********matrix 4x4 class************/

matrix::matrix(MATRIX_INIT type, float x, float y, float z) {
  switch (type) {
	case ROTATE:
	 {matrix Rx(1,0,0,0,0,cos((float)x),-sin((float)x),0,0,sin((float)x),cos((float)x),0,0,0,0,1);
	  matrix Ry(cos((float)y),0,sin((float)y),0,0,1,0,0,-sin((float)y),0,cos((float)y),0,0,0,0,1);
	  matrix Rz(cos((float)z),-sin((float)z),0,0,sin((float)z),cos((float)z),0,0,0,0,1,0,0,0,0,1);
	  matrix R = Rz*Rx*Ry;
	  for (int i=0; i < 16; i++)
	  {
		  values[i] = R.values[i];
	  }}
    break;
	case TRANSLATE:
	  values[0] = 1; values[1] = 0; values[2] = 0; values[3] = x;
	  values[4] = 0; values[5] = 1; values[6] = 0; values[7] = y;
	  values[8] = 0; values[9] = 0; values[10] = 1; values[11] = z;
	  values[12] = 0; values[13] = 0; values[14] = 0; values[15] = 1;
	break;
	case SCALE:
	  values[0] = x; values[1] = 0; values[2] = 0; values[3] = 0;
	  values[4] = 0; values[5] = y; values[6] = 0; values[7] = 0;
	  values[8] = 0; values[9] = 0; values[10] = z; values[11] = 0;
	  values[12] = 0; values[13] = 0; values[14] = 0; values[15] = 1;
	break;
  }
}

matrix::matrix(float distance) {
  values[0] = 1.0; values[1] = 0.0; values[2] = 0.0; values[3] = 0.0;
  values[4] = 0.0; values[5] = 1.0; values[6] = 0.0; values[7] = 0.0;
  values[8] = 0.0; values[9] = 0.0; values[10] = 1.0; values[11] = 0.0;
  values[12] = 0.0; values[13] = 0.0; values[14] = 1.0/distance; values[15] = 0.0;
}

matrix::matrix(float* elements) {
  int length = sizeof(elements) / sizeof(float);
  for (int i=0; i < length; i++)
  {
    values[i] = elements[i];
  }
}

float matrix::operator()(int row, int column) {
  return values[row*4 + column];
}

matrix::matrix(float v0, float v1, float v2, float v3,
	       float v4, float v5, float v6, float v7,
	       float v8, float v9, float v10, float v11,
	       float v12, float v13, float v14, float v15) {
  values[0] = v0; values[1] = v1; values[2] = v2; values[3] = v3;
  values[4] = v4; values[5] = v5; values[6] = v6; values[7] = v7;
  values[8] = v8; values[9] = v9; values[10] = v10; values[11] = v11;
  values[12] = v12; values[13] = v13; values[14] = v14; values[15] = v15;
}

matrix::matrix() {  //default constructor generates an identity matrix
  for (int i=0; i < 16; i++)
    values[i] = 0.0;

  values[0] = 1.0;
  values[5] = 1.0;
  values[10] = 1.0;
  values[15] = 1.0;
}

matrix matrix::transpose() {
  return matrix(values[0],values[4],values[8],values[12],
		values[1],values[5],values[9],values[13],
		values[2],values[6],values[10],values[14],
		values[3],values[7],values[11],values[15]);
}

Vector matrix::operator*(Vector& vec) {
  //the point has 1 as the last coordinate, Vector 0
  return Vector(values[0]*vec.px + values[1]*vec.py + values[2]*vec.pz,
		values[4]*vec.px + values[5]*vec.py + values[6]*vec.pz,
		values[8]*vec.px + values[9]*vec.py + values[10]*vec.pz);
}

point matrix::operator*(point& p2) {
  return point(values[0]*p2.px + values[1]*p2.py + values[2]*p2.pz + values[3],
	       values[4]*p2.px + values[5]*p2.py + values[6]*p2.pz + values[7],
	       values[8]*p2.px + values[9]*p2.py + values[10]*p2.pz + values[11]);
}

matrix matrix::operator*(matrix& mat) {
  return matrix(values[0]*mat.values[0]+values[1]*mat.values[4]+values[2]*mat.values[8]+values[3]*mat.values[12],
		values[0]*mat.values[1]+values[1]*mat.values[5]+values[2]*mat.values[9]+values[3]*mat.values[13],
		values[0]*mat.values[2]+values[1]*mat.values[6]+values[2]*mat.values[10]+values[3]*mat.values[14],
		values[0]*mat.values[3]+values[1]*mat.values[7]+values[2]*mat.values[11]+values[3]*mat.values[15],
		values[4]*mat.values[0]+values[5]*mat.values[4]+values[6]*mat.values[8]+values[7]*mat.values[12],
		values[4]*mat.values[1]+values[5]*mat.values[5]+values[6]*mat.values[9]+values[7]*mat.values[13],
		values[4]*mat.values[2]+values[5]*mat.values[6]+values[6]*mat.values[10]+values[7]*mat.values[14],
		values[4]*mat.values[3]+values[5]*mat.values[7]+values[6]*mat.values[11]+values[7]*mat.values[15],
		values[8]*mat.values[0]+values[9]*mat.values[4]+values[10]*mat.values[8]+values[11]*mat.values[12],
		values[8]*mat.values[1]+values[9]*mat.values[5]+values[10]*mat.values[9]+values[11]*mat.values[13],
		values[8]*mat.values[2]+values[9]*mat.values[6]+values[10]*mat.values[10]+values[11]*mat.values[14],
		values[8]*mat.values[3]+values[9]*mat.values[7]+values[10]*mat.values[11]+values[11]*mat.values[15],
		values[12]*mat.values[0]+values[13]*mat.values[4]+values[14]*mat.values[8]+values[15]*mat.values[12],
		values[12]*mat.values[1]+values[13]*mat.values[5]+values[14]*mat.values[9]+values[15]*mat.values[13],
		values[12]*mat.values[2]+values[13]*mat.values[6]+values[14]*mat.values[10]+values[15]*mat.values[14],
		values[12]*mat.values[3]+values[13]*mat.values[7]+values[14]*mat.values[11]+values[15]*mat.values[15]);
}

/**********point class***********/

point::point()
{
	px = 0.0;
	py = 0.0;
	pz = 0.0;
}

point::point(float x, float y, float z) {
  px = x;
  py = y;
  pz = z;
  parray[0] = x;
  parray[1] = y;
  parray[2] = z;
}

void point::set(float x, float y, float z)
{
	px = x;
	py = y;
	pz = z;
}

point point::operator*(float scalar) {
  return point(px*scalar, py*scalar, pz*scalar);
}

point point::operator+(Vector& vec2) {
  return point(px + vec2.px, py + vec2.py, pz + vec2.pz);
}

point point::operator-(Vector& vec2) {
  return point(px - vec2.px, py - vec2.py, pz - vec2.pz);
}

Vector point::operator-(point& p2) {
  return Vector(px - p2.px, py - p2.py, pz - p2.pz);
}

float  point::operator[](int index) {
  if ( (index < 0) || (index > 2) ) {
    printf("Index out of bounds\n");
    return 0.0;
  }
  else
    return parray[index];  
}
