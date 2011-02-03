#include "includes.h"

extern GLubyte* mybuffer;

void display()
{
	glClearColor(0.0, 0.0, 0.0, 0.0); //#3399FF light blue
	glClear(GL_COLOR_BUFFER_BIT);

	glDrawPixels(width, height, GL_RGB, GL_UNSIGNED_BYTE, mybuffer);
	/*glColor3f(1.0, 1.0, 1.0); 
	glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
	gluPerspective(60.0, width / height, 1.0, 400.0);
	glMatrixMode(GL_MODELVIEW);
  glShadeModel(GL_SMOOTH);
  glLoadIdentity();*/

	glutSwapBuffers();
  glutPostRedisplay();
}

int main(int argc, char** argv)
{
  int WindowID;

  //mybuffer = (GLubyte*)malloc(512*512*3*sizeof(GLubyte));
	mybuffer = (GLubyte*)malloc(width*height*3*sizeof(GLubyte));

	for (int i=0; i < width*height*3; i++)
	{
		mybuffer[i] = 0;
		mybuffer[i+1] = 0;
		mybuffer[i+2] = 0;
	}

	glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
  glutInitWindowSize(width, height);
  WindowID = glutCreateWindow("Ray Tracer");

  MakeGUI();

  glutDisplayFunc(display);
  glutMainLoop();

	free(mybuffer);
	return 0;
}