#include "includes.h"

GLUI_EditText* File_Textbox;
GLUI_EditText* Anim_Textbox;
GLUI_EditText* Output_Textbox;
GLUI_Button* Render;
GLUI_Panel* Res_Options_Panel;
GLUI_Panel* Resolution_Panel;
GLUI_Panel* Options_Panel;
GLUI_Spinner* X_Resolution;
GLUI_Spinner* Y_Resolution;
GLUI_Checkbox* check_box_transparency;
GLUI_Checkbox* check_box_refraction;
GLUI_Checkbox* check_box_shadows;
GLUI_Checkbox* check_output_file;
GLUI_Spinner* reflections_spinner;
GLUI_Spinner* blurry_spinner;
GLUI_Spinner* blurdepth_spinner;
GLUI_Panel* Camera_Panel;
GLUI_Panel* Camera_Translation_Panel;
GLUI_Panel* Camera_Rotation_Panel;
GLUI_Spinner* Camera_Translation_X;
GLUI_Spinner* Camera_Translation_Y;
GLUI_Spinner* Camera_Translation_Z;
GLUI_Spinner* Camera_Rotation_X;
GLUI_Spinner* Camera_Rotation_Y;
GLUI_Spinner* Camera_Rotation_Z;
GLUI_Panel* Alias_Panel;
GLUI_RadioGroup* Alias_Radio;
GLUI_Panel* Text_Panel;

RayTracer tracer; //memory will accumulate with destructor being called

extern GLubyte* mybuffer;
bool animLoaded = false;

void callback_func(int caller)
{
	clock_t start, finish;
	double time;

  switch (caller)
	{
	  case 1:			
			  //load the file in the textbox into the scene
			  tracer.setResolution(X_Resolution->get_int_val(),Y_Resolution->get_int_val());
				tracer.blurrySamples = blurry_spinner->get_int_val();
				tracer.blurDepth = blurdepth_spinner->get_int_val();

				//resize the output window
				//glutReshapeWindow(X_Resolution->get_int_val(), Y_Resolution->get_int_val());

				/*if ( mybuffer != NULL )
					free(mybuffer);

				mybuffer = (GLubyte*)malloc(X_Resolution->get_int_val()*Y_Resolution->get_int_val()*3*sizeof(GLubyte));

				if ( mybuffer == NULL )
					return;

				for (int i=0; i < X_Resolution->get_int_val()*Y_Resolution->get_int_val()*3; i++)
				{
					mybuffer[i] = 0;
					mybuffer[i+1] = 0;
					mybuffer[i+2] = 0;
				}*/

			  //if ( tracer.loadScene("C:\\Documents and Settings\\spillow\\Desktop\\scene6.txt") ) {
				if ( tracer.loadScene(File_Textbox->get_text()) ) {
					//printf("%s\n", File_Textbox->get_text());
	        
					//load the animation
					//animLoaded = tracer.loadAnimation("C:\\Documents and Settings\\spillow\\Desktop\\scene.anim");
					tracer.loadAnimation(Anim_Textbox->get_text());
          //animLoaded = tracer.loadAnimation("asd;flkjl;kj");
	
					//time how long it takes
					start = clock();
					tracer.rayTrace();
					finish = clock();
					printf("time: %lf\n", (finish-start) / (double)CLOCKS_PER_SEC);
			}
			else
				printf("load error\n");
		break;
		case 6:
			if (check_box_shadows->get_int_val() == 1)
				tracer.ShadowsOn = true;
			else
				tracer.ShadowsOn = false;
		break;
		case 7:
			tracer.traceDepth = reflections_spinner->get_int_val();
		break;
		case 14:
			if ( Alias_Radio->get_int_val() == 0 )
				tracer.setSampling(1);
			else if ( Alias_Radio->get_int_val() == 1 )
				tracer.setSampling(4);
			else if ( Alias_Radio->get_int_val() == 2 )
				tracer.setSampling(16);
			else if ( Alias_Radio->get_int_val() == 3 )
				tracer.setSampling(25);
		break;
	}
}

void MakeGUI()
{
  GLUI *glui = GLUI_Master.create_glui( "Controls", 0 );

	GLUI_Panel* File_Panel = glui->add_panel("filepanel", false);

	Render = glui->add_button_to_panel(File_Panel,"RENDER",1,callback_func);
	Render->set_alignment(GLUI_ALIGN_CENTER);

	//top panel has file textbox and render button
	Text_Panel = glui->add_panel_to_panel(File_Panel,"textpanel",false);
	File_Textbox = glui->add_edittext_to_panel(Text_Panel,"SCENE:",1,0,0,callback_func);
	File_Textbox->set_alignment(GLUI_ALIGN_LEFT);
	File_Textbox->set_w(400);
	/*glui->add_column_to_panel(File_Panel, false);
	glui->add_column_to_panel(File_Panel, false);
	glui->add_column_to_panel(File_Panel, false);
	glui->add_column_to_panel(File_Panel, false);
	glui->add_column_to_panel(File_Panel, false);*/
	Render->set_alignment(GLUI_ALIGN_RIGHT);
	Anim_Textbox = glui->add_edittext_to_panel(Text_Panel,"ANIM:",1,0,4,callback_func);
	Anim_Textbox->set_alignment(GLUI_ALIGN_LEFT);
	Anim_Textbox->set_w(400);
	Output_Textbox = glui->add_edittext_to_panel(Text_Panel,"OUTPUT:",1,0,15,callback_func);
	Output_Textbox->set_alignment(GLUI_ALIGN_LEFT);
	Output_Textbox->set_w(400);

	//holds resolution and render options
	Res_Options_Panel = glui->add_panel("resoptionspanel", false);
	Resolution_Panel = glui->add_panel_to_panel(Res_Options_Panel,"Resolution",true);
	X_Resolution = glui->add_spinner_to_panel(Resolution_Panel,"X:",GLUI_SPINNER_INT,0,2,callback_func);
	Y_Resolution = glui->add_spinner_to_panel(Resolution_Panel,"Y:",GLUI_SPINNER_INT,0,3,callback_func);
	X_Resolution->set_int_limits(0,512);
	X_Resolution->set_int_val(512);
	Y_Resolution->set_int_limits(0,512);
	Y_Resolution->set_int_val(512);

  glui->add_column_to_panel(Res_Options_Panel, false);
	glui->add_column_to_panel(Res_Options_Panel, false);
	glui->add_column_to_panel(Res_Options_Panel, false);
	glui->add_column_to_panel(Res_Options_Panel, false);
	glui->add_column_to_panel(Res_Options_Panel, false);

  Options_Panel = glui->add_panel_to_panel(Res_Options_Panel,"Options",true);
  //check_box_transparency = glui->add_checkbox_to_panel(Options_Panel,"Transparency",0,4,callback_func);
	//check_box_refraction = glui->add_checkbox_to_panel(Options_Panel,"Refraction",0,5,callback_func);
	check_box_shadows = glui->add_checkbox_to_panel(Options_Panel,"Shadows",0,6,callback_func);
	reflections_spinner = glui->add_spinner_to_panel(Options_Panel,"Reflections",GLUI_SPINNER_INT,0,7,callback_func);
	blurry_spinner = glui->add_spinner_to_panel(Options_Panel,"Blurry Samples",GLUI_SPINNER_INT,0,26,callback_func);
	blurdepth_spinner = glui->add_spinner_to_panel(Options_Panel,"Blur Depth",GLUI_SPINNER_INT,0,27,callback_func);
	//check_box_transparency->set_int_val(1);
	//check_box_refraction->set_int_val(1);
	check_box_shadows->set_int_val(1);
	reflections_spinner->set_int_val(3);
	reflections_spinner->set_int_limits(1,10);
	check_output_file = glui->add_checkbox_to_panel(Options_Panel,"Output file",0,20,callback_func);
	check_output_file->set_int_val(1);
	blurry_spinner->set_int_val(64);
	blurry_spinner->set_int_limits(1,128);
	blurdepth_spinner->set_int_val(0);
	blurdepth_spinner->set_int_limits(0,5);

	//holds the camera translation and rotation spinners
  /*Camera_Panel = glui->add_panel("camerapanel", false);
  Camera_Translation_Panel = glui->add_panel_to_panel(Camera_Panel,"Camera Translation");
	Camera_Translation_X = glui->add_spinner_to_panel(Camera_Translation_Panel,"X:",GLUI_SPINNER_FLOAT,0,8,callback_func);
	Camera_Translation_Y = glui->add_spinner_to_panel(Camera_Translation_Panel,"Y:",GLUI_SPINNER_FLOAT,0,9,callback_func);
	Camera_Translation_Z = glui->add_spinner_to_panel(Camera_Translation_Panel,"Z:",GLUI_SPINNER_FLOAT,0,10,callback_func);

	glui->add_column_to_panel(Camera_Panel, false);

  Camera_Rotation_Panel = glui->add_panel_to_panel(Camera_Panel,"Camera Rotation");
	Camera_Rotation_X = glui->add_spinner_to_panel(Camera_Rotation_Panel,"X:",GLUI_SPINNER_FLOAT,0,11,callback_func);
	Camera_Rotation_Y = glui->add_spinner_to_panel(Camera_Rotation_Panel,"Y:",GLUI_SPINNER_FLOAT,0,12,callback_func);
	Camera_Rotation_Z = glui->add_spinner_to_panel(Camera_Rotation_Panel,"Z:",GLUI_SPINNER_FLOAT,0,13,callback_func);*/


  Alias_Panel = glui->add_panel("Anti-Aliasing");
	Alias_Panel->set_alignment(GLUI_ALIGN_LEFT);
  Alias_Radio = glui->add_radiogroup_to_panel(Alias_Panel,0,14,callback_func);
	glui->add_radiobutton_to_group(Alias_Radio, "1x");
	glui->add_radiobutton_to_group(Alias_Radio, "4x");
	glui->add_radiobutton_to_group(Alias_Radio, "16x");
	glui->add_radiobutton_to_group(Alias_Radio, "25x");

}