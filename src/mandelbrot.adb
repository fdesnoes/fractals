--------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Mandelbrot ensemble in Ada
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /Mandelbrot.adb$
--| Author           : Frederic Desnoes
--| Created On       : 2020/05/06
--| Last Modified By : $Author: Frederic Desnoes$
--| Last Modified On : $Date: 2022/07/13$
--| Status           : $State: Expe $
--|
--------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Ada.Numerics.Elementary_Functions;
with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;
with Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Sequential_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use interfaces;
with Delay_Aux_Pkg;
with draw_mandelbrot;
with draw_julia;
with draw_buddhabrot;
with mandelbrot_types; use mandelbrot_types;

with Gnoga.Types;
with Gnoga.Types.Colors; use Gnoga.Types.Colors;
with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D; use Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Gui.Element.form;

----------------------------------------------------------------------------

Package body fractals is

	use all type Gnoga.String;
	subtype String is Gnoga.String;
	----------------------------------------------------------------------------
	procedure Mandelbrot is
	  Main_Window : Gnoga.Gui.Window.Window_Type;
	  My_Window : Gnoga.Gui.Window.Window_Type;
	  My_View : Gnoga.Gui.View.View_Type;
	  Data_input: Gnoga.Gui.View.View_Type;
	  Mon_Canvas : Gnoga.Gui.Element.Canvas.Canvas_Type;
	  Context : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
	  Image_Import : Gnoga.Gui.Element.Common.IMG_Type;
	  My_Button_Julia : Gnoga.Gui.Element.Common.Button_Type;
	  My_Button_Mandelbrot : Gnoga.Gui.Element.Common.Button_Type;
	  My_Button_Buddhabrot : Gnoga.Gui.Element.Common.Button_Type;
	  My_Button_Image : Gnoga.Gui.Element.Common.Button_Type;
	  My_Exit   : Gnoga.Gui.Element.Common.Button_Type;

	  Image_ini	: Image_Data_Type;
	  Image_sauvegarde  : Gnoga.Gui.Element.Common.IMG_Type; --Gnoga.Gui.Element.Multimedia.Video_Type; --
	  F : Ada.Streams.Stream_IO.File_Type;
	  name : constant String := "Image_file";

	  Iterations_Form : Gnoga.Gui.Element.Form.Form_Type;
	  Input_Text_Iterations : Gnoga.Gui.Element.Form.Text_Type;
	  Question_Label_Iterations : Gnoga.Gui.Element.Form.Label_Type;

	  Julia_Form_x1  : Gnoga.Gui.Element.Form.Form_Type;
	  Julia_Input_text_x1: Gnoga.Gui.Element.Form.Text_Type;
	  Julia_Question_Label_x1: Gnoga.Gui.Element.Form.Label_Type;
	  Julia_Form_y1  : Gnoga.Gui.Element.Form.Form_Type;
	  Julia_Input_text_y1: Gnoga.Gui.Element.Form.Text_Type;
	  Julia_Question_Label_y1: Gnoga.Gui.Element.Form.Label_Type;


	  procedure draw_buddhabrot_tasking(Iteration_Max : Integer) is
	    value_color: Gnoga.Types.Color_Type;
	    tmp_color: Gnoga.Types.RGBA_Type;
	    
	    task T1 is
	    	entry compute1;
	    end T1;
	    task T2 is
	    	entry compute2;
	    end T2;
	    task T3 is
	    	entry compute3;
	    end T3;
	    task T4 is
	    	entry compute4;
	    end T4; 
	    --task T4;
	    task T5 is
	    	entry draw1;
	    	entry draw2;
	    	entry draw3;
	    	entry draw4;
	    end T5;
	    
	    
	    task body T1 is
	    	
	      begin
	      	draw_buddhabrot (Iteration_Max,
	      			x1,y1,
	      			image_x,image_y,
	 			1.0,image_x/2.0,1.0, image_y/2.0,
	 			pixels,
	 			zoom);
		--Delay_Aux_Pkg.Show_Elapsed_Time;
		T5.draw1;
		--end compute1;
	       end;
	       
	       
	    task body T2 is
	    	
	      begin
	      draw_buddhabrot (Iteration_Max,
	      			x1,y1,
	      			image_x,image_y,
	 			image_x/2.0+1.0,image_x,1.0, image_y/2.0,
	 			pixels,
	 			zoom);
		--Delay_Aux_Pkg.Show_Elapsed_Time;
		T5.draw2;
		--end compute2;
	    end;
	    
	    task body T3 is
	    	
	      begin
	      draw_buddhabrot (Iteration_Max,
	      			x1,y1,
	      			image_x,image_y,
	 			image_x/2.0+1.0,image_x,image_y/2.0+1.0, image_y,
	 			pixels,
	 			zoom);
		--Delay_Aux_Pkg.Show_Elapsed_Time; 
		T5.draw3;
		--end compute2;
	    end;
	    
	   task body T4 is
	    	
	      begin
	      draw_buddhabrot (Iteration_Max,
	      			x1,y1,
	      			image_x,image_y,
	 			1.0,image_x/2.0,image_y/2.0+1.0, image_y,
	 			pixels,
	 			zoom);
		--Delay_Aux_Pkg.Show_Elapsed_Time;
		T5.draw4;
		--end compute2;
	       end;
	       
	    task body T5 is  
	    begin
	    accept draw1 do
	    accept draw2 do
	    accept draw3 do
	    accept draw4 do
	    delay 10.0;     
	    for i in 1 .. integer(image_x) loop
	      for j in 1 .. integer(image_y) loop
		if pixels (i,j).n /= 0
		  then
		    value_color :=Gnoga.types.color_type'value(integer'image(255-integer'Min(pixels(i,j).n, 255)));
		    tmp_color := (0,0,value_color,1.0);
		    Context.Stroke_Color(tmp_color);
		    Context.Stroke_Text(".",i ,j);
		    Context.Stroke;
		  end if;
	      end loop;
	    end loop;
	    Delay_Aux_Pkg.Show_Elapsed_Time;
	    end draw4;
	    end draw3;
	    end draw2;
	    end draw1;
	    end T5;
	    
	  begin
	  	null;
	  end draw_buddhabrot_tasking;

	  Procedure clear_screen is
	    begin
	      Context.Fill_Rectangle ((1, 1, integer(image_x), integer(image_y)));
	    end clear_screen;
	  
	  procedure Write_image_PPM_IO (ImageFractal: in out Context_2D_Type) is	
		begin
	   	 Create (F, Out_File, name & ".ppm");
	   		--  PPM Header:
	    	 String'Write (
	      		Stream (F),
	      		"P6 " &
	      		integer'image(integer(image_y)) &
	      		integer'image(integer(image_x)) & " 255" & ASCII.LF
			);
			-- PPM image:	 
	    	  for y in 1..integer(image_x) loop
	      		for x in 1..integer(image_y) loop
			Unsigned_8'Write (Stream (f), Unsigned_8(Pixel(ImageFractal,x, y).Red));
			Unsigned_8'Write (Stream (f), Unsigned_8(Pixel(ImageFractal,x, y).Green));
			Unsigned_8'Write (Stream (f), Unsigned_8(Pixel(ImageFractal,x, y).Blue));
	      		end loop;
	    	  end loop;
	   		Close (F);
		end Write_image_PPM_IO;

	  procedure On_Click_julia (Object : in out Gnoga.Gui.Base.Base_Type'Class);
	   --  The procedure matches the prototype for Action_Events
	   --  This procedure can therefore be assigned to any event handler
	   --  of that nature. It can even be assigned to multiple handlers at
	   --  the same time. See Gnoga.Base in the event section of the spec
	   --  for more details about available events.

	  procedure On_Click_julia (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
	      pragma Unreferenced (Object);
	   begin
	      My_View.New_Line;
	      My_View.Put_Line ("I've been clicked!");
	      clear_screen;
	      Context.Stroke_Color ("Blue");
	      draw_julia (Iteration_Max,
	 			image_x, image_y,
	 			zoom,
	 			x1, y1,
	 			Float'Value(Julia_Input_Text_x1.value), Float'Value(Julia_Input_Text_y1.value),
	 			Context);
	   end On_Click_julia;
	  
	  procedure On_Click_mandelbrot (Object : in out Gnoga.Gui.Base.Base_Type'Class);
	  procedure On_Click_mandelbrot (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
	      pragma Unreferenced (Object);
	   begin
	      clear_screen;
	      My_View.New_Line;
	      My_View.Put_Line ("I've been clicked!");
	      Context.Stroke_Color ("Blue");
	      draw_mandelbrot(Integer'Value(Input_Text_Iterations.value),
	 			image_x, image_y,
	 			zoom,x1, y1,
	 			Context);
	   end On_Click_mandelbrot;

	  procedure On_Click_buddhabrot (Object : in out Gnoga.Gui.Base.Base_Type'Class);
	  procedure On_Click_buddhabrot (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
	      pragma Unreferenced (Object);
	   begin
	      My_View.New_Line;
	      My_View.Put_Line ("I've been clicked!");
	      clear_screen;
	      draw_buddhabrot_tasking(Integer'Value(Input_Text_Iterations.value));
	   end On_Click_buddhabrot;

	  procedure On_Image (Object : in out Gnoga.Gui.Base.Base_Type'Class);
	   --  When this action is fired it will save the image as .png
	  procedure On_Image (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
	      pragma Unreferenced (Object);
	   begin
	      Get_Image_Data (Context,Image_Ini,0,0,integer(image_x),integer(image_y));
	      Write_image_PPM_IO(Context);
	      --Image_Export.Create(Mon_Canvas,"/image/Yourfractal.png");
	   end On_Image;

	  procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
	   --  When this action is fired it will end the application
	  procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
	      pragma Unreferenced (Object);
	   begin
	      My_View.New_Line;
	      My_View.Put_Line ("Closing application");
	   
	      My_Button_Julia.Disabled;
	      My_Button_Mandelbrot.Disabled;
	      My_Button_Buddhabrot.Disabled;
	      My_Exit.Disabled;

	      Gnoga.Application.Singleton.End_Application;
	   end On_Exit;
	 
	begin --  Mandelbrot
	  Gnoga.Application.Title ("Mandelbrot");
	  Gnoga.Application.HTML_On_Close
		    ("<b>Connection to Application has been terminated</b>");
	  -- Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
	  Gnoga.Application.Singleton.Initialize (Main_Window => My_Window);
	  My_View.Create (My_Window);
	  My_Button_Julia.Create (My_View, "Julia!");
	   --  Since My_Button is created as a child of My_View a View_Type,
	   --  View_Type is designed to automatically add newly created children
	   --  in to the browser screen.
	  My_Button_Julia.On_Click_Handler (On_Click_julia'Unrestricted_Access);
	   --  We assign the procedure On_Click to be called as a handler for
	   --  On_Click events for My_Button. It would be preferable that
	   --  On_Click be declared at library level so that Unrestricted_Access
	   --  is not needed.
	  My_Button_Mandelbrot.Create (My_View, "Mandelbrot!");
	  My_Button_Mandelbrot.On_Click_Handler(On_Click_mandelbrot'Unrestricted_Access);
	  My_Button_Buddhabrot.Create (My_View, "Buddhabrot!");
	  My_Button_Buddhabrot.On_Click_Handler(On_Click_buddhabrot'Unrestricted_Access);
	  My_Button_Image.Create (My_View, "Image");
	  My_Button_Image.On_Click_Handler (On_Image'Unrestricted_Access);
	  My_Exit.Create (My_View, "End App");
	  My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);
	  
	  -- Data_Input.Create(My_View);
	  Iterations_Form.Create(My_View);
	  Input_Text_Iterations.Create(Iterations_Form, 4, "25");
	  Question_Label_Iterations.Create(Iterations_Form, Input_Text_Iterations, "Enter the number of iterations");

	  Julia_Form_x1.Create(My_View);
	  Julia_Form_x1.New_Line;
	  Julia_Input_Text_x1.Create(Julia_Form_x1, 20, "0.285");
	  Julia_Question_Label_x1.Create(Julia_Form_x1, Julia_Input_Text_x1, "Enter the real part of c as a float between -2.1 and +0.6");
	  
	  Julia_Form_y1.Create(My_View);
	  Julia_Input_Text_y1.Create(Julia_Form_y1, 20, "0.01");
	  Julia_Question_Label_y1.Create(Julia_Form_y1, Julia_Input_Text_y1, "Enter the imaginary part of c as a float between -1.2 and +1.2");

	  My_View.Horizontal_Rule;
	  Mon_Canvas.Create (Parent => My_View, Width => 1200, Height => 800);
	  Context.Get_Drawing_Context_2D (Mon_Canvas);
	  
	  Context.Save;
	  Context.Font (Height => "1px");
	  Context.Fill_Color ("white");
	  Context.Rotate_Degrees(90.0);
	  context.translate(0,-integer(image_y));
	  Context.Fill_Rectangle ((1, 1, integer(image_x), integer(image_y)));
	  
	Gnoga.Application.Singleton.Message_Loop;

	exception
	  when E : others =>
	    Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " & Ada.Exceptions.Exception_Message (E));
	end Mandelbrot;
--exception
 -- when E : other =>
--  	Gnoga.log(E);

end fractals;

