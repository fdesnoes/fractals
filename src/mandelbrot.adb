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
--| Last Modified On : $Date: 2022/07/02$
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

  X1 : constant float := -2.1;
  X2 : constant float := 0.6;
  y1 : constant float := -1.2;
  y2 : constant float := 1.2;
  zoom : constant float := 200.0; -- pour une distance de 1 sur le plan, on a 300 pixel sur l'image
  iteration_max : integer := 25;

  image_x : constant float := (x2-x1)*zoom;
  image_y : constant float := (y2-y1)*zoom;

  c_r : float := 0.0;
  c_i : float := 0.0;
  z_r : float := 0.0;
  z_i : float := 0.0;
  tmp : float := 0.0;
  Iteration : integer := 0;
  Screen_X : float := 0.0;
  Screen_Y : float := 0.0;
  
  

 procedure draw_mandelbrot (Iteration_Max: Integer) is
    begin
    Screen_X:=0.0;    
     while Screen_X < image_x loop
     Screen_Y:=0.0;
        while Screen_Y < image_y loop
          c_r := Screen_X / zoom + x1;
          c_i := Screen_Y / zoom + y1;
          z_r := 0.0;
          z_i := 0.0;
          Iteration:= 0;
          while z_r* z_r + z_i * z_i < 4.0 and then Iteration < iteration_max loop
            tmp := z_r;
            z_r := z_r * z_r - z_i * z_i + c_r;
            z_i := 2.0 * z_i*tmp  + c_i;
            Iteration := Iteration + 1;
          end loop;
          if Iteration = Iteration_Max then
            Context.Stroke_Text(".",integer(Screen_X) ,integer(Screen_Y) );
            Context.Stroke;
          else
            null;
          end if;
          Screen_Y := Screen_Y +1.0;
          end loop;
        Screen_X := Screen_X +1.0;
        end loop;
  end draw_mandelbrot;
 
  procedure draw_julia (c_r:float; c_i: float; iteration_Max: Integer)
  	with Pre => (c_r > -2.1 and c_r< 0.6 and c_i> -1.2 and c_i <1.2) is
    begin
    Screen_X:=0.0;    
     while Screen_X < image_x loop
     Screen_Y:=0.0;
        while Screen_Y < image_y loop
          z_r := Screen_X / zoom + x1+0.5;
          z_i := Screen_Y / zoom + y1;
          Iteration:= 0;
          while z_r* z_r + z_i * z_i < 4.0 and then Iteration < iteration_max loop
            tmp := z_r;
            z_r := z_r * z_r - z_i * z_i + c_r;
            z_i := 2.0 * z_i*tmp  + c_i;
            Iteration := Iteration + 1;
          end loop;
          if Iteration = Iteration_Max then
            Context.Stroke_Text(".",integer(Screen_X) ,integer(Screen_Y) );
            Context.Stroke;
          else
            null;
          end if;
          Screen_Y := Screen_Y +1.0;
          end loop;
        Screen_X := Screen_X +1.0;
        end loop;
  end draw_julia;

  procedure draw_buddhabrot(Iteration_Max : Integer) is
    pixels: array (integer range 1 .. integer(image_x), integer range 1 .. integer(image_y)) of integer;
    tmp_pixel: array (integer range 1 .. integer(image_x), integer range 1 .. integer(image_y)) of integer;
    value_color: Gnoga.Types.Color_Type;
    tmp_color: Gnoga.Types.RGBA_Type;
    task T1 is
    	entry compute;
    end T1;
    task T2 is
    	entry compute2;
    end T2;
    --task T2;
    --task T3;
    --task T4;
    task T5 is
    	entry draw;
    end T5;
    
    
    task body T1 is
      begin
      accept compute do
      Screen_X:=0.0;  
      for i in 1 .. integer(image_x)/2 loop
        for j in 1 .. integer(image_y) loop
          pixels (i,j) := 0;
        end loop;
      end loop; 
       while Screen_X < image_x loop
       Screen_Y:=0.0;
          while Screen_Y < image_y loop
            c_r := Screen_X / zoom + x1;
            c_i := Screen_Y / zoom + y1;
            z_r := 0.0;
            z_i := 0.0;
            Iteration:= 0;
            for i in 1 .. integer(image_x) loop
              for j in 1 .. integer(image_y) loop
                tmp_pixel (i,j) := 0;
              end loop;
            end loop;
            while z_r* z_r + z_i * z_i < 4.0 and then Iteration < iteration_max loop
              tmp := z_r;
              z_r := z_r * z_r - z_i * z_i + c_r;
              z_i := 2.0 * z_i*tmp  + c_i;
              Iteration := Iteration + 1;
              if integer((z_r-x1)*zoom) < integer(image_x) and integer((z_r-x1)*zoom) > 0 and integer((z_i-y1)*zoom) < integer(image_y) and integer((z_i-y1)*zoom) > 0
                then
                  tmp_pixel(integer((z_r-x1)*zoom),integer((z_i-y1)*zoom)) := 1;
              end if;
            end loop;
            if Iteration /= Iteration_Max then
              for i in 1 .. integer(image_x) loop
                for j in 1 .. integer(image_y) loop
                  if tmp_pixel (i,j) = 1
                    then
                    pixels(i,j):=pixels(i,j)+1;
                  end if;
                end loop;
              end loop;
            else
              null;
            end if;
            Screen_Y := Screen_Y +1.0;
          end loop;
          Screen_X := Screen_X +1.0;
        end loop;
        Delay_Aux_Pkg.Show_Elapsed_Time;
       end compute;
       end;
       
       
    task body T2 is
      begin
      accept compute2 do
      Screen_X:=image_x/2.0+1.0;  
      for i in integer(image_x/2.0)+1 .. integer(image_x) loop
        for j in 1 .. integer(image_y) loop
          pixels (i,j) := 0;
        end loop;
      end loop; 
       while Screen_X < image_x loop
       Screen_Y:=0.0;
          while Screen_Y < image_y loop
            c_r := Screen_X / zoom + x1;
            c_i := Screen_Y / zoom + y1;
            z_r := 0.0;
            z_i := 0.0;
            Iteration:= 0;
            for i in 1 .. integer(image_x) loop
              for j in 1 .. integer(image_y) loop
                tmp_pixel (i,j) := 0;
              end loop;
            end loop;
            while z_r* z_r + z_i * z_i < 4.0 and then Iteration < iteration_max loop
              tmp := z_r;
              z_r := z_r * z_r - z_i * z_i + c_r;
              z_i := 2.0 * z_i*tmp  + c_i;
              Iteration := Iteration + 1;
              if integer((z_r-x1)*zoom) < integer(image_x) and integer((z_r-x1)*zoom) > 0 and integer((z_i-y1)*zoom) < integer(image_y) and integer((z_i-y1)*zoom) > 0
                then
                  tmp_pixel(integer((z_r-x1)*zoom),integer((z_i-y1)*zoom)) := 1;
              end if;
            end loop;
            if Iteration /= Iteration_Max then
              for i in integer(image_x/2.0)+1 .. integer(image_x) loop
                for j in 1 .. integer(image_y) loop
                  if tmp_pixel (i,j) = 1
                    then
                    pixels(i,j):=pixels(i,j)+1;
                  end if;
                end loop;
              end loop;
            else
              null;
            end if;
            Screen_Y := Screen_Y +1.0;
          end loop;
          Screen_X := Screen_X +1.0;
        end loop;
        Delay_Aux_Pkg.Show_Elapsed_Time;
        end compute2;
    end;
       
    task body T5 is  
    begin
    accept draw;
    delay 10.0;     
    for i in 1 .. integer(image_x) loop
      for j in 1 .. integer(image_y) loop
        if pixels (i,j) /= 0
          then
            -- tmp_color := Gnoga.Types.colors.To_Color_Enumeration(integer'Min(pixels(i,j), 255), integer'Min(pixels(i,j), 255), integer'Min(pixels(i,j), 255),1.0);
            -- tmp_color := (255, 0, 0, 1.0);
            -- value_color :=integer'image(integer'Min(pixels(i,j), 255));
            value_color :=Gnoga.types.color_type'value(integer'image(255-integer'Min(pixels(i,j), 255)));
            -- tmp_color := Gnoga.types.To_RGBA(0,0,integer'image(integer'Min(pixels(i,j),255)),1.0);
            tmp_color := (0,0,value_color,1.0);
            -- Context.Stroke_Color("Gnoga.Types.Colors.To_Color_Enumeration(tmp_color)");
            Context.Stroke_Color(tmp_color);
            Context.Stroke_Text(".",i ,j);
            Context.Stroke;
          end if;
      end loop;
    end loop;
    Delay_Aux_Pkg.Show_Elapsed_Time;
    end;
    
  begin
  	T1.compute;
  	T2.compute2;
  	T5.draw;
  end draw_buddhabrot;

  Procedure clear_screen is
    begin
      Context.Fill_Rectangle ((0, 0, integer(image_x), integer(image_y)));
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
      draw_julia (Float'Value(Julia_Input_Text_x1.value), Float'Value(Julia_Input_Text_y1.value), Integer'Value(Input_Text_Iterations.value));
   end On_Click_julia;
  
  procedure On_Click_mandelbrot (Object : in out Gnoga.Gui.Base.Base_Type'Class);
  procedure On_Click_mandelbrot (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      My_View.New_Line;
      My_View.Put_Line ("I've been clicked!");
      clear_screen;
      Context.Stroke_Color ("Blue");
      draw_mandelbrot(Integer'Value(Input_Text_Iterations.value));
   end On_Click_mandelbrot;

  procedure On_Click_buddhabrot (Object : in out Gnoga.Gui.Base.Base_Type'Class);
  procedure On_Click_buddhabrot (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      My_View.New_Line;
      My_View.Put_Line ("I've been clicked!");
      clear_screen;
      draw_buddhabrot(Integer'Value(Input_Text_Iterations.value));
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
  Julia_Question_Label_y1.Create(Julia_Form_y1, Julia_Input_Text_y1, "Enter the imaginary part of c as a float between -1,2 and +1,2");


  -- Julia_Label.Create (Julia_Form,Julia_Value_Label,"x1",false);
  -- Julia_Range.Create (Julia_Form);
  -- Julia_Range.Minimum (-2.1);
  -- Julia_Range.Maximum (0.6);
  -- Julia_Range.Value (0.285);
  -- Julia_Value_Label.Create (Julia_Form, Julia_Range, Julia_range.value, False); 
  -- Julia_Range_Label.Create (Julia_Form, Julia_Range, " ", False); 
  -- Julia_Range.On_Change_Handler (Julia_Range_Change'Unrestricted_Access);
  
  My_View.Horizontal_Rule;
  Mon_Canvas.Create (Parent => My_View, Width => 1200, Height => 800);
  Context.Get_Drawing_Context_2D (Mon_Canvas);
  
  Context.Save;
  Context.Font (Height => "1px");
  Context.Fill_Color ("white");
  Context.Rotate_Degrees(90.0);
  context.translate(0,-integer(image_y));
  Context.Fill_Rectangle ((0, 0, integer(image_x), integer(image_y)));
  --Draw_Image (Context,Image_sauvegarde,0,0);
  
Gnoga.Application.Singleton.Message_Loop;

exception
  when E : others =>
    Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " & Ada.Exceptions.Exception_Message (E));
end Mandelbrot;
