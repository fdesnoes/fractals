--------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Julia ensemble in Ada
--|   Package de l'algorithme de Julia classique
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /Draw_mandelbrot.adb$
--| Author           : Frederic Desnoes
--| Created On       : 2022/07/07
--| Last Modified By : $Author: Frederic Desnoes$
--| Last Modified On : $Date: 2022/07/07$
--| Status           : $State: Expe $
--|
--------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D; use Gnoga.Gui.Element.Canvas.Context_2D;
 			
 procedure draw_julia  (Iteration_Max: Integer;
 			image_x, image_y: float;
 			zoom:float;
 			x1, y1: float;
 			c_r,c_i: float;
 			Context : in out Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type) is
 		--with Pre => (c_r > -2.1 and c_r< 0.6 and c_i> -1.2 and c_i <1.2) is
 
 		Screen_X, Screen_Y : float;
    		z_r, z_i, tmp: float;
               iteration: integer;
    begin		
    Screen_X:=0.0;    
     while Screen_X < image_x loop
     Screen_Y:=0.0;
        while Screen_Y < image_y loop
          z_r := Screen_X / zoom + x1;
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
