--------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Mandelbrot ensemble in Ada
--|   Package de l'algorithme de Mandelbrot classique
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /Draw_mandelbrot.adb$
--| Author           : Frederic Desnoes
--| Created On       : 2022/07/02
--| Last Modified By : $Author: Frederic Desnoes$
--| Last Modified On : $Date: 2022/07/02$
--| Status           : $State: Expe $
--|
--------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D; use Gnoga.Gui.Element.Canvas.Context_2D;
 			
 procedure draw_mandelbrot (Iteration_Max: Integer;
 			image_x, image_y: float;
 			zoom:float;x1, y1: float;
 			Context : in out Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type) is
 			
    Screen_X, Screen_Y : float;
    c_r, c_i, z_r, z_i, tmp: float;
    iteration: integer;
    
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
