
--------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Buddhbrot ensemble in Ada
--|   Package de l'algorithme de Buddhabrot classique
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /draw_buddhabrot.adb$
--| Author           : Frederic Desnoes
--| Created On       : 2022/07/10
--| Last Modified By : $Author: Frederic Desnoes$
--| Last Modified On : $Date: 2022/07/10$
--| Status           : $State: Expe $
--|
--------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;

with delay_aux_pkg;
with mandelbrot_types;
 			
procedure draw_buddhabrot  (Iteration_Max: Integer;
			x1,y1: float;
			image_x,image_y:float;
 			image_x_left,image_x_right,image_y_left, image_y_right: float;
 			pixels: in out mandelbrot_types.pixs;
 			zoom:float) is

    	i,j, iteration: integer;
    	Screen_X, Screen_Y, c_r, c_i, z_r, z_i, tmp: float;
    	tmp_pixel: array (integer range 1 .. integer(image_x), integer range 1 .. integer(image_y)) of integer;
      begin
      Screen_X:=image_x_left;  
      for i in integer(image_x_left) .. integer(image_x_right) loop
        for j in integer(image_y_left) .. integer(image_y_right) loop
          pixels (i,j).n := 0;
        end loop;
      end loop; 
       while Screen_X <= image_x_right loop
       Screen_Y:=image_y_left;
          while Screen_Y <= image_y_right loop
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
              for i in 1 .. integer(image_x) loop --integer(image_x_left) .. integer(image_x_right) loop
        	for j in 1 .. integer(image_y) loop--integer(image_y_left) .. integer(image_y_right) loop
                  if tmp_pixel (i,j) = 1
                    then
                    pixels(i,j).n:=pixels(i,j).n+1;
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
    end;
