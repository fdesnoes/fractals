-------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Julia ensemble in Ada
--|   Package de l'algorithme de Buddhabrot
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /Draw_buddhabrot.ads$
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

--with delay_aux_pkg;
with mandelbrot_types;

 procedure draw_buddhabrot (Iteration_Max: Integer;
 			x1, y1: float;
 			image_x,image_y:float;
 			image_x_left,image_x_right,image_y_left, image_y_right: float;
 			pixels: in out mandelbrot_types.pixs;
 			zoom:float);
