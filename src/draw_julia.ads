--------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Julia ensemble in Ada
--|   Package de l'algorithme de Mandelbrot classique
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /Draw_mandelbrot.ads$
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

 procedure draw_julia (Iteration_Max: Integer;
 			image_x, image_y: float;
 			zoom:float;
 			x1, y1: float;
 			c_r, c_i: float;
 			Context : in out Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type)
 		with Pre => (c_r > -2.1 and c_r< 0.6 and c_i> -1.2 and c_i <1.2);
