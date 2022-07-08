--------------------------------------------------------------------------------
--|   Mandelbrot - The Classic Mandelbrot ensemble in Ada
--|   Package de l'algorithme de Mandelbrot classique
--|
--|   Copyright (C) 2020 Frederic Desnoes (frederic.desnoes@wanadoo.fr)
--|
--|
--| Filename         : $Source: /Draw_mandelbrot.ads$
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
 			Context : in out Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type);
