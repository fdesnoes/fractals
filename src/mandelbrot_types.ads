with Ada.Unchecked_Conversion;


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

Package mandelbrot_types is

type pix is record
	n: integer range 0..255;
	end record;
	
type pixs is
	array (positive range <>,positive range <>) of pix;
		
  X1 : constant float := -1.5; ---2.1;
  X2 : constant float := 0.6; --0.6;
  y1 : constant float := -1.2; ---1.2;
  y2 : constant float := 1.2; --1.2;
  zoom : constant float := 300.0; -- pour une distance de 1 sur le plan, on a 300 pixel sur l'image
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
  
  pixels : pixs(1 .. integer(image_x),1..integer(image_y));
  
 end mandelbrot_types;
